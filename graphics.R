library(tidyverse)
library(ggimage)
library(ggtext)
library(furrr)
library(gt)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multicore(workers = parallel::detectCores() - 1))
source('helpers.R')

history <-
  read_csv('predictions/history.csv', show_col_types = F) %>%
  mutate('logo' = paste0('flags/', team, '.png')) %>%
  mutate('eliminated' = (r32 == 0))

df_stats <-
  read_csv('predictions/sim_results.csv', show_col_types = F) %>%
  mutate('logo' = paste0('flags/', team, '.png'))

eliminated_teams <- history$team[history$date == max(history$date) & history$r32 == 0]
history_ko <- history %>% filter(!team %in% eliminated_teams)

history_plot <- function(data, y_col, y_label, subtitle_, ncol_ = 4) {
  data <- filter(data, !is.na(group))
  data_alive <- filter(data, !eliminated)
  data_elim <- filter(data, eliminated)
  p <- 
    ggplot(data, aes(x = date, y = .data[[y_col]])) +
    facet_wrap(~paste('Group', group), ncol = ncol_) +
    geom_line(aes(group = team), col = 'black', alpha = 0.4) +
    geom_image(data = data_alive, aes(image = logo), size = 0.07) 
  
  if(nrow(data_elim) > 0) {
    p <- 
      p + 
      geom_image(data = data_elim, aes(image = logo), image_fun = transparent, size = 0.07) 
  }
  
  p <- 
    
    p + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = 'Date', y = y_label, title = 'World Cup 2026', subtitle = subtitle_)
  return(p)
}

### Advancement probability over time (by group, 3×4 layout for 12 groups)
history_plot(history, 'r32', 'Chances of Reaching Round of 32', 'R32 Chances Over Time') 
ggsave('figures/r32.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'r16', 'Chances of Reaching Round of 16', 'R16 Chances Over Time') 
ggsave('figures/r16.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'qf', 'Chances of Reaching Quarterfinals', 'QF Chances Over Time') 
ggsave('figures/qf.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'sf', 'Chances of Reaching Semifinals', 'SF Chances Over Time') 
ggsave('figures/sf.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'finals', 'Chances of Reaching Finals', 'Finals Chances Over Time') 
ggsave('figures/finals.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'champ', 'Chances of Winning Tournament', 'Title Chances Over Time') 
ggsave('figures/champ.png', height = 14/1.2, width = 16/1.2)

### Elimination snapshot — stacked bar chart
df_elim <-
  df_stats %>%
  mutate('elim_Group' = 1 - r32,
         'elim_R32'   = r32  - r16,
         'elim_R16'   = r16  - qf,
         'elim_QF'    = qf   - sf,
         'elim_SF'    = sf   - finals,
         'elim_Final' = finals - champ,
         'elim_Champ' = champ) %>%
  mutate('expected_round' = elim_Group + 2 * elim_R32 + 3 * elim_R16 + 4 * elim_QF +
           5 * elim_SF + 6 * elim_Final + 7 * elim_Champ) %>%
  pivot_longer(contains('elim_'),
               names_to = 'elim_round',
               names_prefix = 'elim_',
               values_to = 'elim_prob') %>%
  mutate('elim_round' = factor(elim_round, levels = c('Group', 'R32', 'R16', 'QF', 'SF', 'Final', 'Champ'))) %>%
  mutate('team' = fct_reorder(team, desc(expected_round)))

df_flags <- df_elim %>% distinct(team, logo)

ggplot(df_elim, aes(x = team, y = elim_prob)) +
  geom_col(aes(fill = elim_round), position = position_fill(reverse = T)) +
  geom_image(data = df_flags, aes(x = team, y = -0.05, image = logo), size = 0.035) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.1, 0.02))) +
  coord_cartesian(clip = 'off') +
  labs(y = 'Probability of Elimination at Stage',
       x = '',
       title = 'FIFA World Cup 2026',
       subtitle = 'Elimination Snapshot',
       fill = 'Elimination Round') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom')

ggsave('figures/elim.png', height = 9/1.2, width = 20/1.2)

### R32 advancement by points + GD tile chart
gs_results <- read_rds('predictions/sim_rds/group_stage_results.rds')

df_tile <-
  bind_rows(gs_results) %>%
  filter(goal_diff >= -5, goal_diff <= 5) %>%
  filter(points <= 4) %>% 
  group_by(points, goal_diff) %>%
  summarise('r32' = mean(progress), 'n' = n()) %>%
  ungroup()

ggplot(df_tile, aes(x = points, y = goal_diff)) +
  geom_tile(aes(fill = r32), color = 'black') +
  geom_label(aes(label = paste0(sprintf('%0.1f', 100 * r32), '%'))) +
  scale_fill_distiller(palette = 'RdYlGn', direction = 1, labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  scale_y_continuous(breaks = seq(-5,5,1)) +
  labs(x = 'Points', y = 'Goal Difference',
       title = 'World Cup 2026',
       subtitle = 'Group Stage: Chances of Reaching Round of 32',
       fill = 'R32 Probability') + 
  theme(legend.position = 'bottom')

ggsave('figures/r32_tile.png', height = 9/1.2, width = 12/1.2)

### Most likely knockout matchups — gt table (5-round layout with spanners)
process_round <- function(results, round_name, top_n = 16) {
  bind_rows(results) %>%
    select(team1, team2) %>%
    mutate(
      'team_a' = if_else(team1 < team2, team1, team2),
      'team_b' = if_else(team1 < team2, team2, team1)
    ) %>%
    group_by(team_a, team_b) %>%
    summarise('prob' = n() / n_sims, .groups = 'drop') %>%
    arrange(desc(prob)) %>%
    slice(1:top_n) %>%
    mutate(
      'round' = round_name,
      'rank' = row_number(),
      'flag_a' = file.path(getwd(), paste0('flags/', team_a, '.png')),
      'flag_b' = file.path(getwd(), paste0('flags/', team_b, '.png'))
    )
}

r32_results    <- read_rds('predictions/sim_rds/r32_results.rds')
r16_results    <- read_rds('predictions/sim_rds/r16_results.rds')
qf_results     <- read_rds('predictions/sim_rds/qf_results.rds')
sf_results     <- read_rds('predictions/sim_rds/sf_results.rds')
finals_results <- read_rds('predictions/sim_rds/finals_results.rds')
n_sims <- length(r32_results)

df_matchups <-
  bind_rows(
    process_round(r32_results,    'r32'),
    process_round(r16_results,    'r16'),
    process_round(qf_results,     'qf'),
    process_round(sf_results,     'sf'),
    process_round(finals_results, 'fin')
  ) %>%
  select(rank, round, flag_a, team_a, flag_b, team_b, prob) %>%
  pivot_wider(id_cols = rank, names_from = round,
              values_from = c(flag_a, team_a, flag_b, team_b, prob),
              names_glue = '{round}_{.value}') %>%
  select(r32_flag_a, r32_team_a, r32_flag_b, r32_team_b, r32_prob,
         r16_flag_a, r16_team_a, r16_flag_b, r16_team_b, r16_prob,
         qf_flag_a,  qf_team_a,  qf_flag_b,  qf_team_b,  qf_prob,
         sf_flag_a,  sf_team_a,  sf_flag_b,  sf_team_b,  sf_prob,
         fin_flag_a, fin_team_a, fin_flag_b, fin_team_b, fin_prob)

divider_cols <- c('r16_flag_a', 'qf_flag_a', 'sf_flag_a', 'fin_flag_a')

df_matchups %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = contains('flag')),
    fn = function(x) map(x, ~gt::local_image(filename = .x, height = 20))
  ) %>%
  fmt_percent(columns = contains('prob'), decimals = 1) %>%
  tab_spanner(label = 'Round of 32',   columns = starts_with('r32')) %>%
  tab_spanner(label = 'Round of 16',   columns = starts_with('r16')) %>%
  tab_spanner(label = 'Quarterfinals', columns = starts_with('qf')) %>%
  tab_spanner(label = 'Semifinals',    columns = starts_with('sf')) %>%
  tab_spanner(label = 'Finals',        columns = starts_with('fin')) %>%
  cols_label(
    r32_flag_a = '', r32_team_a = 'Team', r32_flag_b = '', r32_team_b = 'Team', r32_prob = 'Prob',
    r16_flag_a = '', r16_team_a = 'Team', r16_flag_b = '', r16_team_b = 'Team', r16_prob = 'Prob',
    qf_flag_a  = '', qf_team_a  = 'Team', qf_flag_b  = '', qf_team_b  = 'Team', qf_prob  = 'Prob',
    sf_flag_a  = '', sf_team_a  = 'Team', sf_flag_b  = '', sf_team_b  = 'Team', sf_prob  = 'Prob',
    fin_flag_a = '', fin_team_a = 'Team', fin_flag_b = '', fin_team_b = 'Team', fin_prob = 'Prob'
  ) %>%
  tab_header(title = 'FIFA World Cup 2026', subtitle = 'Most Likely Knockout Matchups') %>%
  cols_align(align = 'center') %>%
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = list(cells_title(), cells_column_spanners(), cells_column_labels())
  ) %>%
  tab_style(
    style = cell_borders(sides = 'left', color = 'black', weight = px(2)),
    locations = list(
      cells_body(columns = all_of(divider_cols)),
      cells_column_labels(columns = all_of(divider_cols))
    )
  ) %>%
  gtsave('figures/matchups_table.png', vwidth = 3000)
