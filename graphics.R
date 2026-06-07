library(tidyverse)
library(ggimage)
library(ggtext)
library(furrr)
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
  data_alive <- filter(data, !eliminated)
  data_elim <- filter(data, eliminated)
  ggplot(data, aes(x = date, y = .data[[y_col]])) +
    facet_wrap(~paste('Group', group), ncol = ncol_) +
    geom_line(aes(group = team), col = 'black', alpha = 0.4) +
    geom_image(data = data_alive, aes(image = logo), size = 0.07) +
    geom_image(data = data_elim, aes(image = logo), image_fun = transparent, size = 0.07) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = 'Date', y = y_label, title = 'World Cup 2026', subtitle = subtitle_)
}

### Advancement probability over time (by group, 3×4 layout for 12 groups)
history_plot(history, 'r32', 'Chances of Reaching Round of 32', 'R32 Chances Over Time') +
  ggsave('figures/r32.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'r16', 'Chances of Reaching Round of 16', 'R16 Chances Over Time') +
  ggsave('figures/r16.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'qf', 'Chances of Reaching Quarterfinals', 'QF Chances Over Time') +
  ggsave('figures/qf.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'sf', 'Chances of Reaching Semifinals', 'SF Chances Over Time') +
  ggsave('figures/sf.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'finals', 'Chances of Reaching Finals', 'Finals Chances Over Time') +
  ggsave('figures/finals.png', height = 14/1.2, width = 16/1.2)

history_plot(history, 'champ', 'Chances of Winning Tournament', 'Title Chances Over Time') +
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

labels <- paste0("<img src ='", unique(df_elim %>% arrange(-expected_round) %>% pull(logo)), "', width = '15'/>")

ggplot(df_elim, aes(x = team, y = elim_prob)) +
  geom_col(aes(fill = elim_round), position = position_fill(reverse = T)) +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Probability of Elimination at Stage',
       x = '',
       title = 'FIFA World Cup 2026',
       subtitle = 'Elimination Snapshot',
       fill = 'Elimination Round') +
  theme(axis.text.x = ggtext::element_markdown(size = 7),
        legend.position = 'bottom')

ggsave('figures/elim.png', height = 9/1.2, width = 20/1.2)
