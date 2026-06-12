### World Cup 2026 Game Predictions
library(tidyverse)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multicore(workers = parallel::detectCores() - 1))
source('helpers.R')
library(gt)
library(gtExtras)
dir.create('figures/matchweek_preds', showWarnings = FALSE)

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

df_ratings <- read_csv('predictions/ratings.csv', show_col_types = F)
schedule <-
  read_csv('data/schedule.csv', show_col_types = F) %>%
  mutate('date' = as.Date(date))

preds <- adorn_xg(schedule)
preds <- bind_cols(preds, map2_dfr(preds$lambda_1, preds$lambda_2, ~as_tibble(match_probs(.x, .y))))

### Preserve historical predictions for played games
if(file.exists('predictions/game_predictions.csv')) {
  preds_old <-
    read_csv('predictions/game_predictions.csv', show_col_types = F) %>%
    filter(date < Sys.Date()) %>%
    select(lambda_1, lambda_2, win, draw, loss)
  if(nrow(preds_old) > 0) {
    preds[preds$date < Sys.Date(), c('lambda_1', 'lambda_2', 'win', 'draw', 'loss')] <- preds_old
  }
}

preds <- bind_cols(preds, map2_dfr(preds$lambda_1, preds$lambda_2, ~as_tibble(match_probs_ko(.x, .y))))
write_csv(preds, 'predictions/game_predictions.csv')

### Helper to build a gt matchweek or KO round table
make_game_table <- function(df, title_str, subtitle_str, group_stage = T) {
  df %>%
    gt() %>%
    cols_align('center') %>%
    fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
    { if(group_stage)
        fmt_percent(., columns = c(win, loss, draw), decimals = 0, sep_mark = '')
      else
        fmt_percent(., columns = c(win_ko, loss_ko), decimals = 0, sep_mark = '') } %>%
    data_color(
      columns = c(lambda_1, lambda_2),
      fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                   domain = range(preds[, c('lambda_1', 'lambda_2')], na.rm = T))
    ) %>%
    { if(group_stage)
        data_color(., columns = c(win, loss, draw),
                   fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1)))
      else
        data_color(., columns = c(win_ko, loss_ko),
                   fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) } %>%
    tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
              locations = list(cells_column_labels(columns = gt::everything()))) %>%
    tab_style(style = list(cell_borders(sides = "right", color = "black", weight = px(3))),
              locations = list(cells_body(columns = c(lambda_2)))) %>%
    tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
    { if(group_stage)
        tab_spanner(., label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss'))
      else
        tab_spanner(., label = 'Match Outcome Probabilities', columns = c('win_ko', 'loss_ko')) } %>%
    text_transform(locations = cells_body(columns = "logo1"),
                   fn = function(x) map_chr(x, ~local_image(filename = as.character(.x), height = 30))) %>%
    text_transform(locations = cells_body(columns = "logo2"),
                   fn = function(x) map_chr(x, ~local_image(filename = as.character(.x), height = 30))) %>%
    { if(group_stage)
        cols_label(., date = 'Date', team1 = 'Team 1', logo1 = '', team2 = 'Team 2', logo2 = '',
                   group = 'Group', lambda_1 = 'Team 1', lambda_2 = 'Team 2',
                   win = 'Team 1', draw = 'Draw', loss = 'Team 2')
      else
        cols_label(., date = 'Date', team1 = 'Team 1', logo1 = '', team2 = 'Team 2', logo2 = '',
                   lambda_1 = 'Team 1', lambda_2 = 'Team 2',
                   win_ko = 'Team 1', loss_ko = 'Team 2') } %>%
    tab_source_note("Luke Benz (@recspecs730)") %>%
    tab_source_note("Data: github.com/martj42/international_results | Country Images: Flaticon.com") %>%
    tab_header(title = md(paste0('**', title_str, '**')),
               subtitle = md(paste0('**', subtitle_str, '**'))) %>%
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold')
}

### Side-by-side matchweek table (Groups A-F left, G-L right)
make_mw_table <- function(df, subtitle_str) {
  df_sorted <-
    df %>%
    arrange(group, date)

  n_per_group <- nrow(df_sorted) / 12

  df1 <-
    df_sorted %>%
    filter(group <= 'F')
  names(df1) <- paste0(names(df1), '_1')

  df2 <-
    df_sorted %>%
    filter(group > 'F')
  names(df2) <- paste0(names(df2), '_2')

  group_borders <- seq(n_per_group, nrow(df1) - n_per_group, by = n_per_group)

  bind_cols(df1, df2) %>%
    gt() %>%
    cols_align('center') %>%
    fmt_number(columns = c(lambda_1_1, lambda_2_1, lambda_1_2, lambda_2_2), decimals = 2, sep_mark = '') %>%
    fmt_percent(columns = c(win_1, draw_1, loss_1, win_2, draw_2, loss_2), decimals = 0, sep_mark = '') %>%
    data_color(
      columns = c(lambda_1_1, lambda_2_1, lambda_1_2, lambda_2_2),
      fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                               domain = range(preds[, c('lambda_1', 'lambda_2')], na.rm = T))
    ) %>%
    data_color(
      columns = c(win_1, draw_1, loss_1, win_2, draw_2, loss_2),
      fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "bottom", color = "black", weight = px(3))
      ),
      locations = list(
        cells_column_labels(columns = gt::everything())
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "bottom", color = "black", weight = px(3))
      ),
      locations = list(
        cells_body(rows = group_borders)
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "right", color = "black", weight = px(3))
      ),
      locations = list(
        cells_body(columns = c(group_1, lambda_2_1, loss_1, group_2, lambda_2_2))
      )
    ) %>%
    tab_spanner(label = 'Expected Goals', columns = c('lambda_1_1', 'lambda_2_1'), id = '1') %>%
    tab_spanner(label = 'Expected Goals', columns = c('lambda_1_2', 'lambda_2_2'), id = '2') %>%
    tab_spanner(label = 'Match Outcome Probabilities', columns = c('win_1', 'draw_1', 'loss_1'), id = '3') %>%
    tab_spanner(label = 'Match Outcome Probabilities', columns = c('win_2', 'draw_2', 'loss_2'), id = '4') %>%
    text_transform(
      locations = cells_body(columns = c(logo1_1, logo1_2, logo2_1, logo2_2)),
      fn = function(x) map_chr(x, ~{
        local_image(filename = as.character(.x), height = 30)
      })
    ) %>%
    cols_label(
      date_1 = 'Date', team1_1 = 'Team 1', logo1_1 = '', team2_1 = 'Team 2', logo2_1 = '',
      group_1 = 'Group', lambda_1_1 = 'Team 1', lambda_2_1 = 'Team 2',
      win_1 = 'Team 1', draw_1 = 'Draw', loss_1 = 'Team 2',
      date_2 = 'Date', team1_2 = 'Team 1', logo1_2 = '', team2_2 = 'Team 2', logo2_2 = '',
      group_2 = 'Group', lambda_1_2 = 'Team 1', lambda_2_2 = 'Team 2',
      win_2 = 'Team 1', draw_2 = 'Draw', loss_2 = 'Team 2'
    ) %>%
    tab_source_note("Luke Benz (@recspecs730)") %>%
    tab_source_note("Data: github.com/martj42/international_results | Country Images: Flaticon.com") %>%
    tab_header(
      title = html(paste0("<img src='", file.path(getwd(), 'flags/fifa_logo.jpg'), "' height='160'>")),
      subtitle = md(paste0('**', subtitle_str, '**'))
    ) %>%
    tab_options(
      column_labels.font.size = 20,
      heading.subtitle.font.size = 30,
      heading.subtitle.font.weight = 'bold',
      column_labels.font.weight = 'bold'
    )
}

### Group stage matchweeks (24 games each, three matchdays)
df_preds <-
  preds %>%
  filter(!is.na(group)) %>%
  arrange(date) %>%
  mutate('logo1' = paste0('flags/', team1, '.png'),
         'logo2' = paste0('flags/', team2, '.png')) %>%
  select(date, team1, logo1, team2, logo2, group, lambda_1, lambda_2, win, draw, loss)

### Matchweek 1: June 11-17 (first game of each group)
### Matchweek 2: June 18-22 (second game of each group)
### Matchweek 3: June 24-27 (third game, simultaneous within each group)
mw1 <- df_preds %>% filter(date >= as.Date('2026-06-11'), date <= as.Date('2026-06-17'))
mw2 <- df_preds %>% filter(date >= as.Date('2026-06-18'), date <= as.Date('2026-06-23'))
mw3 <- df_preds %>% filter(date >= as.Date('2026-06-24'), date <= as.Date('2026-06-27'))

if(max(mw1$date) > Sys.Date()) {
  gtsave_extra(make_mw_table(mw1, 'Matchweek 1'), 'figures/matchweek_preds/matchweek1.png', vwidth = 1800)
}
if(max(mw2$date) > Sys.Date()) {
  gtsave_extra(make_mw_table(mw2, 'Matchweek 2'), 'figures/matchweek_preds/matchweek2.png', vwidth = 1800)
}
if(max(mw3$date) > Sys.Date()) {
  gtsave_extra(make_mw_table(mw3, 'Matchweek 3'), 'figures/matchweek_preds/matchweek3.png', vwidth = 1800)
}

### Knockout round tables
df_preds_ko <-
  preds %>%
  filter(!is.na(ko_round), !is.na(team1)) %>%
  arrange(date) %>%
  mutate('logo1' = paste0('flags/', team1, '.png'),
         'logo2' = paste0('flags/', team2, '.png')) %>%
  select(date, team1, logo1, team2, logo2, ko_round, lambda_1, lambda_2, win_ko, loss_ko)

for(rnd in c('R32', 'R16', 'QF', 'SF')) {
  df_rnd <- df_preds_ko %>% filter(str_detect(ko_round, rnd)) %>% select(-ko_round)
  if(nrow(df_rnd) > 0) {
    subtitles <- list(R32 = 'Round of 32', R16 = 'Round of 16', QF = 'Quarterfinals', SF = 'Semifinals')
    gtsave_extra(make_game_table(df_rnd, 'World Cup 2026 Game Predictions', subtitles[[rnd]], group_stage = F),
                 paste0('figures/matchweek_preds/', tolower(rnd), '_preds.png'))
  }
}
