### World Cup 2026 Simulations
library(tidyverse)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multicore(workers = 8))
source('helpers.R')

n_sims <- 10000
set.seed(12345)
run_date <- max(Sys.Date(), as.Date('2026-06-11'))

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in ratings and schedule
df_ratings <- read_csv('predictions/ratings.csv', show_col_types = F)
schedule <-
  read_csv('data/schedule.csv', show_col_types = F) %>%
  mutate('date' = as.Date(date)) %>%
  mutate('team1_score' = ifelse(date > run_date, NA, team1_score),
         'team2_score' = ifelse(date > run_date, NA, team2_score)) %>%
  mutate('team1_score' = case_when(
    is.na(shootout_winner) ~ as.numeric(team1_score),
    shootout_winner == team1 ~ 0.1 + as.numeric(team1_score),
    shootout_winner == team2 ~ -0.1 + as.numeric(team1_score)
  ))

schedule <- adorn_xg(schedule)

### Simulate Group Stage (72 games)
df_group_stage <- filter(schedule, !is.na(group))

if(any(is.na(schedule$team1_score[!is.na(schedule$group)]))) {
  dfs_group_stage <- map(1:n_sims, ~df_group_stage)
  group_stage_results <-
    future_map(dfs_group_stage, sim_group_stage,
               .options = furrr_options(seed = 12921))

  ### Build R32 bracket from group stage results
  r32_brackets <-
    future_map(group_stage_results, ~build_knockout_bracket(.x$standings),
               .options = furrr_options(seed = 31121))
} else {
  gsr <- sim_group_stage(df_group_stage)
  group_stage_results <- map(1:n_sims, ~gsr)
  r32_brackets <- future_map(1:n_sims, ~filter(schedule, str_detect(ko_round, 'R32')))
}

### R32 (16 games)
r32_brackets <-
  future_map(r32_brackets, ~{
    schedule %>%
      filter(str_detect(ko_round, 'R32')) %>%
      mutate('team1' = ifelse(is.na(.$team1), .x$team1, .$team1),
             'team2' = ifelse(is.na(.$team2), .x$team2, .$team2)) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

r32_results <- future_map(r32_brackets, sim_ko_round)

### R16 (8 games) — pairs of consecutive R32 winners
r16_brackets <-
  future_map(r32_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>%
      filter(str_detect(ko_round, 'R16')) %>%
      mutate('team1' = map_chr(1:nrow(.), ~winners[2 * .x - 1]),
             'team2' = map_chr(1:nrow(.), ~winners[2 * .x])) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

r16_results <- future_map(r16_brackets, sim_ko_round)

### QF (4 games) — pairs of consecutive R16 winners
qf_brackets <-
  future_map(r16_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>%
      filter(str_detect(ko_round, 'QF')) %>%
      mutate('team1' = map_chr(1:nrow(.), ~winners[2 * .x - 1]),
             'team2' = map_chr(1:nrow(.), ~winners[2 * .x])) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

qf_results <- future_map(qf_brackets, sim_ko_round)

### SF (2 games) — pairs of consecutive QF winners
sf_brackets <-
  future_map(qf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>%
      filter(str_detect(ko_round, 'SF')) %>%
      mutate('team1' = winners[c(1, 3)],
             'team2' = winners[c(2, 4)]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

sf_results <- future_map(sf_brackets, sim_ko_round)

### Final
final_brackets <-
  future_map(sf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    schedule %>%
      filter(ko_round == 'FINAL') %>%
      mutate('team1' = winners[1],
             'team2' = winners[2]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

finals_results <- future_map(final_brackets, sim_ko_round)

### 3rd place match
third_brackets <-
  future_map(sf_results, ~{
    losers <- ifelse(.x$team1_score > .x$team2_score, .x$team2, .x$team1)
    schedule %>%
      filter(ko_round == '3RD') %>%
      mutate('team1' = losers[1],
             'team2' = losers[2]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  })

third_results <- future_map(third_brackets, sim_ko_round)

### Aggregate Results
r32_teams <-
  bind_rows(r32_results) %>%
  pivot_longer(c('team1', 'team2')) %>%
  pull(value)

r16_teams <-
  bind_rows(r16_results) %>%
  pivot_longer(c('team1', 'team2')) %>%
  pull(value)

qf_teams <-
  bind_rows(qf_results) %>%
  pivot_longer(c('team1', 'team2')) %>%
  pull(value)

sf_teams <-
  bind_rows(sf_results) %>%
  pivot_longer(c('team1', 'team2')) %>%
  pull(value)

final_teams <-
  bind_rows(finals_results) %>%
  pivot_longer(c('team1', 'team2')) %>%
  pull(value)

winners <-
  bind_rows(finals_results) %>%
  mutate('champ' = ifelse(team1_score > team2_score, team1, team2)) %>%
  pull(champ)

df_stats <-
  map_dfr(group_stage_results, ~.x$standings) %>%
  group_by(team, group) %>%
  summarise('mean_pts' = mean(points),
            'mean_gd' = mean(goal_diff),
            'r32' = mean(progress),
            'r16' = sum(team == r16_teams) / n_sims,
            'qf' = sum(team == qf_teams) / n_sims,
            'sf' = sum(team == sf_teams) / n_sims,
            'finals' = sum(team == final_teams) / n_sims,
            'champ' = sum(team == winners) / n_sims) %>%
  ungroup()

### Save Results
write_csv(df_stats, 'predictions/sim_results.csv')

if(!file.exists('predictions/history.csv')) {
  df_stats %>%
    mutate('date' = run_date) %>%
    write_csv('predictions/history.csv')
}
history <-
  read_csv('predictions/history.csv', show_col_types = F) %>%
  filter(date != run_date) %>%
  bind_rows(df_stats %>% mutate('date' = run_date)) %>%
  arrange(date)
write_csv(history, 'predictions/history.csv')

write_rds(map(group_stage_results, ~.x$standings), 'predictions/sim_rds/group_stage_results.rds')
write_rds(map(group_stage_results, ~.x$results), 'predictions/sim_rds/group_stage_game_results.rds')
write_rds(r32_results, 'predictions/sim_rds/r32_results.rds')
write_rds(r16_results, 'predictions/sim_rds/r16_results.rds')
write_rds(qf_results, 'predictions/sim_rds/qf_results.rds')
write_rds(sf_results, 'predictions/sim_rds/sf_results.rds')
write_rds(finals_results, 'predictions/sim_rds/finals_results.rds')
write_rds(third_results, 'predictions/sim_rds/third_results.rds')
