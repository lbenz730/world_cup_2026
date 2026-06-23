### World Cup 2026 Simulations
library(tidyverse)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multicore(workers = 8))
source('helpers.R')

n_sims <- 10000
set.seed(12345)
run_date <- max(Sys.Date() - (as.integer(format(Sys.time(), '%H')) < 12), as.Date('2026-06-10')) 



run_date <- as.Date(run_date)

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in ratings and schedule
df_ratings <- read_csv('predictions/ratings.csv', show_col_types = F)
alpha_vec <- setNames(df_ratings$alpha, df_ratings$team)
delta_vec <- setNames(df_ratings$delta, df_ratings$team)
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

df_combinations <- read_csv('data/third_place_combinations.csv', show_col_types = F)

sched_r32 <- filter(schedule, str_detect(ko_round, 'R32'))
sched_r16 <- filter(schedule, str_detect(ko_round, 'R16'))
sched_qf <- filter(schedule, str_detect(ko_round, 'QF'))
sched_sf <- filter(schedule, str_detect(ko_round, 'SF'))
sched_final <- filter(schedule, ko_round == 'FINAL')
sched_3rd <- filter(schedule, ko_round == '3RD')

### Simulate Group Stage (72 games)
cat('Simming Group Stage\n')
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
  r32_brackets <- future_map(1:n_sims, ~sched_r32, .options = furrr_options(seed = 8136))
}

### R32 (16 games)
cat('Simming R32\n')
r32_brackets <-
  future_map(r32_brackets, ~{
    sched_r32 %>%
      mutate('team1' = ifelse(is.na(.$team1), .x$team1, .$team1),
             'team2' = ifelse(is.na(.$team2), .x$team2, .$team2)) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8137))

r32_results <- future_map(r32_brackets, sim_ko_round, .options = furrr_options(seed = 8138))

### R16 (8 games) — bracket-correct pairings; order M89,M90,M95,M96,M93,M94,M91,M92
cat('Simming R16\n')
r16_t1 <- c(12, 3, 13, 2, 5, 11, 9, 1)
r16_t2 <- c(8, 4, 14, 15, 6, 7, 10, 16)

r16_brackets <-
  future_map(r32_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    sched_r16 %>%
      mutate('team1' = winners[r16_t1],
             'team2' = winners[r16_t2]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8139))

r16_results <- future_map(r16_brackets, sim_ko_round, .options = furrr_options(seed = 8140))

### QF (4 games) — consecutive R16 winner pairs give M97,M100,M98,M99 → correct SF halves
cat('Simming QF\n')
qf_brackets <-
  future_map(r16_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    sched_qf %>%
      mutate('team1' = winners[c(1, 3, 5, 7)],
             'team2' = winners[c(2, 4, 6, 8)]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8141))

qf_results <- future_map(qf_brackets, sim_ko_round, .options = furrr_options(seed = 8142))

### SF (2 games) — W(QF1) vs W(QF2) = SF M101; W(QF3) vs W(QF4) = SF M102
cat('Simming SF\n')
sf_brackets <-
  future_map(qf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    sched_sf %>%
      mutate('team1' = winners[c(1, 3)],
             'team2' = winners[c(2, 4)]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8143))

sf_results <- future_map(sf_brackets, sim_ko_round, .options = furrr_options(seed = 8144))

### Final
cat('Simming Final\n')
final_brackets <-
  future_map(sf_results, ~{
    winners <- ifelse(.x$team1_score > .x$team2_score, .x$team1, .x$team2)
    sched_final %>%
      mutate('team1' = winners[1],
             'team2' = winners[2]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8145))

finals_results <- future_map(final_brackets, sim_ko_round, .options = furrr_options(seed = 8146))

### 3rd place match
cat('Simming 3rd Place Match\n')
third_brackets <-
  future_map(sf_results, ~{
    losers <- ifelse(.x$team1_score > .x$team2_score, .x$team2, .x$team1)
    sched_3rd %>%
      mutate('team1' = losers[1],
             'team2' = losers[2]) %>%
      select(-lambda_1, -lambda_2) %>%
      adorn_xg(.)
  }, .options = furrr_options(seed = 8147))

third_results <- future_map(third_brackets, sim_ko_round, .options = furrr_options(seed = 8148))

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
