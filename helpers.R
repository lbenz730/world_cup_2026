### Dictionary of Team Codes
team_codes <- function(df) {
  teams <- sort(unique(c(df$home_team, df$away_team)))
  codes <- 1:length(teams)
  names(codes) <- teams
  return(codes)
}

### Generate Goal Expectations Given Teams + Location
goal_expectations <- function(team_1, team_2, location) {
  if(is.na(team_1)) {
    return(list('lambda_1' = NA,
                'lambda_2' = NA))
  }
  alpha_1 <- filter(df_ratings, team == team_1) %>% pull(alpha)
  delta_1 <- filter(df_ratings, team == team_1) %>% pull(delta)
  alpha_2 <- filter(df_ratings, team == team_2) %>% pull(alpha)
  delta_2 <- filter(df_ratings, team == team_2) %>% pull(delta)

  loc_1 <- case_when(team_1 == location ~ home_field,
                     team_2 == location ~ 0,
                     T ~ neutral_field)
  loc_2 <- case_when(team_1 == location ~ 0,
                     team_2 == location ~ home_field,
                     T ~ neutral_field)

  lambda_1 <- exp(mu + alpha_1 + delta_2 + loc_1)
  lambda_2 <- exp(mu + alpha_2 + delta_1 + loc_2)

  return(list('lambda_1' = lambda_1,
              'lambda_2' = lambda_2))
}

adorn_xg <- function(df) {
  df_xg <- future_pmap_dfr(list('team_1' = df$team1,
                                 'team_2' = df$team2,
                                 'location' = df$location),
                            ~{as_tibble(goal_expectations(..1, ..2, ..3))})
  return(bind_cols(df, df_xg))
}

### Simulate Group Stage (12 groups A-L, top 2 + best 8 third-place advance)
sim_group_stage <- function(df_group_stage) {
  ix1 <- is.na(df_group_stage$team1_score)
  ix2 <- is.na(df_group_stage$team2_score)
  df_group_stage$team1_score[ix1] <- rpois(sum(ix1), lambda = df_group_stage$lambda_1[ix1])
  df_group_stage$team2_score[ix2] <- rpois(sum(ix2), lambda = df_group_stage$lambda_2[ix2])

  df_results <-
    bind_rows(
      select(df_group_stage, 'team' = team1, 'opp' = team2, 'team_score' = team1_score, 'opp_score' = team2_score, group),
      select(df_group_stage, 'team' = team2, 'opp' = team1, 'team_score' = team2_score, 'opp_score' = team1_score, group)
    )

  standings <-
    df_results %>%
    group_by(group, team) %>%
    summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
              'goal_diff' = sum(team_score - opp_score),
              'goals_scored' = sum(team_score),
              'goals_allowed' = sum(opp_score)) %>%
    ungroup() %>%
    arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>%
    group_by(group) %>%
    mutate('place' = 1:n()) %>%
    ungroup()

  standings <- group_tiebreak(standings, df_results) %>%
    arrange(group, place)

  ### Top 8 third-place teams advance
  third_place <-
    standings %>%
    filter(place == 3) %>%
    arrange(desc(points), desc(goal_diff), desc(goals_scored)) %>%
    slice(1:8)

  standings <-
    standings %>%
    left_join(select(third_place, team) %>% mutate('progress_3rd' = T), by = 'team') %>%
    mutate('progress' = case_when(place < 3 ~ T,
                                  place == 3 & !is.na(progress_3rd) ~ T,
                                  T ~ F)) %>%
    select(-progress_3rd)

  return(list('standings' = standings,
              'results' = df_results))
}

group_tiebreak <- function(standings, df_results) {
  options(dplyr.summarise.inform = F)
  df_final <- NULL

  for(g in LETTERS[1:12]) {
    group_standings <- filter(standings, group == g)
    group_results <- filter(df_results, group == g)

    if(group_standings$points[1] != group_standings$points[4]) {
      if(group_standings$points[1] == group_standings$points[3] &
         group_standings$goal_diff[1] == group_standings$goal_diff[3] &
         group_standings$goals_scored[1] == group_standings$goals_scored[3]) {
        tiebreak_order <-
          group_results %>%
          inner_join(group_standings %>% select(team, place), by = 'team') %>%
          filter(team %in% group_standings$team[1:3], opp %in% group_standings$team[1:3]) %>%
          group_by(group, team, place) %>%
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>%
          ungroup() %>%
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>%
          group_by(group) %>%
          mutate('place' = 1:n()) %>%
          ungroup() %>%
          pull(team)
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:3] <- ix
      }
      if((group_standings$points[1] != group_standings$points[3]) &
         (group_standings$points[1] == group_standings$points[2]) &
         group_standings$goal_diff[1] == group_standings$goal_diff[2] &
         group_standings$goals_scored[1] == group_standings$goals_scored[2]) {
        tiebreak_order <-
          group_results %>%
          inner_join(group_standings %>% select(team, place), by = 'team') %>%
          filter(team %in% group_standings$team[1:2], opp %in% group_standings$team[1:2]) %>%
          group_by(group, team, place) %>%
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>%
          ungroup() %>%
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>%
          group_by(group) %>%
          mutate('place' = 1:n()) %>%
          ungroup() %>%
          pull(team)
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:2] <- ix
      }
      if(group_standings$points[2] == group_standings$points[4] &
         group_standings$goal_diff[2] == group_standings$goal_diff[4] &
         group_standings$goals_scored[2] == group_standings$goals_scored[4]) {
        tiebreak_order <-
          group_results %>%
          filter(team %in% group_standings$team[2:4], opp %in% group_standings$team[2:4]) %>%
          group_by(group, team) %>%
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>%
          ungroup() %>%
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>%
          group_by(group) %>%
          mutate('place' = 1:n()) %>%
          ungroup() %>%
          pull(team)
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:4] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[1] != group_standings$points[3]) &
         (group_standings$points[2] == group_standings$points[3]) &
         group_standings$goal_diff[2] == group_standings$goal_diff[3] &
         group_standings$goals_scored[2] == group_standings$goals_scored[3]) {
        tiebreak_order <-
          group_results %>%
          inner_join(group_standings %>% select(team, place), by = 'team') %>%
          filter(team %in% group_standings$team[2:3], opp %in% group_standings$team[2:3]) %>%
          group_by(group, team, place) %>%
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>%
          ungroup() %>%
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>%
          group_by(group) %>%
          mutate('place' = 1:n()) %>%
          ungroup() %>%
          pull(team)
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:3] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[3] == group_standings$points[4]) &
         group_standings$goal_diff[3] == group_standings$goal_diff[4] &
         group_standings$goals_scored[3] == group_standings$goals_scored[4]) {
        tiebreak_order <-
          group_results %>%
          inner_join(group_standings %>% select(team, place), by = 'team') %>%
          filter(team %in% group_standings$team[3:4], opp %in% group_standings$team[3:4]) %>%
          group_by(group, team, place) %>%
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>%
          ungroup() %>%
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>%
          group_by(group) %>%
          mutate('place' = 1:n()) %>%
          ungroup() %>%
          pull(team)
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[3:4] <- ix
      }
    }
    df_final <- bind_rows(df_final, group_standings)
  }
  return(df_final)
}

### Build R32 bracket.
### Requires data/third_place_combinations.csv (run data/scrape_combinations.R to generate).
### R32 match order (matches ko_round "R32 1" through "R32 16"):
###   R32 1:  2A vs 2B       R32 2:  1E vs 3rd(m74)   R32 3:  1F vs 2C
###   R32 4:  1C vs 2F       R32 5:  1I vs 3rd(m77)   R32 6:  2E vs 2I
###   R32 7:  1A vs 3rd(m79) R32 8:  1L vs 3rd(m80)   R32 9:  1D vs 3rd(m81)
###   R32 10: 1G vs 3rd(m82) R32 11: 2K vs 2L         R32 12: 1H vs 2J
###   R32 13: 1B vs 3rd(m85) R32 14: 1J vs 2H         R32 15: 1K vs 3rd(m87)
###   R32 16: 2D vs 2G
build_knockout_bracket <- function(group_stage_results) {
  get_team <- function(grp, pos) {
    filter(group_stage_results, group == grp, place == pos) %>% pull(team)
  }

  ### Third-place teams by group
  third_qualifiers <-
    group_stage_results %>%
    filter(progress, place == 3) %>%
    arrange(desc(points), desc(goal_diff), desc(goals_scored))

  qualifying_groups <- sort(third_qualifiers$group)
  groups_key <- paste(qualifying_groups, collapse = '')

  df_combinations <- read_csv('data/third_place_combinations.csv', show_col_types = F)
  combo <- filter(df_combinations, groups == groups_key)

  get_3rd <- function(slot_col) {
    grp <- pull(combo, slot_col)
    filter(third_qualifiers, group == grp) %>% pull(team)
  }

  tibble(
    'team1' = c(get_team('A', 2), get_team('E', 1), get_team('F', 1), get_team('C', 1),
                get_team('I', 1), get_team('E', 2), get_team('A', 1), get_team('L', 1),
                get_team('D', 1), get_team('G', 1), get_team('K', 2), get_team('H', 1),
                get_team('B', 1), get_team('J', 1), get_team('K', 1), get_team('D', 2)),
    'team2' = c(get_team('B', 2), get_3rd('m74'), get_team('C', 2), get_team('F', 2),
                get_3rd('m77'), get_team('I', 2), get_3rd('m79'), get_3rd('m80'),
                get_3rd('m81'), get_3rd('m82'), get_team('L', 2), get_team('J', 2),
                get_3rd('m85'), get_team('H', 2), get_3rd('m87'), get_team('G', 2))
  )
}

### Simulate KO Round Games
sim_ko_round <- function(df) {
  lambdas_1 <- df$lambda_1[is.na(df$team1_score)]
  lambdas_2 <- df$lambda_2[is.na(df$team2_score)]

  n <- length(lambdas_1)
  if(n > 0) {
    goals_1 <- rpois(n, lambdas_1)
    goals_2 <- rpois(n, lambdas_2)

    tie_ix <- goals_1 == goals_2
    if(sum(tie_ix) > 0) {
      goals_1[tie_ix] <- goals_1[tie_ix] + rpois(sum(tie_ix), lambdas_1[tie_ix]/3)
      goals_2[tie_ix] <- goals_2[tie_ix] + rpois(sum(tie_ix), lambdas_2[tie_ix]/3)
      tie_ix <- goals_1 == goals_2
      if(sum(tie_ix) > 0) {
        goals_1[tie_ix] <- goals_1[tie_ix] + sample(c(0.1, -0.1), size = sum(tie_ix), replace = T)
      }
    }
    df$team1_score[is.na(df$team1_score)] <- goals_1
    df$team2_score[is.na(df$team2_score)] <- goals_2
  }
  return(df)
}

### W/D/L probabilities given expected goals
match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(list('win' = win_prob, 'draw' = tie_prob, 'loss' = loss_prob))
}

match_probs_ko <- function(lambda_1, lambda_2) {
  regulation <- match_probs(lambda_1, lambda_2)
  extra_time <- match_probs(lambda_1/3, lambda_2/3)
  win <- regulation$win + regulation$draw * extra_time$win + 0.5 * regulation$draw * extra_time$draw
  loss <- regulation$loss + regulation$draw * extra_time$loss + 0.5 * regulation$draw * extra_time$draw
  return(list('win_ko' = win, 'loss_ko' = loss))
}

### Custom ggplot theme
theme_set(theme_bw() +
            theme(plot.title = element_text(size = 24, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  plot.subtitle = element_text(size = 20, hjust = 0.5),
                  strip.text = element_text(size = 14, hjust = 0.5),
                  legend.position = "none")
)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.2*a", channel = "alpha")
}
