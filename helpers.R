### Dictionary of Team Codes
team_codes <- function(df) {
  teams <- sort(unique(c(df$home_team, df$away_team)))
  codes <- 1:length(teams)
  names(codes) <- teams
  return(codes)
}

adorn_xg <- function(df) {
  na_mask <- is.na(df$team1)
  alpha_1 <- alpha_vec[df$team1]
  delta_1 <- delta_vec[df$team1]
  alpha_2 <- alpha_vec[df$team2]
  delta_2 <- delta_vec[df$team2]
  loc_1 <- case_when(df$team1 == df$location ~ home_field,
                     df$team2 == df$location ~ 0,
                     TRUE ~ neutral_field)
  loc_2 <- case_when(df$team1 == df$location ~ 0,
                     df$team2 == df$location ~ home_field,
                     TRUE ~ neutral_field)
  df$lambda_1 <- ifelse(na_mask, NA_real_, exp(mu + alpha_1 + delta_2 + loc_1))
  df$lambda_2 <- ifelse(na_mask, NA_real_, exp(mu + alpha_2 + delta_1 + loc_2))
  df
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

  for (g in LETTERS[1:12]) {
    group_standings <- filter(standings, group == g)
    group_results   <- filter(df_results, group == g)
    group_standings <- resolve_group_ties(group_standings, group_results)
    df_final        <- bind_rows(df_final, group_standings)
  }
  return(df_final)
}

resolve_group_ties <- function(standings, results) {
  h2h_stats <- function(teams) {
    results %>%
      filter(team %in% teams, opp %in% teams) %>%
      group_by(team) %>%
      summarise(
        h2h_pts = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
        h2h_gd  = sum(team_score - opp_score),
        h2h_gs  = sum(team_score),
        .groups  = 'drop'
      )
  }

  pts_values <-
    standings %>%
    group_by(points) %>%
    filter(n() > 1) %>%
    pull(points) %>%
    unique()

  for (pts in pts_values) {
    tied_teams <- standings$team[standings$points == pts]
    base_place <- min(standings$place[standings$team %in% tied_teams])

    h2h <- h2h_stats(tied_teams)

    sub <-
      standings %>%
      filter(team %in% tied_teams) %>%
      left_join(h2h, by = 'team') %>%
      arrange(desc(h2h_pts), desc(h2h_gd), desc(h2h_gs),
              desc(goal_diff), desc(goals_scored))

    new_places <- base_place + seq(0, length(tied_teams) - 1)
    standings$place[match(sub$team, standings$team)] <- new_places
  }

  standings %>% arrange(place)
}

### Build R32 bracket.
### Requires df_combinations pre-loaded in calling env (data/third_place_combinations.csv).
build_knockout_bracket <- function(group_stage_results) {
  team_map <- setNames(group_stage_results$team,
                       paste0(group_stage_results$group, group_stage_results$place))
  get_team <- function(grp, pos) team_map[[paste0(grp, pos)]]

  third_qualifiers <-
    group_stage_results %>%
    filter(progress, place == 3) %>%
    arrange(desc(points), desc(goal_diff), desc(goals_scored))

  third_map <- setNames(third_qualifiers$team, third_qualifiers$group)
  qualifying_groups <- sort(third_qualifiers$group)
  groups_key <- paste(qualifying_groups, collapse = '')

  combo <- filter(df_combinations, groups == groups_key)
  get_3rd <- function(slot_col) third_map[[pull(combo, slot_col)]]

  tibble(
    'team1' = c(get_team('A', 1), get_team('B', 1), get_team('A', 2), get_team('F', 1),
                get_team('K', 2), get_team('H', 1), get_team('G', 1), get_team('I', 1),
                get_team('C', 1), get_team('E', 2), get_team('D', 1), get_team('E', 1),
                get_team('J', 1), get_team('D', 2), get_team('K', 1), get_team('L', 1)),

    'team2' = c(get_3rd('m74'), get_3rd('m77'), get_team('B', 2), get_team('C', 2),
                get_team('L', 2), get_team('J', 2), get_3rd('m81'), get_3rd('m82'),
                get_team('F', 2), get_team('I', 2), get_3rd('m79'), get_3rd('m80'),
                get_team('H', 2), get_team('G', 2), get_3rd('m85'), get_3rd('m87'))
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
