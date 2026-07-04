library(tidyverse)
library(googlesheets4)
library(gt)
source('helpers.R')

gs4_auth(email = 'lukesbenz@gmail.com')

### ── Read all forecaster probabilities ──────────────────────────────────────
ss_url <- "https://docs.google.com/spreadsheets/d/1_UKsDiPET4ifTlTkUeIMqPLeKrGEOkgsqAPUg9uT-sc/edit"
meta <- gs4_get(ss_url)
sheet_name <- 'All Entries'
df_raw <- read_sheet(ss_url, sheet = sheet_name, col_types = 'c')

# Sheet uses Czech Republic / Turkey; schedule uses Czechia / Turkiye
sheet_to_sched <- c('Czech Republic' = 'Czechia', 'Turkey' = 'Turkiye')

all_prob_cols <- c("P_R1", "P_R32", "P_R16", "P_QF", "P_SF", "P_LF", "P_WF")
round_to_col <- c(R1 = "P_R1", R32 = "P_R32", R16 = "P_R16",
                  QF = "P_QF", SF = "P_SF", LF = "P_LF", WF = "P_WF")

forecaster_probs <-
  df_raw %>%
  filter(name != 'Perfect Foresight') %>%
  mutate(across(all_of(all_prob_cols), as.numeric),
         team = recode(team, !!!sheet_to_sched)) %>%
  select(name, team, all_of(all_prob_cols))

### ── Compute team elimination rounds from schedule.csv ──────────────────────
schedule <- read_csv('data/schedule.csv', show_col_types = FALSE)

game_results <-
  schedule %>%
  filter(is.na(ko_round), !is.na(team1_score)) %>%
  select(group, team1, team2, s1 = team1_score, s2 = team2_score) %>%
  pivot_longer(c(team1, team2), names_to = 'side', values_to = 'team') %>%
  mutate(scored = if_else(side == 'team1', s1, s2),
         conceded = if_else(side == 'team1', s2, s1),
         pts = case_when(scored > conceded ~ 3L, scored == conceded ~ 1L, TRUE ~ 0L))

standings <-
  game_results %>%
  group_by(group, team) %>%
  summarise(pts = sum(pts), gd = sum(scored - conceded), gf = sum(scored), .groups = 'drop') %>%
  arrange(group, desc(pts), desc(gd), desc(gf)) %>%
  group_by(group) %>%
  mutate(place = row_number()) %>%
  ungroup()

third_place_advancing <-
  standings %>%
  filter(place == 3) %>%
  arrange(desc(pts), desc(gd), desc(gf)) %>%
  slice_head(n = 8) %>%
  pull(team)

gs_eliminated <-
  standings %>%
  mutate(d_r1 = case_when(place <= 2 ~ 0L,
                           place == 3 & team %in% third_place_advancing ~ 0L,
                           TRUE ~ 1L)) %>%
  filter(d_r1 == 1L) %>%
  select(team) %>%
  mutate(outcome_round = "R1")

ko_done <- filter(schedule, !is.na(ko_round), !is.na(team1_score))

ko_outcomes <- tibble(team = character(), outcome_round = character())
if(nrow(ko_done) > 0) {
  ko_with_result <-
    ko_done %>%
    mutate(
      round_prefix = str_extract(ko_round, "^[A-Za-z0-9]+"),
      loser = case_when(
        team1_score > team2_score ~ team2,
        team2_score > team1_score ~ team1,
        shootout_winner == team1 ~ team2,
        shootout_winner == team2 ~ team1
      ),
      winner = case_when(
        team1_score > team2_score ~ team1,
        team2_score > team1_score ~ team2,
        shootout_winner == team1 ~ team1,
        shootout_winner == team2 ~ team2
      )
    )

  ko_main <-
    ko_with_result %>%
    filter(round_prefix %in% c("R32", "R16", "QF", "SF")) %>%
    select(outcome_round = round_prefix, team = loser) %>%
    filter(!is.na(team))

  finals_rows <- filter(ko_with_result, round_prefix == "Finals")
  finals_outcomes <-
    if(nrow(finals_rows) > 0) {
      bind_rows(
        finals_rows %>% select(team = loser) %>% mutate(outcome_round = "LF"),
        finals_rows %>% select(team = winner) %>% mutate(outcome_round = "WF")
      ) %>%
      filter(!is.na(team))
    } else {
      tibble(team = character(), outcome_round = character())
    }

  ko_outcomes <- bind_rows(ko_main, finals_outcomes)
}

team_outcomes <- bind_rows(gs_eliminated, ko_outcomes)

n_resolved <- nrow(team_outcomes)
n_still_alive <- 48 - n_resolved

### ── Compute log scores by round ────────────────────────────────────────────
round_order <- c("R1", "R32", "R16", "QF", "SF", "LF", "WF")

scores_by_round <-
  forecaster_probs %>%
  inner_join(select(team_outcomes, team, outcome_round), by = 'team') %>%
  pivot_longer(all_of(all_prob_cols), names_to = "prob_col", values_to = "prob_used") %>%
  filter(prob_col == round_to_col[outcome_round]) %>%
  mutate(log_i = log(prob_used)) %>%
  group_by(name, outcome_round) %>%
  summarise(log_score = sum(log_i), .groups = 'drop')

resolved_rounds <- intersect(round_order, unique(scores_by_round$outcome_round))

### ── Score still-competing teams via survival probability ───────────────────
deduct_cols <- c("P_R1", paste0("P_", globally_resolved_rounds(schedule)))
survival_cols <- setdiff(all_prob_cols, deduct_cols)

active_scores <-
  forecaster_probs %>%
  anti_join(team_outcomes, by = 'team') %>%
  mutate(p_alive = rowSums(across(all_of(survival_cols)))) %>%
  group_by(name) %>%
  summarise(Active = sum(log(p_alive)), .groups = 'drop')

scores <-
  scores_by_round %>%
  pivot_wider(names_from = outcome_round, values_from = log_score) %>%
  left_join(active_scores, by = 'name') %>%
  mutate(log_score = rowSums(across(all_of(c(resolved_rounds, "Active"))), na.rm = TRUE)) %>%
  arrange(desc(log_score)) %>%
  mutate(rank = row_number())

### ── Build gt table ─────────────────────────────────────────────────────────
user_entry <- 'Respecs730 - v2'

tbl <-
  scores %>%
  select(rank, name, all_of(resolved_rounds), Active, log_score) %>%
  gt() %>%
  cols_label(rank = 'Rank',
             name = 'Forecaster',
             Active = 'Active',
             log_score = 'Total',
             !!!setNames(resolved_rounds, resolved_rounds)) %>%
  fmt_number(columns = c(all_of(resolved_rounds), Active, log_score), decimals = 3) %>%
  tab_spanner(label = 'Log Score by Round', columns = all_of(resolved_rounds)) %>%
  tab_header(title = md('**FIFA World Cup 2026 — Forecasting Contest**'),
             subtitle = md(glue::glue(
               '{n_resolved} of 48 teams resolved &nbsp;·&nbsp; {n_still_alive} still competing'
             ))) %>%
  tab_style(style = list(cell_fill(color = '#E63946', alpha = 0.20),
                         cell_text(weight = 'bold')),
            locations = cells_body(rows = name == user_entry)) %>%
  tab_style(style = cell_text(weight = 'bold'),
            locations = cells_body(columns = log_score)) %>%
  tab_style(style = cell_text(color = 'grey50'),
            locations = cells_body(columns = rank)) %>%
  tab_footnote(footnote = md(paste0(
    'Log score (higher is better): for resolved teams, Σ log(p) where p is the pre-tournament ',
    'marginal probability of the team\'s actual elimination round (GS/R32/R16/QF/SF/LF/WF). ',
    'For still-competing teams (Active column), Σ log(1 − Σ eliminated-round p) using survival ',
    'probability through the most recently completed round. ',
    '3rd-place tiebreaker: points → goal difference → goals scored.')),
    locations = cells_column_labels(columns = log_score)) %>%
  cols_width(rank ~ px(55),
             name ~ px(280),
             Active ~ px(75),
             log_score ~ px(90),
             everything() ~ px(75)) %>%
  opt_table_font(font = google_font('Source Sans Pro')) %>%
  tab_options(table.border.top.color = 'transparent',
              heading.border.bottom.color = '#dddddd',
              column_labels.border.top.color = '#dddddd',
              table_body.border.bottom.color = '#dddddd',
              heading.title.font.size = px(18),
              heading.subtitle.font.size = px(13),
              data_row.padding = px(5))

gtsave(tbl, 'figures/comparison/contest_standings.png', zoom = 2)
