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

forecaster_probs <-
  df_raw %>%
  mutate(adv_prob = 1 - as.numeric(P_R1),
         team = recode(team, !!!sheet_to_sched)) %>%
  select(name, team, adv_prob)

### ── Compute current group standings from schedule.csv ──────────────────────
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

# Best 8 of 12 third-place teams advance
third_place_advancing <-
  standings %>%
  filter(place == 3) %>%
  arrange(desc(pts), desc(gd), desc(gf)) %>%
  slice_head(n = 8) %>%
  pull(team)

d_r1 <-
  standings %>%
  mutate(d_r1 = case_when(place <= 2 ~ 0L,
                          place == 3 & team %in% third_place_advancing ~ 0L,
                          TRUE ~ 1L)) %>%
  select(team, d_r1)

n_advancing <- sum(d_r1$d_r1 == 0)
n_eliminated <- sum(d_r1$d_r1 == 1)

### ── Compute scores ─────────────────────────────────────────────────────────
scores <-
  forecaster_probs %>%
  inner_join(d_r1, by = 'team') %>%
  mutate(outcome_adv = 1L - d_r1,
         log_i = if_else(d_r1 == 1L, log(1 - adv_prob), log(adv_prob)),
         brier_i = 2 * (adv_prob - outcome_adv)^2) %>%
  group_by(name) %>%
  summarise(log_score = sum(log_i),
            brier = sum(brier_i),
            .groups = 'drop') %>%
  arrange(desc(log_score)) %>%
  mutate(rank = row_number())

### ── Build gt table ─────────────────────────────────────────────────────────
user_entry <- 'Respecs730 - v2'

tbl <-
  scores %>%
  select(rank, name, log_score, brier) %>%
  gt() %>%
  cols_label(rank = 'Rank',
             name = 'Forecaster',
             log_score = 'Log Score',
             brier = 'Brier') %>%
  fmt_number(columns = c(log_score, brier), decimals = 3) %>%
  tab_header(title = md('**FIFA World Cup 2026 — Group Stage Forecasting Contest**'),
             subtitle = md(glue::glue(
               'Current group standings &nbsp;·&nbsp; ',
               '{n_advancing} advancing / {n_eliminated} eliminated (48 teams total)'
             ))) %>%
  tab_style(style = list(cell_fill(color = '#E63946', alpha = 0.20),
                         cell_text(weight = 'bold')),
            locations = cells_body(rows = name == user_entry)) %>%
  tab_style(style = cell_text(color = 'grey50'),
            locations = cells_body(columns = rank)) %>%
  tab_footnote(footnote = md(paste0(
    'Log score (higher is better): Σ log(p) if advanced + Σ log(1−p) if eliminated. ',
    'Brier (lower is better): Σ 2(p − outcome)². ',
    'p = pre-tournament probability of advancing from the group stage. ',
    '3rd-place tiebreaker: points → goal difference → goals scored.')),
    locations = cells_column_labels(columns = log_score)) %>%
  cols_width(rank ~ px(55),
             name ~ px(300),
             log_score ~ px(110),
             brier ~ px(90)) %>%
  opt_table_font(font = google_font('Source Sans Pro')) %>%
  tab_options(table.border.top.color = 'transparent',
              heading.border.bottom.color = '#dddddd',
              column_labels.border.top.color = '#dddddd',
              table_body.border.bottom.color = '#dddddd',
              heading.title.font.size = px(18),
              heading.subtitle.font.size = px(13),
              data_row.padding = px(5))

gtsave(tbl, 'figures/comparison/contest_standings.png', zoom = 2)
