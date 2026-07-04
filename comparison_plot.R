library(tidyverse)
library(googlesheets4)
library(ggimage)
library(ggtext)
source('helpers.R')

dir.create('figures/comparison', showWarnings = FALSE)

ss_url <- "https://docs.google.com/spreadsheets/d/1_UKsDiPET4ifTlTkUeIMqPLeKrGEOkgsqAPUg9uT-sc/edit"

gs4_auth(email = "lukesbenz@gmail.com")

df_raw <- read_sheet(ss_url, sheet = 'All Entries', col_types = "c") %>%
  filter(name != 'Perfect Foresight')

### ── Highlighted forecasters ────────────────────────────────────────────────
highlight_cfg <-
  tribble( ~name, ~label, ~colour,
           "Respecs730 - v2", "Recspecs730", "#E63946",
           # "Bookmaker Consensus", "Bookmaker Consensus", "#2A9D8F",
           "Silver Bulletin", "Silver Bulletin", "#F4A261",
           "Internally Consistent Polymarket", "Polymarket", "#9B5DE5")

### ── Fix team name → flag file mismatches ───────────────────────────────────
name_fixes <-
  c("Czech Republic" = "Czechia",
    "Turkey" = "Turkiye")

### ── Reshape ─────────────────────────────────────────────────────────────────
df_long <-
  df_raw %>%
  select(name, team, P_R1) %>%
  mutate('P_R1' = as.numeric(P_R1),
         'adv_prob' = 1 - P_R1,
         'team' = recode(team, !!!name_fixes),
         'logo' = paste0('flags/', team, '.png')) %>%
  filter(!is.na(adv_prob), name != "Agnostic") %>%
  left_join(highlight_cfg, by = "name") %>%
  mutate('is_highlight' = !is.na(label),
         'legend_label' = if_else(is.na(label), "Other", label),
         'dot_colour' = if_else(is.na(colour), "#999999", colour))

### ── Sort teams by Collective Consciousness advance probability, ascending ───
team_order <-
  df_long %>%
  filter(name == "Collective Consciousness") %>%
  arrange(adv_prob) %>%
  pull(team)

df_long <- mutate(df_long, 'team' = factor(team, levels = team_order))

df_logos <- df_long %>% distinct(team, logo)
df_other <- filter(df_long, !is_highlight)
df_hi <- filter(df_long, is_highlight)

legend_colours <-
  highlight_cfg %>%
  select(label, colour) %>%
  deframe()

### ── Team label colours (green = advanced, red = eliminated) ────────────────
team_labels <-
  read_csv('predictions/sim_results.csv', show_col_types = F) %>%
  select(team, r32) %>%
  mutate('text_colour' = case_when(r32 == 1 ~ "#27AE60",
                                   r32 == 0 ~ "#E74C3C",
                                   TRUE ~ "black"),
         'md_label' = paste0("<span style='color:", text_colour, "'>", team, "</span>")) %>%
  select(team, md_label) %>%
  deframe()

n_teams <- n_distinct(df_long$team)
plot_height <- max(10, n_teams * 0.25)

### ── Plot ───────────────────────────────────────────────────────────────────
ggplot(df_long, aes(x = adv_prob, y = team)) +
  geom_point(data = df_other,
             colour = "#999999",
             size = 1.5,
             alpha = 0.35,
             shape = 16) +
  geom_point(data = df_hi,
             aes(colour = legend_label),
             size = 2.8,
             alpha = 1,
             shape = 16) +
  geom_image(data = df_logos,
             aes(x = -0.04, y = team, image = logo),
             size = 0.017,
             asp = plot_height / 10) +
  scale_x_continuous(labels = scales::percent,
                     limits = c(-0.06, 1),
                     breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_colour_manual(values = legend_colours) +
  scale_y_discrete(labels = team_labels) +
  labs(x = "Probability of Advancing from Group Stage",
       y = NULL,
       title = "FIFA World Cup 2026",
       subtitle = "Group Stage Advance Probability - Forecaster Comparison",
       colour = NULL) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_markdown(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 8, r = 12, b = 8, l = 8))

ggsave('figures/comparison/advance_prob_comparison.png',
       height = plot_height,
       width = 16)

### ── Log-score table ────────────────────────────────────────────────────────
sim_results <-
  read_csv('predictions/sim_results.csv', show_col_types = F) %>%
  select(team, current = r32)

forecasters_wide <-
  df_raw %>%
  filter(name %in% c("Silver Bulletin", "Internally Consistent Polymarket", "Respecs730 - v2")) %>%
  mutate(
    team = recode(team, !!!name_fixes),
    adv = 1 - as.numeric(P_R1),
    name = case_when(
      name == "Internally Consistent Polymarket" ~ "Polymarket",
      name == "Respecs730 - v2" ~ "Recspecs730",
      TRUE ~ name
    )
  ) %>%
  select(name, team, adv) %>%
  pivot_wider(names_from = name, values_from = adv)

forecaster_emoji <- c(Recspecs730 = "👓", `Silver Bulletin` = "🟩", Polymarket = "🔵")

margin_stars <- function(margin) {
  if (margin < 0.05) "⭐"
  else if (margin < 0.25) "⭐⭐"
  else "⭐⭐⭐"
}

fmt_cell <- function(p, outcome) {
  pct <- sprintf("%.1f%%", p * 100)
  if (outcome == 1) sprintf("%s (%.3f)", pct, log(p))
  else if (outcome == 0) sprintf("%s (%.3f)", pct, log(1 - p))
  else pct
}

tbl <-
  sim_results %>%
  left_join(forecasters_wide, by = "team") %>%
  arrange(desc(current)) %>%
  rowwise() %>%
  mutate(
    winner = {
      if (current %in% c(0, 1)) {
        ls <- c(
          Recspecs730 = if (current == 1) log(Recspecs730) else log(1 - Recspecs730),
          `Silver Bulletin` = if (current == 1) log(`Silver Bulletin`) else log(1 - `Silver Bulletin`),
          Polymarket = if (current == 1) log(Polymarket) else log(1 - Polymarket)
        )
        sorted <- sort(ls, decreasing = TRUE)
        margin <- sorted[1] - sorted[2]
        winner_name <- names(sorted)[1]
        paste(forecaster_emoji[winner_name], winner_name, margin_stars(margin))
      } else NA_character_
    },
    Recspecs730_fmt = fmt_cell(Recspecs730, current),
    Polymarket_fmt = fmt_cell(Polymarket, current),
    SilverBulletin_fmt = fmt_cell(`Silver Bulletin`, current)
  ) %>%
  ungroup()

resolved <- filter(tbl, current %in% c(0, 1))
log_scores <-
  resolved %>%
  summarise(
    Recspecs730 = sum(if_else(current == 1, log(Recspecs730), log(1 - Recspecs730))),
    Polymarket = sum(if_else(current == 1, log(Polymarket), log(1 - Polymarket))),
    SilverBulletin = sum(if_else(current == 1, log(`Silver Bulletin`), log(1 - `Silver Bulletin`)))
  )

tbl_fmt <-
  tbl %>%
  mutate(current = sprintf("%.1f%%", current * 100), winner = replace_na(winner, "")) %>%
  select(Team = team, `Current (👓 Recspecs730)` = current,
         `Pre-WC (👓 Recspecs730)` = Recspecs730_fmt,
         `🔵 Polymarket` = Polymarket_fmt,
         `🟩 Silver Bulletin` = SilverBulletin_fmt,
         Winner = winner)

scores_row <-
  tibble(
    Team = c("", "**Log Score**"),
    `Current (👓 Recspecs730)` = c("", ""),
    `Pre-WC (👓 Recspecs730)` = c("", sprintf("%.3f", log_scores$Recspecs730)),
    `🔵 Polymarket` = c("", sprintf("%.3f", log_scores$Polymarket)),
    `🟩 Silver Bulletin` = c("", sprintf("%.3f", log_scores$SilverBulletin)),
    Winner = c("", "")
  )

resolved <- filter(tbl, current %in% c(0, 1))

brier_scores <-
  resolved %>%
  summarise(
    `👓 Recspecs730` = sum(2 * (Recspecs730 - current)^2),
    `🔵 Polymarket` = sum(2 * (Polymarket - current)^2),
    `🟩 Silver Bulletin` = sum(2 * (`Silver Bulletin` - current)^2)
  )

brier_tbl <-
  brier_scores %>%
  pivot_longer(everything(), names_to = "Forecaster", values_to = "Brier Score") %>%
  arrange(`Brier Score`) %>%
  mutate(`Brier Score` = sprintf("%.3f", `Brier Score`))

writeLines(
  c(
    knitr::kable(bind_rows(tbl_fmt, scores_row), format = "markdown"),
    "",
    "_**Log score:** For each clinched/eliminated team, each forecaster earns_",
    "_log(p) if the team advanced or log(1 - p) if the team was eliminated,_",
    "_where p is the forecaster's pre-tournament probability of advancing from the group stage._",
    "_Scores are summed across all resolved teams (higher is better)._",
    "",
    "_**Winner margin key:** ⭐ < 0.05 log score margin | ⭐⭐ 0.05–0.25 | ⭐⭐⭐ ≥ 0.25_",
    "",
    "## Brier Score (resolved teams)",
    "",
    knitr::kable(brier_tbl, format = "markdown"),
    "",
    "_**Brier score:** For each clinched/eliminated team, each forecaster is penalized (p - outcome)²,_",
    "_where p is the advancement probability and outcome is 1 if advanced, 0 if eliminated._",
    "_Scores are summed across all resolved teams (lower is better)._"
  ),
  'figures/comparison/log_score_table.txt'
)

### ── R32 Elim Prob Comparison Plot ──────────────────────────────────────────
r32_teams <-
  read_csv('predictions/sim_results.csv', show_col_types = F) %>%
  filter(r32 == 1) %>%
  pull(team)

df_long_r32 <-
  df_raw %>%
  select(name, team, P_R32) %>%
  mutate('P_R32' = as.numeric(P_R32),
         'team' = recode(team, !!!name_fixes),
         'logo' = paste0('flags/', team, '.png')) %>%
  filter(!is.na(P_R32), name != "Agnostic", team %in% r32_teams) %>%
  left_join(highlight_cfg, by = "name") %>%
  mutate('is_highlight' = !is.na(label),
         'legend_label' = if_else(is.na(label), "Other", label),
         'dot_colour' = if_else(is.na(colour), "#999999", colour))

team_order_r32 <-
  df_long_r32 %>%
  filter(name == "Collective Consciousness") %>%
  arrange(P_R32) %>%
  pull(team)

df_long_r32 <- mutate(df_long_r32, 'team' = factor(team, levels = team_order_r32))

df_logos_r32 <- df_long_r32 %>% distinct(team, logo)
df_other_r32 <- filter(df_long_r32, !is_highlight)
df_hi_r32 <- filter(df_long_r32, is_highlight)

r32_team_labels <-
  read_csv('predictions/sim_results.csv', show_col_types = F) %>%
  filter(team %in% r32_teams) %>%
  select(team, r16) %>%
  mutate('text_colour' = case_when(r16 == 1 ~ "#27AE60",
                                   r16 == 0 ~ "#E74C3C",
                                   TRUE ~ "black"),
         'md_label' = paste0("<span style='color:", text_colour, "'>", team, "</span>")) %>%
  select(team, md_label) %>%
  deframe()

n_teams_r32 <- n_distinct(df_long_r32$team)
plot_height_r32 <- max(10, n_teams_r32 * 0.25)

ggplot(df_long_r32, aes(x = P_R32, y = team)) +
  geom_point(data = df_other_r32,
             colour = "#999999",
             size = 1.5,
             alpha = 0.35,
             shape = 16) +
  geom_point(data = df_hi_r32,
             aes(colour = legend_label),
             size = 2.8,
             alpha = 1,
             shape = 16) +
  geom_image(data = df_logos_r32,
             aes(x = -0.04, y = team, image = logo),
             size = 0.017,
             asp = plot_height_r32 / 10) +
  scale_x_continuous(labels = scales::percent,
                     limits = c(-0.06, 1),
                     breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_colour_manual(values = legend_colours) +
  scale_y_discrete(labels = r32_team_labels) +
  labs(x = "Probability of Being Eliminated in Round of 32",
       y = NULL,
       title = "FIFA World Cup 2026",
       subtitle = "R32 Elimination Probability - Forecaster Comparison",
       colour = NULL) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_markdown(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 8, r = 12, b = 8, l = 8))

ggsave('figures/comparison/r32_elim_comparison.png',
       height = plot_height_r32,
       width = 16)

### ── All-rounds log-score table ─────────────────────────────────────────────
all_prob_cols <- c("P_R1", "P_R32", "P_R16", "P_QF", "P_SF", "P_LF", "P_WF")
round_to_col <- c(R1 = "P_R1", R32 = "P_R32", R16 = "P_R16",
                  QF = "P_QF", SF = "P_SF", LF = "P_LF", WF = "P_WF")

sched_ar <- read_csv('data/schedule.csv', show_col_types = F)

gs_games_ar <-
  sched_ar %>%
  filter(is.na(ko_round), !is.na(team1_score)) %>%
  select(group, team1, team2, s1 = team1_score, s2 = team2_score) %>%
  pivot_longer(c(team1, team2), names_to = 'side', values_to = 'team') %>%
  mutate(scored = if_else(side == 'team1', s1, s2),
         conceded = if_else(side == 'team1', s2, s1),
         pts = case_when(scored > conceded ~ 3L, scored == conceded ~ 1L, TRUE ~ 0L))

standings_ar <-
  gs_games_ar %>%
  group_by(group, team) %>%
  summarise(pts = sum(pts), gd = sum(scored - conceded), gf = sum(scored), .groups = 'drop') %>%
  arrange(group, desc(pts), desc(gd), desc(gf)) %>%
  group_by(group) %>%
  mutate(place = row_number()) %>%
  ungroup()

third_adv_ar <-
  standings_ar %>%
  filter(place == 3) %>%
  arrange(desc(pts), desc(gd), desc(gf)) %>%
  slice_head(n = 8) %>%
  pull(team)

gs_elim_ar <-
  standings_ar %>%
  mutate(d_r1 = case_when(place <= 2 ~ 0L,
                           place == 3 & team %in% third_adv_ar ~ 0L,
                           TRUE ~ 1L)) %>%
  filter(d_r1 == 1L) %>%
  select(team) %>%
  mutate(outcome_round = "R1")

ko_done_ar <- filter(sched_ar, !is.na(ko_round), !is.na(team1_score))

ko_outcomes_ar <- tibble(team = character(), outcome_round = character())
if(nrow(ko_done_ar) > 0) {
  ko_res_ar <-
    ko_done_ar %>%
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

  ko_main_ar <-
    ko_res_ar %>%
    filter(round_prefix %in% c("R32", "R16", "QF", "SF")) %>%
    select(outcome_round = round_prefix, team = loser) %>%
    filter(!is.na(team))

  finals_ar <- filter(ko_res_ar, round_prefix == "Finals")
  finals_outcomes_ar <-
    if(nrow(finals_ar) > 0) {
      bind_rows(
        finals_ar %>% select(team = loser) %>% mutate(outcome_round = "LF"),
        finals_ar %>% select(team = winner) %>% mutate(outcome_round = "WF")
      ) %>%
      filter(!is.na(team))
    } else {
      tibble(team = character(), outcome_round = character())
    }

  ko_outcomes_ar <- bind_rows(ko_main_ar, finals_outcomes_ar)
}

team_outcomes_ar <- bind_rows(gs_elim_ar, ko_outcomes_ar)

deduct_cols_ar <- c("P_R1", paste0("P_", globally_resolved_rounds(sched_ar)))

hi_names_ar <- c("Silver Bulletin", "Internally Consistent Polymarket", "Respecs730 - v2")
name_map_ar <- c("Internally Consistent Polymarket" = "Polymarket",
                 "Respecs730 - v2" = "Recspecs730")

forecasters_ar <-
  df_raw %>%
  filter(name %in% hi_names_ar) %>%
  mutate(team = recode(team, !!!name_fixes),
         across(all_of(all_prob_cols), as.numeric),
         name = recode(name, !!!name_map_ar)) %>%
  select(name, team, all_of(all_prob_cols))

survival_cols_ar <- setdiff(all_prob_cols, deduct_cols_ar)

scoring_probs_ar <-
  bind_rows(
    forecasters_ar %>%
      left_join(select(team_outcomes_ar, team, outcome_round), by = "team") %>%
      filter(!is.na(outcome_round)) %>%
      pivot_longer(all_of(all_prob_cols), names_to = "prob_col", values_to = "prob_used") %>%
      filter(prob_col == round_to_col[outcome_round]) %>%
      select(name, team, outcome_round, prob_used),
    forecasters_ar %>%
      left_join(select(team_outcomes_ar, team, outcome_round), by = "team") %>%
      filter(is.na(outcome_round)) %>%
      mutate(prob_used = rowSums(across(all_of(survival_cols_ar)))) %>%
      select(name, team, outcome_round, prob_used)
  )

scoring_wide_ar <-
  scoring_probs_ar %>%
  pivot_wider(names_from = name, values_from = prob_used) %>%
  rename(p_Recspecs730 = Recspecs730,
         p_Polymarket = Polymarket,
         p_SilverBulletin = `Silver Bulletin`)

recspecs_current_ar <-
  forecasters_ar %>%
  filter(name == "Recspecs730") %>%
  select(-name) %>%
  left_join(select(team_outcomes_ar, team, outcome_round), by = "team") %>%
  mutate(current = if_else(!is.na(outcome_round), 0,
                           rowSums(across(all_of(survival_cols_ar))))) %>%
  select(team, current)

tbl_ar <-
  scoring_wide_ar %>%
  left_join(recspecs_current_ar, by = "team") %>%
  arrange(desc(current)) %>%
  rowwise() %>%
  mutate(
    winner_ar = if(!is.na(outcome_round)) {
      ls <- c(Recspecs730 = log(p_Recspecs730),
               Polymarket = log(p_Polymarket),
               `Silver Bulletin` = log(p_SilverBulletin))
      sorted <- sort(ls, decreasing = TRUE)
      margin <- sorted[1] - sorted[2]
      paste(forecaster_emoji[names(sorted)[1]], names(sorted)[1], margin_stars(margin))
    } else {
      NA_character_
    }
  ) %>%
  ungroup() %>%
  mutate(
    current_pct = sprintf("%.1f%%", current * 100),
    recspecs_fmt = if_else(!is.na(outcome_round),
                           sprintf("%.1f%% (%.3f)", p_Recspecs730 * 100, log(p_Recspecs730)),
                           sprintf("%.1f%%", p_Recspecs730 * 100)),
    polymarket_fmt = if_else(!is.na(outcome_round),
                             sprintf("%.1f%% (%.3f)", p_Polymarket * 100, log(p_Polymarket)),
                             sprintf("%.1f%%", p_Polymarket * 100)),
    sb_fmt = if_else(!is.na(outcome_round),
                     sprintf("%.1f%% (%.3f)", p_SilverBulletin * 100, log(p_SilverBulletin)),
                     sprintf("%.1f%%", p_SilverBulletin * 100)),
    winner_ar = replace_na(winner_ar, "")
  )

tbl_ar_fmt <-
  tbl_ar %>%
  select(Team = team,
         outcome_round,
         `Current (👓 Recspecs730)` = current_pct,
         `Pre-WC (👓 Recspecs730)` = recspecs_fmt,
         `🔵 Polymarket` = polymarket_fmt,
         `🟩 Silver Bulletin` = sb_fmt,
         Winner = winner_ar)

rounds_output_order_ar <- c("WF", "LF", "SF", "QF", "R16", "R32", "R1")
section_labels_ar <- c(
  R1  = "**— Eliminated: Group Stage —**",
  R32 = "**— Eliminated: Round of 32 —**",
  R16 = "**— Eliminated: Round of 16 —**",
  QF  = "**— Eliminated: Quarterfinals —**",
  SF  = "**— Eliminated: Semifinals —**",
  LF  = "**— Runner-up —**",
  WF  = "**— Champion —**"
)

present_rounds_ar <- intersect(rounds_output_order_ar, team_outcomes_ar$outcome_round)

alive_kable_ar <-
  tbl_ar_fmt %>%
  filter(is.na(outcome_round)) %>%
  select(-outcome_round) %>%
  knitr::kable(format = "markdown")

### ── Round-by-round log score breakdown ─────────────────────────────────────
round_order_ar <- c("R1", "R32", "R16", "QF", "SF", "LF", "WF", "Active")

round_breakdown_ar <-
  scoring_probs_ar %>%
  mutate(round_label = if_else(is.na(outcome_round), "Active", outcome_round)) %>%
  group_by(name, round_label) %>%
  summarise(log_score = sum(log(prob_used)), .groups = 'drop')

present_round_labels_ar <- intersect(round_order_ar, unique(round_breakdown_ar$round_label))

breakdown_wide_ar <-
  round_breakdown_ar %>%
  pivot_wider(names_from = round_label, values_from = log_score) %>%
  mutate(Total = rowSums(across(all_of(present_round_labels_ar)), na.rm = TRUE),
         Forecaster = case_when(
           name == "Recspecs730" ~ "👓 Recspecs730",
           name == "Polymarket" ~ "🔵 Polymarket",
           TRUE ~ "🟩 Silver Bulletin"
         )) %>%
  arrange(desc(Total)) %>%
  select(Forecaster, all_of(present_round_labels_ar), Total) %>%
  mutate(across(all_of(c(present_round_labels_ar, "Total")), ~sprintf("%.3f", .)))

elim_kables_ar <-
  present_rounds_ar %>%
  map(function(rnd) {
    rows <- filter(tbl_ar_fmt, outcome_round == rnd) %>% select(-outcome_round)
    c("",
      section_labels_ar[rnd],
      "",
      knitr::kable(rows, format = "markdown"))
  }) %>%
  unlist()

brier_ar <-
  forecasters_ar %>%
  left_join(select(team_outcomes_ar, team, outcome_round), by = "team") %>%
  filter(!is.na(outcome_round)) %>%
  pivot_longer(all_of(all_prob_cols), names_to = "prob_col", values_to = "p") %>%
  mutate(actual_col = round_to_col[outcome_round],
         ind = as.integer(prob_col == actual_col),
         sq_err = (p - ind)^2) %>%
  group_by(name) %>%
  summarise(brier = sum(sq_err), .groups = 'drop') %>%
  arrange(brier) %>%
  mutate(
    Forecaster = case_when(
      name == "Recspecs730" ~ "👓 Recspecs730",
      name == "Polymarket" ~ "🔵 Polymarket",
      TRUE ~ "🟩 Silver Bulletin"
    ),
    `Brier Score` = sprintf("%.3f", brier)
  ) %>%
  select(Forecaster, `Brier Score`)

n_resolved_ar <- nrow(team_outcomes_ar)

writeLines(
  c(
    c(alive_kable_ar, elim_kables_ar),
    "",
    "## Log Score by Round",
    "",
    knitr::kable(breakdown_wide_ar, format = "markdown"),
    "",
    "_**Log score (all rounds):** For each resolved team, each forecaster earns log(p) where p is their_",
    "_pre-tournament marginal probability of the team's actual elimination round (or championship)._",
    "_For each still-competing team, each forecaster earns log(1 − Σ p) using their pre-tournament_",
    "_survival probability through the most recently completed round (the Active column)._",
    sprintf("_Scores are summed across all 48 teams (%d resolved, %d still competing; higher is better)._",
            n_resolved_ar, 48 - n_resolved_ar),
    "",
    "_**Current probability:** For still-competing teams, shows Recspecs730's pre-tournament_",
    "_probability of the team surviving all rounds played so far (1 − Σ P_eliminated for resolved rounds)._",
    "",
    "_**Winner margin key:** ⭐ < 0.05 log score margin | ⭐⭐ 0.05–0.25 | ⭐⭐⭐ ≥ 0.25_",
    "",
    "## Brier Score — multi-category (resolved teams)",
    "",
    knitr::kable(brier_ar, format = "markdown"),
    "",
    "_**Brier score (multi-category):** For each resolved team, each forecaster is penalized_",
    "_Σ (p_j − I_j)² across all 7 outcome categories (GS/R32/R16/QF/SF/LF/WF),_",
    "_where I_j = 1 for the actual outcome and 0 otherwise. Scores are summed (lower is better)._"
  ),
  'figures/comparison/log_score_table_allrounds.txt'
)
