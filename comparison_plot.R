library(tidyverse)
library(googlesheets4)
library(ggimage)
library(ggtext)
source('helpers.R')

dir.create('figures/comparison', showWarnings = FALSE)

ss_url <- "https://docs.google.com/spreadsheets/d/1WxpBE-endMaSFqxzF5GSnRP1y80dqW6g2wmgvxNkcpA"

gs4_auth(email = "lukesbenz@gmail.com")

### ── Read the predictions tab (gid 596723494) ───────────────────────────────
meta <- gs4_get(ss_url)
sheet_name <- meta$sheets$name[meta$sheets$id == 596723494]
df_raw <- read_sheet(ss_url, sheet = sheet_name, col_types = "c")

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

pre_wc <-
  read_csv('predictions/history.csv', show_col_types = F) %>%
  filter(date == min(date)) %>%
  select(team, pre_wc = r32)

forecasters_wide <-
  df_raw %>%
  filter(name %in% c("Silver Bulletin", "Internally Consistent Polymarket")) %>%
  mutate(
    team = recode(team, !!!name_fixes),
    adv = 1 - as.numeric(P_R1),
    name = if_else(name == "Internally Consistent Polymarket", "Polymarket", name)
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
  left_join(pre_wc, by = "team") %>%
  left_join(forecasters_wide, by = "team") %>%
  arrange(desc(current)) %>%
  rowwise() %>%
  mutate(
    winner = {
      if (current %in% c(0, 1)) {
        ls <- c(
          Recspecs730 = if (current == 1) log(pre_wc) else log(1 - pre_wc),
          `Silver Bulletin` = if (current == 1) log(`Silver Bulletin`) else log(1 - `Silver Bulletin`),
          Polymarket = if (current == 1) log(Polymarket) else log(1 - Polymarket)
        )
        sorted <- sort(ls, decreasing = TRUE)
        margin <- sorted[1] - sorted[2]
        winner_name <- names(sorted)[1]
        paste(forecaster_emoji[winner_name], winner_name, margin_stars(margin))
      } else NA_character_
    },
    Recspecs730_fmt = fmt_cell(pre_wc, current),
    Polymarket_fmt = fmt_cell(Polymarket, current),
    SilverBulletin_fmt = fmt_cell(`Silver Bulletin`, current)
  ) %>%
  ungroup()

resolved <- filter(tbl, current %in% c(0, 1))
log_scores <-
  resolved %>%
  summarise(
    Recspecs730 = sum(if_else(current == 1, log(pre_wc), log(1 - pre_wc))),
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
    `👓 Recspecs730` = sum((pre_wc - current)^2),
    `🔵 Polymarket` = sum((Polymarket - current)^2),
    `🟩 Silver Bulletin` = sum((`Silver Bulletin` - current)^2)
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
