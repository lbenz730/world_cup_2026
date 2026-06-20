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
       subtitle = "Group Stage Advance Probability — Forecaster Comparison",
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
