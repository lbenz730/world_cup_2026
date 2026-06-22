### KO Round Opponent Mini-Tables
library(tidyverse)
library(gt)
library(magick)
library(ggsci)

r32_results <- read_rds('predictions/sim_rds/r32_results.rds')
r16_results <- read_rds('predictions/sim_rds/r16_results.rds')
sim_results <- read_csv('predictions/sim_results.csv', show_col_types = F)
n_sims <- length(r32_results)

dir.create('figures/ko_mini_tables/r32', showWarnings = FALSE, recursive = TRUE)
dir.create('figures/ko_mini_tables/r16', showWarnings = FALSE, recursive = TRUE)

### ── Generic mini-table builder ─────────────────────────────────────────────
make_ko_mini_table <- function(team_name, df, elim_label = 'Elim. Before R32') {
  elim_prob <- df$elim_prob[1]
  group <- df$group[1]

  header <- html(paste0(
    "<div style='display:flex; align-items:center; justify-content:center; gap:6px;'>",
    "<img src='", getwd(), "/flags/", team_name, ".png' height='25'>",
    "<b>", team_name, " (", group, ")</b>",
    "</div>"
  ))

  if (elim_prob == 1) {
    return(
      tibble(logo = NA_character_, label = elim_label, prob = 1.0) %>%
        gt() %>%
        tab_header(title = header) %>%
        text_transform(locations = cells_body(columns = logo),
                       fn = function(x) '') %>%
        fmt_percent(columns = prob, decimals = 1) %>%
        tab_style(style = list(cell_fill(color = '#e8e8e8'), cell_text(style = 'italic')),
                  locations = cells_body(rows = 1)) %>%
        tab_style(style = cell_fill(color = '#e8e8e8'),
                  locations = cells_title()) %>%
        cols_label(logo = '', label = '', prob = '') %>%
        cols_align(align = 'left', columns = label) %>%
        cols_align(align = 'right', columns = prob) %>%
        tab_options(table.font.size = 12,
                    heading.title.font.size = 13,
                    heading.title.font.weight = 'bold',
                    data_row.padding = px(3),
                    table.border.top.style = 'solid',
                    table.border.top.color = 'black',
                    table.width = px(300))
    )
  }

  top_opps <- df %>% filter(rank %in% 1:3) %>% arrange(rank)
  other_row <- df %>% filter(opponent == 'Other')
  n_pad <- 3 - nrow(top_opps)

  other_prob <- if (nrow(other_row) > 0) {
    other_row$prob
  } else {
    0
  }

  rows <-
    tibble(logo = c(top_opps$logo, rep(NA_character_, n_pad), NA, NA),
           label = c(top_opps$opponent, rep('—', n_pad), 'Other', elim_label),
           prob = c(top_opps$prob, rep(0, n_pad), other_prob, elim_prob))

  rows %>%
    gt() %>%
    tab_header(title = header) %>%
    text_transform(locations = cells_body(columns = logo),
                   fn = function(x) {
                     map_chr(x, ~ {
                       if (is.na(.x)) {
                         ''
                       } else {
                         local_image(.x, height = 16)
                       }
                     })
                   }) %>%
    fmt_percent(columns = prob, decimals = 1) %>%
    data_color(columns = prob,
               rows = 1:4,
               fn = scales::col_numeric(palette = rgb_material('amber', n = 100), domain = c(0, 1))) %>%
    tab_style(style = list(cell_fill(color = '#e8e8e8'), cell_text(style = 'italic')),
              locations = cells_body(rows = 5)) %>%
    cols_label(logo = '', label = '', prob = '') %>%
    cols_align(align = 'left', columns = label) %>%
    cols_align(align = 'right', columns = prob) %>%
    tab_options(table.font.size = 12,
                heading.title.font.size = 13,
                heading.title.font.weight = 'bold',
                data_row.padding = px(3),
                table.border.top.style = 'solid',
                table.border.top.color = 'black',
                table.width = px(300))
}

### ── Helper: build padded image list and montage grid ───────────────────────
make_grid <- function(imgs, n_col, title_text) {
  max_h <- max(map_int(imgs, ~ image_info(.x)$height))
  img_w <- image_info(imgs[[1]])$width

  imgs <-
    map(imgs, ~ image_extent(.x, paste0(img_w, 'x', max_h), color = 'white', gravity = 'North'))

  grid <-
    image_join(imgs) %>%
    image_montage(geometry = '+2+2', tile = paste0(n_col, 'x'), bg = 'white')

  title_img <-
    image_blank(image_info(grid)$width, 80, color = 'white') %>%
    image_annotate(title_text, gravity = 'Center', size = 40, weight = 700, color = 'black')

  image_append(image_join(list(title_img, grid)), stack = TRUE)
}

### ── R32 ─────────────────────────────────────────────────────────────────────
r32_games <- bind_rows(r32_results)

r32_opponent_probs <-
  bind_rows(r32_games %>% select(team = team1, opponent = team2),
            r32_games %>% select(team = team2, opponent = team1)) %>%
  group_by(team, opponent) %>%
  summarise(prob = n() / n_sims, .groups = 'drop') %>%
  arrange(team, desc(prob)) %>%
  group_by(team) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top3_r32 <- filter(r32_opponent_probs, rank <= 3)

other_r32 <-
  r32_opponent_probs %>%
  filter(rank > 3) %>%
  group_by(team) %>%
  summarise(prob = sum(prob), .groups = 'drop') %>%
  mutate(opponent = 'Other', rank = 4L)

active_teams_r32 <-
  sim_results %>%
  arrange(group, desc(r32)) %>%
  select(team, group, r32)

team_rows_r32 <-
  bind_rows(top3_r32, other_r32) %>%
  right_join(active_teams_r32, by = 'team') %>%
  mutate(elim_prob = 1 - r32,
         prob = replace_na(prob, 0),
         logo = if_else(!is.na(opponent) & opponent != 'Other',
                        paste0(getwd(), '/flags/', opponent, '.png'),
                        NA_character_))

team_imgs_r32 <-
  active_teams_r32$team %>%
  map(function(tm) {
    tbl <- make_ko_mini_table(tm, filter(team_rows_r32, team == tm), 'Elim. Before R32')
    path <- paste0('figures/ko_mini_tables/r32/', tm, '.png')
    gtsave(tbl, path, vwidth = 600)
    image_read(path)
  })

names(team_imgs_r32) <- active_teams_r32$team

make_grid(team_imgs_r32, n_col = 8,
          'FIFA World Cup 2026 | Round of 32 Matchup Probabilities') %>%
  image_write('figures/simulation_tables/r32_matchups.png', format = 'png')

### Sub-grids by group triple (3 groups × 4 teams = 4 cols × 3 rows)
group_triples <- list(c('A','B','C'), c('D','E','F'), c('G','H','I'), c('J','K','L'))

for (grps in group_triples) {
  teams_in_grps <- active_teams_r32$team[active_teams_r32$group %in% grps]
  make_grid(team_imgs_r32[teams_in_grps], n_col = 4,
            paste0('FIFA World Cup 2026 | Groups ', paste(grps, collapse = ', '), ' | R32 Matchup Probabilities')) %>%
    image_write(paste0('figures/simulation_tables/r32_matchups_', paste(grps, collapse = ''), '.png'),
                format = 'png')
}

### ── R16 ─────────────────────────────────────────────────────────────────────
r16_games <- bind_rows(r16_results)

r16_opponent_probs <-
  bind_rows(r16_games %>% select(team = team1, opponent = team2),
            r16_games %>% select(team = team2, opponent = team1)) %>%
  group_by(team, opponent) %>%
  summarise(prob = n() / n_sims, .groups = 'drop') %>%
  arrange(team, desc(prob)) %>%
  group_by(team) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top3_r16 <- filter(r16_opponent_probs, rank <= 3)

other_r16 <-
  r16_opponent_probs %>%
  filter(rank > 3) %>%
  group_by(team) %>%
  summarise(prob = sum(prob), .groups = 'drop') %>%
  mutate(opponent = 'Other', rank = 4L)

active_teams_r16 <-
  sim_results %>%
  arrange(group, desc(r16)) %>%
  select(team, group, r32, r16)

team_rows_r16 <-
  bind_rows(top3_r16, other_r16) %>%
  right_join(active_teams_r16, by = 'team') %>%
  mutate(elim_prob = 1 - r16,
         prob = replace_na(prob, 0),
         logo = if_else(!is.na(opponent) & opponent != 'Other',
                        paste0(getwd(), '/flags/', opponent, '.png'),
                        NA_character_))

team_imgs_r16 <-
  active_teams_r16$team %>%
  map(function(tm) {
    tbl <- make_ko_mini_table(tm, filter(team_rows_r16, team == tm), 'Elim. Before R16')
    path <- paste0('figures/ko_mini_tables/r16/', tm, '.png')
    gtsave(tbl, path, vwidth = 600)
    image_read(path)
  })

make_grid(team_imgs_r16, n_col = 8,
          'FIFA World Cup 2026 | Round of 16 Matchup Probabilities') %>%
  image_write('figures/simulation_tables/r16_matchups.png', format = 'png')
