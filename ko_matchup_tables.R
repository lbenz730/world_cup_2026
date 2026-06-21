### KO Round Opponent Mini-Tables
library(tidyverse)
library(gt)
library(magick)
library(ggsci)

r32_results <- read_rds('predictions/sim_rds/r32_results.rds')
sim_results <- read_csv('predictions/sim_results.csv', show_col_types = F)
n_sims <- length(r32_results)

r32_games <- bind_rows(r32_results)

r32_opponent_probs <-
  bind_rows(
    r32_games %>% select(team = team1, opponent = team2),
    r32_games %>% select(team = team2, opponent = team1)
  ) %>%
  group_by(team, opponent) %>%
  summarise(prob = n() / n_sims, .groups = 'drop') %>%
  arrange(team, desc(prob)) %>%
  group_by(team) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top3 <-
  r32_opponent_probs %>%
  filter(rank <= 3)

other <-
  r32_opponent_probs %>%
  filter(rank > 3) %>%
  group_by(team) %>%
  summarise(prob = sum(prob), .groups = 'drop') %>%
  mutate(opponent = 'Other', rank = 4L)

active_teams <-
  sim_results %>%
  arrange(group, desc(r32)) %>%
  select(team, group, r32)

team_rows <-
  bind_rows(top3, other) %>%
  right_join(active_teams, by = 'team') %>%
  mutate(
    elim_prob = 1 - r32,
    prob = replace_na(prob, 0),
    logo = if_else(
      !is.na(opponent) & opponent != 'Other',
      paste0(getwd(), '/flags/', opponent, '.png'),
      NA_character_
    )
  )

make_r32_mini_table <- function(team_name, df) {
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
      tibble(logo = NA_character_, label = 'Elim. Before R32', prob = 1.0) %>%
        gt() %>%
        tab_header(title = header) %>%
        text_transform(
          locations = cells_body(columns = logo),
          fn = function(x) ''
        ) %>%
        fmt_percent(columns = prob, decimals = 1) %>%
        tab_style(
          style = list(cell_fill(color = '#e8e8e8'), cell_text(style = 'italic')),
          locations = cells_body(rows = 1)
        ) %>%
        tab_style(
          style = cell_fill(color = '#e8e8e8'),
          locations = cells_title()
        ) %>%
        cols_label(logo = '', label = '', prob = '') %>%
        cols_align(align = 'left', columns = label) %>%
        cols_align(align = 'right', columns = prob) %>%
        tab_options(
          table.font.size = 12,
          heading.title.font.size = 13,
          heading.title.font.weight = 'bold',
          data_row.padding = px(3),
          table.border.top.style = 'solid',
          table.border.top.color = 'black',
          table.width = px(300)
        )
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
    tibble(
      logo = c(top_opps$logo, rep(NA_character_, n_pad), NA, NA),
      label = c(top_opps$opponent, rep('—', n_pad), 'Other', 'Elim. Before R32'),
      prob = c(top_opps$prob, rep(0, n_pad), other_prob, elim_prob)
    )

  rows %>%
    gt() %>%
    tab_header(title = header) %>%
    text_transform(
      locations = cells_body(columns = logo),
      fn = function(x) {
        map_chr(x, ~ {
          if (is.na(.x)) {
            ''
          } else {
            local_image(.x, height = 16)
          }
        })
      }
    ) %>%
    fmt_percent(columns = prob, decimals = 1) %>%
    data_color(
      columns = prob,
      rows = 1:4,
      fn = scales::col_numeric(palette = rgb_material('amber', n = 100), domain = c(0, 1))
    ) %>%
    tab_style(
      style = list(cell_fill(color = '#e8e8e8'), cell_text(style = 'italic')),
      locations = cells_body(rows = 5)
    ) %>%
    cols_label(logo = '', label = '', prob = '') %>%
    cols_align(align = 'left', columns = label) %>%
    cols_align(align = 'right', columns = prob) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 13,
      heading.title.font.weight = 'bold',
      data_row.padding = px(3),
      table.border.top.style = 'solid',
      table.border.top.color = 'black',
      table.width = px(300)
    )
}

dir.create('figures/ko_mini_tables', showWarnings = FALSE)

team_imgs <-
  active_teams$team %>%
  map(function(tm) {
    df_team <- team_rows %>% filter(team == tm)
    tbl <- make_r32_mini_table(tm, df_team)
    path <- paste0('figures/ko_mini_tables/', tm, '.png')
    gtsave(tbl, path, vwidth = 600)
    image_read(path)
  })

max_h <- max(map_int(team_imgs, ~ image_info(.x)$height))
img_w <- image_info(team_imgs[[1]])$width

team_imgs <-
  map(team_imgs, ~ image_extent(.x, paste0(img_w, 'x', max_h), color = 'white', gravity = 'North'))

names(team_imgs) <- active_teams$team

n_col <- 8
ko_grid <-
  image_join(team_imgs) %>%
  image_montage(geometry = '+2+2', tile = paste0(n_col, 'x'), bg = 'white')

title_img <-
  image_blank(image_info(ko_grid)$width, 80, color = 'white') %>%
  image_annotate('FIFA World Cup 2026 | Round of 32 Matchup Probabilities',
                 gravity = 'Center',
                 size = 40,
                 weight = 700,
                 color = 'black')

ko_grid <-
  image_append(image_join(list(title_img, ko_grid)), stack = TRUE)
image_write(ko_grid, 'figures/simulation_tables/r32_matchups.png', format = 'png')

### Sub-grids by group triple (3 groups × 4 teams = 4 cols × 3 rows)
group_triples <- list(c('A','B','C'), c('D','E','F'), c('G','H','I'), c('J','K','L'))

for (grps in group_triples) {
  teams_in_grps <- active_teams$team[active_teams$group %in% grps]
  imgs_sub <-
    image_join(team_imgs[teams_in_grps]) %>%
    image_montage(geometry = '+2+2', tile = '4x', bg = 'white')

  title_sub <-
    image_blank(image_info(imgs_sub)$width, 80, color = 'white') %>%
    image_annotate(paste0('FIFA World Cup 2026 | Groups ', paste(grps, collapse = ', '), ' | R32 Matchup Probabilities'),
                   gravity = 'Center',
                   size = 40,
                   weight = 700,
                   color = 'black')

  image_append(image_join(list(title_sub, imgs_sub)), stack = TRUE) %>%
    image_write(paste0('figures/simulation_tables/r32_matchups_', paste(grps, collapse = ''), '.png'),
                format = 'png')
}
