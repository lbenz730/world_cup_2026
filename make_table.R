library(tidyverse)
library(gt)
dir.create('figures/simulation_tables', showWarnings = FALSE)

df_stats <-
  read_csv('predictions/ratings.csv', show_col_types = F) %>%
  select(team, alpha, delta, net_rating) %>%
  inner_join(read_csv('predictions/sim_results.csv', show_col_types = F), by = 'team') %>%
  arrange(group, desc(r32), desc(r16), desc(qf), desc(sf), desc(finals), desc(champ)) %>%
  mutate('logo' = paste0('flags/', team, '.png')) %>%
  select(team, logo, group, alpha, delta, net_rating, mean_pts, mean_gd,
         r32, r16, qf, sf, finals, champ)

make_table <- function(Group = 'all') {
  title <- html(paste0("<img src='", file.path(getwd(), 'flags/fifa_logo.jpg'), "' height='160'>"))

  if(Group == 'all') {
    df1 <-
      df_stats %>%
      filter(group <= 'F') %>%
      group_by(group)
    names(df1) <- paste0(names(df1), '_1')

    df2 <-
      df_stats %>%
      filter(group > 'F') %>%
      group_by(group)
    names(df2) <- paste0(names(df2), '_2')

    bind_cols(df1, df2) %>%
      ungroup() %>%
      gt() %>%

      fmt_number(columns = c(alpha_1, delta_1, net_rating_1, mean_pts_1, mean_gd_1), decimals = 2, sep_mark = '') %>%
      fmt_number(columns = c(alpha_2, delta_2, net_rating_2, mean_pts_2, mean_gd_2), decimals = 2, sep_mark = '') %>%
      fmt_percent(columns = c(r32_1, r16_1, qf_1, sf_1, finals_1, champ_1), decimals = 0, sep_mark = '') %>%
      fmt_percent(columns = c(r32_2, r16_2, qf_2, sf_2, finals_2, champ_2), decimals = 0, sep_mark = '') %>%

      cols_align(align = "center") %>%

      data_color(columns = c(mean_pts_1, mean_pts_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>%
      data_color(columns = c(mean_gd_1, mean_gd_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>%
      data_color(columns = c(r32_1, r16_1, qf_1, sf_1, finals_1, champ_1,
                              r32_2, r16_2, qf_2, sf_2, finals_2, champ_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
      data_color(columns = c(alpha_1, alpha_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>%
      data_color(columns = c(net_rating_1, net_rating_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>%
      data_color(columns = c(delta_1, delta_2),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>%

      tab_style(
        style = list(
          cell_borders(sides = "bottom", color = "black", weight = px(3))
        ),
        locations = list(
          cells_column_labels(columns = gt::everything())
        )
      ) %>%
      tab_style(
        style = list(
          cell_borders(sides = "bottom", color = "black", weight = px(3))
        ),
        locations = list(
          cells_body(rows = c(4, 8, 12, 16, 20, 24))
        )
      ) %>%
      tab_style(
        style = list(
          cell_borders(sides = "right", color = "black", weight = px(3))
        ),
        locations = list(
          cells_body(columns = c(group_1, net_rating_1, mean_gd_1, champ_1,
                                  group_2, net_rating_2, mean_gd_2))
        )
      ) %>%

      tab_spanner(label = 'Ratings', columns = c('alpha_1', 'delta_1', 'net_rating_1'), id = '1') %>%
      tab_spanner(label = 'Ratings', columns = c('alpha_2', 'delta_2', 'net_rating_2'), id = '2') %>%
      tab_spanner(label = 'Group Stage', columns = c('mean_pts_1', 'mean_gd_1'), id = '3') %>%
      tab_spanner(label = 'Group Stage', columns = c('mean_pts_2', 'mean_gd_2'), id = '4') %>%
      tab_spanner(label = 'Knockout Round', columns = c('r32_1', 'r16_1', 'qf_1', 'sf_1', 'finals_1', 'champ_1'), id = '5') %>%
      tab_spanner(label = 'Knockout Round', columns = c('r32_2', 'r16_2', 'qf_2', 'sf_2', 'finals_2', 'champ_2'), id = '6') %>%

      text_transform(
        locations = cells_body(columns = c(logo_1, logo_2)),
        fn = function(x) {
          local_image(
            filename = ifelse(is.na(x), '--', as.character(x)),
            height = 30
          )
        }
      ) %>%

      cols_label(
        team_1 = '', logo_1 = '', group_1 = 'Group',
        alpha_1 = 'Offense', delta_1 = 'Defense', net_rating_1 = 'Overall',
        mean_pts_1 = 'Points', mean_gd_1 = 'GD',
        r32_1 = 'R32', r16_1 = 'R16', qf_1 = 'QF', sf_1 = 'SF', finals_1 = 'Finals', champ_1 = 'Champ',
        team_2 = '', logo_2 = '', group_2 = 'Group',
        alpha_2 = 'Offense', delta_2 = 'Defense', net_rating_2 = 'Overall',
        mean_pts_2 = 'Points', mean_gd_2 = 'GD',
        r32_2 = 'R32', r16_2 = 'R16', qf_2 = 'QF', sf_2 = 'SF', finals_2 = 'Finals', champ_2 = 'Champ'
      ) %>%
      tab_source_note("Luke Benz (@recspecs730)") %>%
      tab_source_note("Ratings = Change in Log Goal Expectations") %>%
      tab_source_note("Based on 10,000 Simulations") %>%
      tab_source_note("Data: github.com/martj42/international_results | Country Images: Flaticon.com") %>%
      tab_header(
        title = title,
        subtitle = md('**World Cup 2026 Simulations**')
      ) %>%
      tab_options(
        column_labels.font.size = 20,
        row_group.font.weight = 'bold',
        heading.title.font.size = 40,
        heading.subtitle.font.size = 30,
        heading.title.font.weight = 'bold',
        heading.subtitle.font.weight = 'bold',
        column_labels.font.weight = 'bold'
      )
  } else {
    df_stats %>%
      filter(group == Group) %>%
      gt() %>%

      fmt_number(columns = c(alpha, delta, net_rating, mean_pts, mean_gd), decimals = 2, sep_mark = '') %>%
      fmt_percent(columns = c(r32, r16, qf, sf, finals, champ), decimals = 0, sep_mark = '') %>%

      cols_align(align = "center") %>%

      data_color(columns = c(mean_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>%
      data_color(columns = c(mean_gd),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>%
      data_color(columns = c(r32, r16, qf, sf, finals, champ),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
      data_color(columns = c(alpha),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>%
      data_color(columns = c(net_rating),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>%
      data_color(columns = c(delta),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>%

      tab_style(
        style = list(
          cell_borders(sides = "bottom", color = "black", weight = px(3))
        ),
        locations = list(
          cells_column_labels(columns = gt::everything())
        )
      ) %>%
      tab_style(
        style = list(
          cell_borders(sides = "right", color = "black", weight = px(3))
        ),
        locations = list(
          cells_body(columns = c(net_rating, mean_gd))
        )
      ) %>%
      tab_style(
        style = list(
          cell_borders(sides = "left", color = "black", weight = px(3))
        ),
        locations = list(
          cells_body(columns = c(alpha))
        )
      ) %>%

      tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>%
      tab_spanner(label = 'Group Stage', columns = c('mean_pts', 'mean_gd')) %>%
      tab_spanner(label = 'Knockout Round', columns = c('r32', 'r16', 'qf', 'sf', 'finals', 'champ')) %>%

      text_transform(
        locations = cells_body(columns = "logo"),
        fn = function(x) map_chr(x, ~{
          local_image(filename = as.character(.x), height = 30)
        })
      ) %>%

      cols_label(
        team = '', logo = '', group = 'Group',
        alpha = 'Offense', delta = 'Defense', net_rating = 'Overall',
        mean_pts = 'Points', mean_gd = 'GD',
        r32 = 'R32', r16 = 'R16', qf = 'QF', sf = 'SF', finals = 'Finals', champ = 'Champ'
      ) %>%
      tab_source_note("Luke Benz (@recspecs730)") %>%
      tab_source_note("Ratings = Change in Log Goal Expectations") %>%
      tab_source_note("Based on 10,000 Simulations") %>%
      tab_source_note("Data: github.com/martj42/international_results | Country Images: Flaticon.com") %>%
      tab_header(
        title = title,
        subtitle = md(paste0('**World Cup 2026 Simulations: Group ', Group, '**'))
      ) %>%
      tab_options(
        column_labels.font.size = 20,
        row_group.font.weight = 'bold',
        row_group.font.size = 20,
        heading.title.font.size = 40,
        heading.subtitle.font.size = 30,
        heading.title.font.weight = 'bold',
        heading.subtitle.font.weight = 'bold',
        column_labels.font.weight = 'bold'
      )
  }
}

gtExtras::gtsave_extra(make_table('all'), filename = 'figures/simulation_tables/world_cup_2026.png', vwidth = 2000)
map(LETTERS[1:12], ~gtExtras::gtsave_extra(make_table(Group = .x), filename = paste0('figures/simulation_tables/', .x, '.png')))
