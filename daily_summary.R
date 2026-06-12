library(tidyverse)
library(gt)
library(gtExtras)
source('helpers.R')

dir.create('figures/daily_summary', showWarnings = FALSE)

history <-
  read_csv('predictions/history.csv', show_col_types = F) %>%
  mutate('date' = as.Date(date))

schedule <-
  read_csv('data/schedule.csv', show_col_types = F) %>%
  mutate('date' = as.Date(date))

### Use the two most recent dates in history
dates <- sort(unique(history$date))
run_date <- max(dates)
today <- filter(history, date == run_date)

### Teams that played today — exit quietly if none
played_today <-
  schedule %>%
  filter(date == run_date, !is.na(team1_score)) %>%
  { union(.$team1, .$team2) }

if(length(played_today) == 0) {
  message('daily_summary.R: no completed games on ', run_date, ', skipping.')
} else {
  
  ### Compute deltas vs. previous date (NA on first day)
  if(length(dates) >= 2) {
    yesterday <-
      filter(history, date == dates[length(dates) - 1]) %>%
      select(team, r32_prev = r32, r16_prev = r16)
    today <-
      today %>%
      left_join(yesterday, by = 'team') %>%
      mutate(
        r32_delta = r32 - r32_prev,
        r16_delta = r16 - r16_prev
      )
  } else {
    today <- mutate(today, r32_delta = NA_real_, r16_delta = NA_real_)
  }
  
  ### Section 1: teams that played today
  section1 <-
    today %>%
    filter(team %in% played_today) %>%
    arrange(group, team) %>%
    mutate('logo' = file.path(getwd(), paste0('flags/', team, '.png')),
           'section' = 'Teams That Played') %>%
    select(section, logo, team, group, r32, r32_delta, r16, r16_delta)
  
  ### Section 2: significant movers who didn't play (|r32 delta| >= 5%)
  section2 <-
    today %>%
    filter(!team %in% played_today, abs(r32_delta) >= 0.05) %>%
    arrange(desc(abs(r32_delta))) %>%
    mutate('logo' = file.path(getwd(), paste0('flags/', team, '.png')),
           'section' = 'Significant Movers (Did Not Play)') %>%
    select(section, logo, team, group, r32, r32_delta, r16, r16_delta)
  
  df_table <-
    if(nrow(section2) > 0) {
      bind_rows(section1, section2)
    } else {
      section1
    }
  
  ### Format delta as small bold colored HTML
  fmt_delta <- function(x) {
    map(as.numeric(x), function(val) {
      if(is.na(val)) {
        return(html(''))
      }
      color <- if(val >= 0) '#27AE60' else '#E74C3C'
      sign <- if(val >= 0) '+' else ''
      html(sprintf('<span style="color:%s; font-size:0.8em; font-weight:bold;">%s%.1f%%</span>',
                   color, sign, val * 100))
    })
  }
  
  tbl <-
    df_table %>%
    gt(groupname_col = 'section') %>%
    cols_align('center') %>%
    text_transform(
      locations = cells_body(columns = logo),
      fn = function(x) map_chr(x, ~local_image(filename = as.character(.x), height = 25))
    ) %>%
    fmt_percent(columns = c(r32, r16), decimals = 1) %>%
    fmt(columns = c(r32_delta, r16_delta), fns = fmt_delta) %>%
    data_color(
      columns = c(r32, r16),
      fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))
    ) %>%
    tab_spanner(label = 'R32 Chances', columns = c(r32, r32_delta)) %>%
    tab_spanner(label = 'R16 Chances', columns = c(r16, r16_delta)) %>%
    tab_style(
      style = cell_borders(sides = 'bottom', color = 'black', weight = px(3)),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_borders(sides = 'right', color = 'black', weight = px(3)),
      locations = cells_body(columns = r32_delta)
    ) %>%
    cols_label(
      logo = '', team = 'Team', group = 'Group',
      r32 = 'Current', r32_delta = 'Change',
      r16 = 'Current', r16_delta = 'Change'
    ) %>%
    tab_header(
      title = md('**FIFA World Cup 2026**'),
      subtitle = md(paste0('**Daily Update — ', format(run_date, '%B %d, %Y'), '**'))
    ) %>%
    tab_source_note('Luke Benz (@recspecs730)') %>%
    tab_source_note('Based on 10,000 Simulations') %>%
    tab_options(
      column_labels.font.size = 16,
      heading.title.font.size = 30,
      heading.subtitle.font.size = 22,
      heading.title.font.weight = 'bold',
      heading.subtitle.font.weight = 'bold',
      column_labels.font.weight = 'bold',
      row_group.font.weight = 'bold',
      row_group.font.size = 14
    )
  
  gtsave_extra(tbl, paste0('figures/daily_summary/daily_summary_', run_date, '.png'), vwidth = 900)
}
