library(XML)
library(RCurl)
library(tidyverse)

get_scores <- function(date, schedule) {
  print(date)
  date_ <- gsub('-', '', date)
  url <- paste0('https://www.espn.com/soccer/fixtures/_/date/', date_, '/league/fifa.world')
  raw <- tryCatch(readHTMLTable(getURL(url)), error = function(e) NULL)
  if(is.null(raw) || length(raw) == 0) return(NULL)

  scores <- raw[[1]]

  penalties_ix <- which(str_detect(scores[, 2], 'FT-Pens'))

  tms <- unique(c(schedule$team1, schedule$team2))
  tms <- tms[!is.na(tms)]

  df <-
    tibble('date' = as.Date(date_, '%Y%m%d'),
           'team1_score' = map_dbl(str_extract_all(scores[, 2], '\\d+'), ~as.numeric(.x[1])),
           'team2_score' = map_dbl(str_extract_all(scores[, 2], '\\d+'), ~as.numeric(.x[2])),
           'team1' = gsub('.*\\s+.\\s+..', '', gsub('\\s[A-Z]+\\d+.*$', '', scores[, 1])),
           'team2' = gsub('.*\\s+.\\s+..', '', gsub('\\s[A-Z]+$', '', scores[, 2])),
           'shootout_winner' = NA)

  if(length(penalties_ix) > 0) {
    penalties_winners <-
      map_chr(scores[penalties_ix, 1], function(x) {
        winner <- tms[map_lgl(tms, ~grepl(paste(.x, '(win|advance) \\d+-\\d+ on penalties'), x))]
        if(length(winner) == 0) return(NA_character_)
        winner
      })
    df$shootout_winner[penalties_ix] <- penalties_winners
    df$team1[penalties_ix] <-
      gsub('\\s+(win|advance) \\d+-\\d+ on penalties', '', df$team1[penalties_ix])
    for(t in tms) {
      df$team1[penalties_ix] <-
        map_chr(df$team1[penalties_ix], ~ifelse(strsplit(.x, t)[[1]][1] == '', t, .x))
    }
  }

  df <-
    bind_rows(df, select(df, date,
                         'team2' = team1, 'team1' = team2,
                         'team1_score' = team2_score, 'team2_score' = team1_score,
                         shootout_winner))

  return(df)
}

### Read In Schedule
schedule <-
  read_csv('data/schedule.csv', show_col_types = F) %>%
  mutate('date' = as.Date(date))

### Scrape all dates from tournament start through today
tournament_dates <- seq.Date(as.Date('2026-06-11'), max(as.Date('2026-06-11'), Sys.Date()), by = 1)
scores <- map_dfr(tournament_dates, ~get_scores(.x, schedule))

### For KO rounds: fill in team names from scores before joining
ko_games <-
  scores %>%
  filter(team1 %in% c(schedule$team1, schedule$team2),
         team2 %in% c(schedule$team1, schedule$team2)) %>%
  distinct() %>%
  filter(team1 > team2)

schedule$team1[!is.na(schedule$ko_round) & is.na(schedule$team1_score)] <- NA
schedule$team2[!is.na(schedule$ko_round) & is.na(schedule$team2_score)] <- NA

for(i in seq_len(nrow(ko_games))) {
  ix_game <- min(which(schedule$date == ko_games$date[i] & is.na(schedule$team1)))
  if(!is.infinite(ix_game)) {
    schedule$team1[ix_game] <- ko_games$team1[i]
    schedule$team2[ix_game] <- ko_games$team2[i]
  }
}

### Update Scores
schedule <-
  schedule %>%
  select(-contains('score'), -contains('shootout_winner')) %>%
  left_join(scores, by = c('date', 'team1', 'team2'))

write_csv(schedule, 'data/schedule.csv')
