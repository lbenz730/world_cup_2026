library(XML)
library(RCurl)
library(tidyverse)

### ESPN renders team names with encoding artifacts and occasionally uses different
### spellings than the schedule. Map known mismatches here.
espn_name_map <- c(
  'Bosnia-Herzegovina' = 'Bosnia and Herzegovina',
  'Congo DR'           = 'DR Congo',
  'Cura ao'            = 'Curaçao',
  'T rkiye'            = 'Turkiye'
)

get_scores <- function(date, schedule) {
  print(date)
  date_ <- gsub('-', '', date)
  url <- paste0('https://www.espn.com/soccer/fixtures/_/date/', date_, '/league/fifa.world')
  raw <- tryCatch(readHTMLTable(getURL(url)), error = function(e) NULL)
  if(is.null(raw) || length(raw) == 0) return(NULL)

  tms <- unique(c(schedule$team1, schedule$team2))
  tms <- tms[!is.na(tms)]

  ### Strip non-ASCII encoding artifacts (e.g. "Â ") and normalize whitespace
  clean_str <- function(x) {
    trimws(gsub('\\s+', ' ', gsub('[^a-zA-Z0-9 \\-]', ' ', x)))
  }

  fix_names <- function(x) {
    mapped <- espn_name_map[x]
    ifelse(is.na(mapped), x, mapped)
  }

  ### Parse one HTML table. ESPN column structure (consistent for all game states):
  ###   V1 = team1 name  ("Mexico", "South Korea")
  ###   V2 = " v {team2}" for upcoming, or " {score} {team2}" for completed
  ###   V3 = kickoff time for upcoming, "FT" or "FT-Pens" for completed
  parse_table <- function(tbl) {
    if(is.null(tbl) || ncol(tbl) < 2 || nrow(tbl) == 0) return(NULL)
    tryCatch({
      v1 <- clean_str(as.character(tbl[, 1]))
      v2 <- clean_str(as.character(tbl[, 2]))

      completed <- str_detect(v2, '\\d+ - \\d+')

      n <- nrow(tbl)
      team1           <- v1
      team2           <- rep(NA_character_, n)
      team1_score     <- rep(NA_real_, n)
      team2_score     <- rep(NA_real_, n)
      shootout_winner <- rep(NA_character_, n)

      # Completed: V2 = "2 - 0 South Africa"
      if(any(completed)) {
        sc <- str_extract(v2[completed], '\\d+ - \\d+')
        team1_score[completed] <- as.numeric(str_extract(sc, '^\\d+'))
        team2_score[completed] <- as.numeric(str_extract(sc, '\\d+$'))
        team2[completed] <- trimws(str_replace(v2[completed], '^.*\\d+ - \\d+\\s*', ''))
      }

      # Upcoming: V2 = "v Czechia"
      if(any(!completed)) {
        team2[!completed] <- trimws(str_replace(v2[!completed], '^v\\s+', ''))
      }

      # FT-Pens: check V3 if available
      if(ncol(tbl) >= 3) {
        v3 <- trimws(as.character(tbl[, 3]))
        penalties_ix <- which(str_detect(v3, 'FT-Pens'))
        if(length(penalties_ix) > 0) {
          shootout_winner[penalties_ix] <- map_chr(v2[penalties_ix], function(x) {
            winner <- tms[map_lgl(tms, ~grepl(paste0(.x, '.*(win|advance).*on penalties'), x))]
            if(length(winner) == 0) NA_character_ else winner[1]
          })
        }
      }

      df <-
        tibble(date = as.Date(date_, '%Y%m%d'),
               team1 = fix_names(team1), team2 = fix_names(team2),
               team1_score, team2_score, shootout_winner) %>%
        filter(!is.na(team1), !is.na(team2), team1 != '', team2 != '')

      if(nrow(df) == 0) return(NULL)

      bind_rows(df, select(df, date,
                           'team2' = team1, 'team1' = team2,
                           'team1_score' = team2_score, 'team2_score' = team1_score,
                           shootout_winner))
    }, error = function(e) NULL)
  }

  ### Process all tables, prefer rows with actual scores over NA placeholders
  map_dfr(raw, parse_table) %>%
    arrange(is.na(team1_score)) %>%
    distinct(date, team1, team2, .keep_all = TRUE)
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
