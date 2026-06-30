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
  ###   V2 = " v {team2}" for upcoming, or " {score} {team2}" for in-progress/completed
  ###   V3 = kickoff time for upcoming, "FT"/"FT-Pens" for completed, "45'" etc for live
  parse_table <- function(tbl) {
    if(is.null(tbl) || ncol(tbl) < 2 || nrow(tbl) == 0) return(NULL)
    tryCatch({
      v1 <- clean_str(as.character(tbl[, 1]))
      v2 <- clean_str(as.character(tbl[, 2]))
      ### Default v3 to "FT" so score-bearing rows without a V3 column are treated as final
      v3 <- if(ncol(tbl) >= 3) trimws(as.character(tbl[, 3])) else rep('FT', nrow(tbl))

      ### ESPN appends "Team advance X-Y on penalties" as a V1-only footnote row (V2/V3 = NA)
      ### after FT-Pens games. Extract the shootout winner from V1 of those rows before
      ### dropping them — otherwise str_detect(NA, ...) propagates NA into any() and crashes.
      pre_filter_sw <- rep(NA_character_, nrow(tbl))
      for(i in seq_len(nrow(tbl))) {
        if(i > 1 && is.na(v2[i]) && isTRUE(str_detect(v3[i - 1], 'FT-Pens'))) {
          winner <- tms[map_lgl(tms, ~grepl(.x, v1[i], ignore.case = TRUE))]
          if(length(winner) > 0) pre_filter_sw[i - 1] <- winner[1]
        }
      }

      keep <- !is.na(v2)
      if(!any(keep)) return(NULL)
      v1 <- v1[keep]; v2 <- v2[keep]; v3 <- v3[keep]; pre_filter_sw <- pre_filter_sw[keep]

      has_score <- str_detect(v2, '\\d+\\s*-\\s*\\d+')
      completed <- has_score & str_detect(v3, '^FT')
      is_live   <- has_score & !completed

      n <- length(v1)
      team1           <- v1
      team2           <- rep(NA_character_, n)
      team1_score     <- rep(NA_real_, n)
      team2_score     <- rep(NA_real_, n)
      shootout_winner <- rep(NA_character_, n)

      # Completed (FT / FT-Pens): V2 = "2 - 0 South Africa"
      if(any(completed)) {
        sc <- str_extract(v2[completed], '\\d+\\s*-\\s*\\d+')
        team1_score[completed] <- as.numeric(str_extract(sc, '^\\d+'))
        team2_score[completed] <- as.numeric(str_extract(sc, '\\d+$'))
        team2[completed] <- trimws(str_replace(v2[completed], '^.*\\d+\\s*-\\s*\\d+\\s*', ''))
      }

      # Live (in-progress): extract team names but no score recorded yet
      if(any(is_live)) {
        team2[is_live] <- trimws(str_replace(v2[is_live], '^.*\\d+\\s*-\\s*\\d+\\s*', ''))
      }

      # Upcoming: V2 = "v Czechia"
      if(any(!has_score)) {
        team2[!has_score] <- trimws(str_replace(v2[!has_score], '^v\\s+', ''))
      }

      # FT-Pens: determine shootout winner from footnote V1 (pre_filter_sw),
      # falling back to old-style text in V2 if not found there
      penalties_ix <- which(str_detect(v3, 'FT-Pens'))
      if(length(penalties_ix) > 0) {
        shootout_winner[penalties_ix] <- map_chr(penalties_ix, function(ix) {
          if(!is.na(pre_filter_sw[ix])) return(pre_filter_sw[ix])
          winner <- tms[map_lgl(tms, ~grepl(paste0(.x, '.*(win|advance).*on penalties'), v2[ix]))]
          if(length(winner) == 0) NA_character_ else winner[1]
        })
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
tournament_dates <- seq.Date(as.Date('2026-06-11'), Sys.Date(), by = 1)
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

### Group stage → R32: fill bracket from group standings
source('helpers.R')
df_combinations <- read_csv('data/third_place_combinations.csv', show_col_types = FALSE)

group_games <-
  schedule %>%
  filter(!is.na(group), !is.na(team1_score), !is.na(team2_score))

groups_complete <- group_games %>% count(group) %>% filter(n == 6) %>% pull(group)

if(length(groups_complete) == 12) {
  team_pts <-
    bind_rows(
      group_games %>%
        transmute(group, team = team1,
                  pts = case_when(team1_score > team2_score ~ 3L,
                                  team1_score == team2_score ~ 1L,
                                  TRUE ~ 0L),
                  gf = team1_score, ga = team2_score),
      group_games %>%
        transmute(group, team = team2,
                  pts = case_when(team2_score > team1_score ~ 3L,
                                  team1_score == team2_score ~ 1L,
                                  TRUE ~ 0L),
                  gf = team2_score, ga = team1_score)
    ) %>%
    group_by(group, team) %>%
    summarise(points = sum(pts), goals_scored = sum(gf), goal_diff = sum(gf) - sum(ga),
              .groups = 'drop')

  gsr <-
    team_pts %>%
    arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>%
    group_by(group) %>%
    mutate(place = row_number()) %>%
    ungroup() %>%
    mutate(progress = place <= 2)

  adv_3rd <-
    gsr %>%
    filter(place == 3) %>%
    arrange(desc(points), desc(goal_diff), desc(goals_scored)) %>%
    slice_head(n = 8) %>%
    pull(group)

  gsr <- mutate(gsr, progress = progress | (place == 3 & group %in% adv_3rd))

  bracket <- build_knockout_bracket(gsr)

  for(r in 1:16) {
    ix <- which(schedule$ko_round == paste('R32', r) & is.na(schedule$team1))
    if(length(ix) == 1) {
      schedule$team1[ix] <- bracket$team1[r]
      schedule$team2[ix] <- bracket$team2[r]
    }
  }
}

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

### Bracket propagation: fill future KO slots from completed-game winners/losers
ko_winner <- function(row) {
  if(is.na(row$team1_score) || is.na(row$team2_score)) {
    return(NA_character_)
  }
  if(!is.na(row$shootout_winner)) {
    return(row$shootout_winner)
  }
  if(as.numeric(row$team1_score) > as.numeric(row$team2_score)) row$team1 else row$team2
}

ko_loser <- function(row) {
  w <- ko_winner(row)
  if(is.na(w)) {
    return(NA_character_)
  }
  if(w == row$team1) row$team2 else row$team1
}

### R32 -> R16 (mirrors r16_t1/r16_t2 in run_sim.R)
r16_t1_src <- c(12, 3, 13, 2, 5, 11, 9, 1)
r16_t2_src <- c(8, 4, 14, 15, 6, 7, 10, 16)

for(g in 1:8) {
  r32_ix1 <- which(schedule$ko_round == paste('R32', r16_t1_src[g]))
  r32_ix2 <- which(schedule$ko_round == paste('R32', r16_t2_src[g]))
  r16_ix  <- which(schedule$ko_round == paste('R16', g))
  if(length(r32_ix1) == 1 && length(r32_ix2) == 1 && length(r16_ix) == 1) {
    w1 <- ko_winner(schedule[r32_ix1, ])
    w2 <- ko_winner(schedule[r32_ix2, ])
    if(!is.na(w1)) schedule$team1[r16_ix] <- w1
    if(!is.na(w2)) schedule$team2[r16_ix] <- w2
  }
}

### R16 -> QF (consecutive R16 pairs: 1v2, 3v4, 5v6, 7v8)
qf_t1_src <- c(1, 3, 5, 7)
qf_t2_src <- c(2, 4, 6, 8)

for(g in 1:4) {
  r16_ix1 <- which(schedule$ko_round == paste('R16', qf_t1_src[g]))
  r16_ix2 <- which(schedule$ko_round == paste('R16', qf_t2_src[g]))
  qf_ix   <- which(schedule$ko_round == paste('QF', g))
  if(length(r16_ix1) == 1 && length(r16_ix2) == 1 && length(qf_ix) == 1) {
    w1 <- ko_winner(schedule[r16_ix1, ])
    w2 <- ko_winner(schedule[r16_ix2, ])
    if(!is.na(w1)) schedule$team1[qf_ix] <- w1
    if(!is.na(w2)) schedule$team2[qf_ix] <- w2
  }
}

### QF -> SF (SF 1: W(QF1) v W(QF3); SF 2: W(QF2) v W(QF4))
sf_t1_src <- c(1, 2)
sf_t2_src <- c(3, 4)

for(g in 1:2) {
  qf_ix1 <- which(schedule$ko_round == paste('QF', sf_t1_src[g]))
  qf_ix2 <- which(schedule$ko_round == paste('QF', sf_t2_src[g]))
  sf_ix  <- which(schedule$ko_round == paste('SF', g))
  if(length(qf_ix1) == 1 && length(qf_ix2) == 1 && length(sf_ix) == 1) {
    w1 <- ko_winner(schedule[qf_ix1, ])
    w2 <- ko_winner(schedule[qf_ix2, ])
    if(!is.na(w1)) schedule$team1[sf_ix] <- w1
    if(!is.na(w2)) schedule$team2[sf_ix] <- w2
  }
}

### SF -> FINAL (W(SF1) v W(SF2)) and 3RD (L(SF1) v L(SF2))
sf1_ix   <- which(schedule$ko_round == 'SF 1')
sf2_ix   <- which(schedule$ko_round == 'SF 2')
final_ix <- which(schedule$ko_round == 'FINAL')
third_ix <- which(schedule$ko_round == '3RD')

if(length(sf1_ix) == 1 && length(sf2_ix) == 1) {
  if(length(final_ix) == 1) {
    w1 <- ko_winner(schedule[sf1_ix, ])
    w2 <- ko_winner(schedule[sf2_ix, ])
    if(!is.na(w1)) schedule$team1[final_ix] <- w1
    if(!is.na(w2)) schedule$team2[final_ix] <- w2
  }
  if(length(third_ix) == 1) {
    l1 <- ko_loser(schedule[sf1_ix, ])
    l2 <- ko_loser(schedule[sf2_ix, ])
    if(!is.na(l1)) schedule$team1[third_ix] <- l1
    if(!is.na(l2)) schedule$team2[third_ix] <- l2
  }
}

write_csv(schedule, 'data/schedule.csv')
