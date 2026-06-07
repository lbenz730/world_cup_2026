### Scrape the FIFA 2026 WC third-place combinations table from Wikipedia.
### Run this script once to generate data/third_place_combinations.csv.
### Requires: rvest, dplyr, readr, stringr

library(rvest)
library(dplyr)
library(readr)
library(stringr)

url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_knockout_stage"
page <- read_html(url)

tbl <- html_nodes(page, "table.wikitable")[[1]]
rows <- html_nodes(tbl, "tr")

### Skip header row (row 1). Parse each data row directly.
### Layout per row:
###   Col 1:    combination number
###   Cols 2-13: one cell per group A-L (empty if that group's 3rd doesn't advance)
###   Last 8:   slot assignments in order m74, m77, m79, m80, m81, m82, m85, m87
###             values are like "3E" (3rd place from group E)

parse_row <- function(row) {
  cells <- html_nodes(row, "td, th")
  texts <- html_text(cells, trim = TRUE)
  n <- length(texts)
  if(n < 9) return(NULL)

  ### Groups A-L are in positions 2:13 (cols 2 through 13)
  group_vals <- texts[2:13]
  groups <- paste(sort(group_vals[nchar(group_vals) == 1 & group_vals %in% LETTERS]), collapse = "")

  ### Slots are always the last 8 cells
  slot_vals <- str_remove(texts[(n - 7):n], "^3")

  tibble(
    groups = groups,
    m74 = slot_vals[1],
    m77 = slot_vals[2],
    m79 = slot_vals[3],
    m80 = slot_vals[4],
    m81 = slot_vals[5],
    m82 = slot_vals[6],
    m85 = slot_vals[7],
    m87 = slot_vals[8]
  )
}

df_combinations <-
  rows[-1] %>%
  lapply(parse_row) %>%
  bind_rows()

cat("Rows parsed:", nrow(df_combinations), "\n")
cat("Groups key lengths:", paste(sort(unique(nchar(df_combinations$groups))), collapse = ", "), "\n")
cat("Sample:\n")
print(head(df_combinations, 3))

write_csv(df_combinations, "data/third_place_combinations.csv")
cat("Saved", nrow(df_combinations), "combinations to data/third_place_combinations.csv\n")
