### Update Scores
source('update_scores.R')

### Re-fit model after each matchweek and after R32/R16
if(as.character(Sys.Date()) %in% c('2026-06-17', '2026-06-23', '2026-06-27',
                                   '2026-07-03', '2026-07-07')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
source('run_sim.R')

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')

### Daily Summary Table
source('daily_summary.R')

### Knockout Table 
# source('ko_matchup_tables.R')

### Comparison Plot for R16
source('comparison_plot.R')
source('contest_standings_table.R')