### Update Scores
source('update_scores.R')

### Re-fit model after each matchweek and after R32/R16
if(as.character(Sys.Date()) %in% c('2026-06-17', '2026-06-23', '2026-06-27',
                                   '2026-07-04', '2026-07-08')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
source('run_sim.R')

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')
