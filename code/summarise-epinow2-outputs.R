## Summarize epinow2 estimates
summarise_all_estimates <- function(
  path, 
  dt, #Late date in time series
  tooday # Date on which analysis is run
){
  dirs <- list.files(path)
  dirs <- dirs[!grepl(dirs, pattern = '(\\d\\d\\d\\d-\\d\\d-\\d\\d)')] # Drop the directories named for dates, not regions
  #dirs <- dirs[!dirs %in% regions_to_exlude]
  ## Write a function to read in outputs from one region
  read_one_estimate <- function(rr, path, dt){
    if(file.exists(sprintf('%s/%s/%s/summarised_estimates.rds', path, rr, dt))){
      ss <- read_rds(sprintf('%s/%s/%s/summarised_estimates.rds', path, rr, dt))
    }else{
      ss <- NULL
    }
    return(ss)
  }
  fulldf <- lapply(dirs, function(rr) read_one_estimate(rr, path, dt)) %>%
    set_names(dirs) %>%
    bind_rows(.id = 'region')
  Rtdf <- filter(fulldf, variable=='R')
  dir_check(sprintf('../figs/cli_nadmit_%s/', tooday))
  write_csv(fulldf, path = sprintf('../figs/cli_nadmit_%s/full_outupts_epinow2_%s.csv', tooday, dt), col_names = T)
  write_csv(Rtdf, path = sprintf('../figs/cli_nadmit_%s/Rt_epinow2_%s.csv', tooday, dt),col_names = T)
  cat(sprintf('.csv outputs written to ../figs/cli_nadmit_%s/', tooday))
}
