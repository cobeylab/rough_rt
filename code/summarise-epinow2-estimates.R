## Summarize epinow2 estimates
summarise_all_estimates <- function(
  path, 
  dt, #Late date in time series
  tooday, # Date on which analysis is run
  outpath = NULL # Optional path for outputs
){
  
  library(optparse)
  ## Read in options from midway
  option_list = list(make_option("--debug", type = "logical", default = FALSE, help = 'optional outpath spec for testing. Default is epinow2_cli_estimates/')); 
  opt_parser = OptionParser(option_list=option_list);
  opt = parse_args(opt_parser); # Now you have a list called "opt" with elements opt$var and opt$out
  
  
  dirs <- list.files(path)
  dirs <- dirs[!grepl(dirs, pattern = '(\\d\\d\\d\\d-\\d\\d-\\d\\d)')] # Drop the directories named for dates, not regions
  
  if(opt$debug==TRUE){
    full_outpath =  sprintf('%s/full_outputs.csv', path)
    rt_outpath =    sprintf('%s/Rt_outputs.csv', path)
  }else{
    full_outpath =  sprintf('../figs/cli_nadmit_%s/full_outupts_epinow2_%s.csv', tooday, dt)
    rt_outpath =    sprintf('../figs/cli_nadmit_%s/Rt_outupts_epinow2_%s.csv', tooday, dt)
  }
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
  write_csv(fulldf, path = full_outpath)
  write_csv(Rtdf, path = rt_outpath)
  cat(sprintf('.csv outputs written to %s', rt_outpath))
}

