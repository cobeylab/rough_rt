
run_epinow2 <- function(dat_df,  # List of parameters used to generate synthetic data
                        obs_colname, # Name of column holding observations
                        dat_type, # Can be 'cases', 'deaths' or 'hospitalizations'
                        prior_smoothing_window,
                        debug = FALSE,
                        output_folder = 'testEpiNow2')
{
  
  if(!dir.exists(output_folder)){dir.create(output_folder)}
  if(!dir.exists(paste0(output_folder, '/figs'))){dir.create(paste0(output_folder, '/figs'))}
  
  ## Set delay distributions for input into EpiNow2 -------------------------------------------
  incubation_period <- list(mean = EpiNow2::covid_incubation_period[1,]$mean, # gamma
                          mean_sd = EpiNow2::covid_incubation_period[1,]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1,]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1,]$sd_sd,
                          max = 30)
  
  generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                          mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                          sd = EpiNow2::covid_generation_times[1, ]$sd,
                          sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                          max = 30)

  
  case_rep_delay <- list(mean = .1, ## Very rough estimates based on carline 
                          mean_sd = .5, 
                          sd = 1,
                          sd_sd = .5,
                          max = 30)
  
  death_rep_delay <- list(mean = 2.86, ## From Linton et al. Table 2 (J. Clin. Med. 2020, 9, 538; doi:10.3390/jcm9020538)
                          mean_sd = 1.98,   
                          sd = 0.53,
                          sd_sd = 2.03,
                          max = 30)
  
  hospital_rep_delay <- list(mean = 2.21, ## Fitted to HK data, similar to Linton et al
                         mean_sd = 0.1, 
                         sd = 0.49,
                         sd_sd = 0.1,
                         max = 30)
  
  stopifnot(dat_type %in% c('cases', 'deaths', 'hospitalizations'))
  if(dat_type == 'cases')   delay <- case_rep_delay  
  if(dat_type == 'deaths')   delay <- death_rep_delay 
  if(dat_type == 'hospitalizations')   delay <- hospital_rep_delay 
    
  
  write_rds(generation_time, sprintf('%s/gen_interval.rds', output_folder))
  write_rds(incubation_period, sprintf('%s/incubation_pd.rds', output_folder))
  write_rds(delay, sprintf('%s/delay.rds', output_folder))


  # Plot the specified distributions
  png(sprintf('%s/figs/specified_distributions.png', output_folder), width = 7, height = 7, units = 'in', res = 300)
  par(mfrow = c(2,2))
  xx = seq(0, 30, by = 0.01)
  ## Reporting delay --
  plot(xx, dlnorm(xx, delay$mean, delay$sd), 
       type = 'l', main = 'assumed reporting delay', xlab = 'days', ylab = 'dens')
  ## Generation interval ---
  plot(xx, 
       dgamma(xx, shape = with(generation_time, get_shape(mean, sd^2)), 
              rate = with(generation_time, get_rate(mean, sd^2))), 
       type = 'l', main = 'assumed generation time', xlab = 'days', ylab = 'dens')
  ## Incubation period --
  plot(xx, 
       dlnorm(xx, incubation_period$mean, incubation_period$sd), 
       type = 'l', main = 'assumed incubation time', xlab = 'days', ylab = 'dens')
  dev.off()
  
  
  ## Input into inference model  -------------------------------------------
  ## Write a wrapper to reformat the desired synthetic data for input into epiEstim
  format_dat <- function(obs_colname, odf = dat_df){
    odf[,c('date', obs_colname)] %>%
      setNames(c('date', 'confirm')) %>%
      data.table::as.data.table() %>%
      return()
  }
  
  
  if(!debug){
    ## Fit to synthetic case observations
    rt_estimates <- EpiNow2::epinow(reported_cases = format_dat(obs_colname, dat_df), 
                                      generation_time = generation_time,
                                      delays = list(reporting_delay = delay,
                                                    incubation_period = incubation_period), 
                                      prior_smoothing_window = prior_smoothing_window,
                                      rt_prior = list(mean = 2, sd = 1), horizon = 0,
                                      samples = 2000, warmup = 500, cores = 4,
                                      chains = 4, verbose = TRUE,
                                      target_folder = paste0(output_folder))
  }
}