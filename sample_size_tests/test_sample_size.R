## Get dates where true rt crosses 1
get_rt_cross_1_dates <- function(sim_df){
  sim_df %>% 
    filter((true_rt<1) != (lag(true_rt)<1))
}


## Given synthetic data and delays to observation, generate time series of observations
get_obs_ts <- function(true_delay_pars,
                       mintime = 100, 
                       maxtime = 250,
                       st.date = '2020-06-15'){
  sim_df <- get_sim_df('stochastic')
  sim_df$incidence <- na_to_0(sim_df$incidence)
  stopifnot('incidence' %in% colnames(sim_df))
  
  ## 1. Generate synthetic observations ----------
  ##     Set the delay to observation
  source('../code/infer_times_of_infection_observation.R')
  # ## Write a function to draw delays for each individual in the data
  rdelay <- function(nn){rlnorm(nn, true_delay_pars[1], true_delay_pars[2])}
  ## Convolve to get synthetic times of observation
  sim_df <-  merge(
    sim_df %>% select(time, incidence, true_rt),
    get_tObs_from_tInf(n_dS = sim_df$incidence, 
                       times = sim_df$time, 
                       r_delay_dist = rdelay, 
                       return_times = T) %>% rename(delayed = n),
    by = 'time') %>%
    filter(time >= mintime & time <= maxtime) %>%
    mutate(date = as.Date(st.date)+(0:(nrow(.)-1)))
}









test_sample_size_end_date <- function(p_obs = 1, 
                             median_cases_per_day = NULL, 
                             nboot = 25,
                             last_obs_time = 210,
                             true_delay_pars,
                             min_window = 1
){
  sim_df <- get_obs_ts(true_delay_pars)
  source('../code/util.R')
  stopifnot(length(p_obs) == 0 | (p_obs <= 1 & p_obs >= 0))
  stopifnot((length(p_obs)>0)|length(median_cases_per_day)>0)
  
  
  ## Truncate to last observed time
  sim_df <- sim_df %>% 
    rename(cases = delayed) %>%
    filter(time <= last_obs_time) 

  ## 1.b Downsample to the desired probabilty of observation or sample size ----------
  if(length(median_cases_per_day)>0){
    raw = median(sim_df$delayed)
    p_obs <- median_cases_per_day/raw
  }
  sim_df$obs = sapply(sim_df$delayed, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs)) #+ min_0(rnorm(nrow(sim_df)))
  cat(sprintf('Setting p_obs=%.3f to obtain a median of %1.0f cases per day', p_obs, median(sim_df$obs)))

  

  delay_mean = exp(true_delay_pars[1]+(true_delay_pars[2]^2)/2)
  
  ## Get Rt ests
  source('../code/cori.R')
  source('../code/upscale.R')
  source('../code/rt_boot.R')
  source('../code/rt_pipeline.R')
  rt_ests <- upscale_cori_pipeline(df = sim_df, 
                                   obscolname = 'obs',
                                   p_obs = p_obs,
                                   delay_mean = delay_mean,
                                   gen_int_pars = c(mean = parlist$true_mean_GI, var = parlist$true_var_GI), 
                                   nboot = 50, 
                                   ttl = '', 
                                   obs_type = 'cases',
                                   min_window = min_window)
  
  ## Return
  # list(df = merge(sim_df, 
  #       rt_ests$df %>% select(date, contains('rt.')), 
  #       by = 'date'),
  #      plot = rt_ests$rt_plot
  # )
  merge(sim_df, 
        rt_ests$df %>% select(date, contains('rt.')), 
        by = 'date') %>%
    mutate(last_day = last_obs_time, median_cases_per_day = median_cases_per_day)
}

