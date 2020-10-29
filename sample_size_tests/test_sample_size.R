## Get dates where true rt crosses 1
get_rt_cross_1_dates <- function(sim_df){
  sim_df %>% 
    filter((true_rt<1) != (lag(true_rt)<1))
}


## Given synthetic data and delays to observation, generate time series of observations
get_obs_ts <- function(rdelay,
                       mintime = 10, 
                       maxtime = 149,
                       st.date = '2020-06-15'){
  sim_df <- get_sim_df('stochastic')
  sim_df$incidence <- na_to_0(sim_df$incidence)
  stopifnot('incidence' %in% colnames(sim_df))
  
  ## 1. Generate synthetic observations ----------
  ##     Set the delay to observation
  source('../code/infer_times_of_infection_observation.R')
  # ## Write a function to draw delays for each individual in the data
  #rdelay <- function(nn){rlnorm(nn, true_delay_pars[1], true_delay_pars[2])}
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




# df = get_sim_df('stochastic') %>% mutate(date = as.Date('2020-02-01')+time)
# obscolname = 'obs_cases'
# gen_int_pars = c(8, 32)
# ttl = ''
# obs_type = 'cases'
# p_obs = 1
# median_cases_per_day = 50
# nboot = 5
# last_obs_time = 250
# rdelay <- function(nn){
#   rlnorm(nn, meanlog = 1.52, sdlog = sqrt(0.39)) %>% # Incubation period from table 2 of Linton et al.
#     ceiling()
# }
# delay_mean = mean(rdelay(10000))
# min_window = 2
# w.tune = 100


# p_obs = 1
# median_cases_per_day = 50
# nboot = 5
# last_obs_time = 250
# rdelay = rdelay
# min_window = 2
# w.tune = 100
# uncertainty = 'upscale_pipeline'



# p_obs = 0.3
# median_cases_per_day = NULL
# nboot = 5
# last_obs_time = tt
# rdelay = rdelay
# min_window = 1
# w.tune = 50
# uncertainty = 'ss_pipieline'





test_sample_size_end_date <- function(p_obs = 1, 
                                      median_cases_per_day = NULL, 
                                      nboot = 5,
                                      last_obs_time = 210,
                                      rdelay,
                                      min_window = 1,
                                      w.tune = 50,
                                      uncertainty  = 'ss_pipeline'
){
  sim_df <- get_obs_ts(rdelay)
  source('../code/util.R')
  stopifnot(length(p_obs) == 0 | (p_obs <= 1 & p_obs >= 0))
  stopifnot((length(p_obs)>0)|length(median_cases_per_day)>0)
  
  
  ## Truncate to last observed time
  sim_df <- sim_df %>% 
    rename(cases = delayed) %>%
    filter(time <= last_obs_time) 
  
  ## 1.b Downsample to the desired probabilty of observation or sample size ----------
  if(length(median_cases_per_day)>0){
    raw = median(sim_df$cases)
    p_obs <- min(1, median_cases_per_day/raw)
  }
  # sim_df$obs = sapply(sim_df$cases, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs)) #+ min_0(rnorm(nrow(sim_df)))

  
  
  
  delay_mean = mean(rdelay(10000))
  
  ## Get Rt ests
  source('../code/cori.R')
  source('../code/upscale.R')
  source('../code/rt_boot.R')
  ## Determine which pipeline to use
  if(uncertainty == 'ss_pipeline'){
    source('../code/ss_pipeline.R')
    rt_fun <- ss_pipeline
  }else if(uncertainty == 'upscale_pipeline'){
    source('../code/rt_pipeline.R')
    rt_fun <- upscale_cori_pipeline
    sim_df$cases = sapply(sim_df$cases, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs)) #+ min_0(rnorm(nrow(sim_df)))
    cat(sprintf('Setting p_obs=%1.3f to obtain a median of %1.0f cases per day\n', p_obs, median(sim_df$cases, na.rm = T)))
    #cat(sprintf('pobs is %1.2f\n', p_obs))
    cat(print(sim_df$cases))
  }else{
    error('invalid  uncertainty input. must be ss_pipeline or upscale_pipeline')
  }
  rt_ests <- rt_fun(df = sim_df, 
                                   obscolname = 'cases',
                                   p_obs = p_obs,
                                   delay_mean = delay_mean,
                                   gen_int_pars = c(mean = parlist$true_mean_GI, var = parlist$true_var_GI), 
                                   nboot = nboot, 
                                   ttl = '', 
                                   obs_type = 'cases',
                                   min_window = min_window,
                                   w.tune = w.tune)
  
  merge(sim_df, 
        rt_ests$df %>% select(date, contains('rt.')), 
        by = 'date') %>%
    mutate(last_day = last_obs_time, 
           median_cases_per_day = median_cases_per_day,
           delay_mean = delay_mean)
}






# test_sample_size_end_date <- function(p_obs = 1, 
#                              median_cases_per_day = NULL, 
#                              nboot = 5,
#                              last_obs_time = 210,
#                              rdelay,
#                              min_window = 1,
#                              w.tune = 50
# ){
#   sim_df <- get_obs_ts(rdelay)
#   source('../code/util.R')
#   stopifnot(length(p_obs) == 0 | (p_obs <= 1 & p_obs >= 0))
#   stopifnot((length(p_obs)>0)|length(median_cases_per_day)>0)
#   
#   
#   ## Truncate to last observed time
#   sim_df <- sim_df %>% 
#     rename(cases = delayed) %>%
#     filter(time <= last_obs_time) 
# 
#   ## 1.b Downsample to the desired probabilty of observation or sample size ----------
#   if(length(median_cases_per_day)>0){
#     raw = median(sim_df$cases)
#     p_obs <- median_cases_per_day/raw
#   }
#   sim_df$obs = sapply(sim_df$cases, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs)) #+ min_0(rnorm(nrow(sim_df)))
#   cat(sprintf('Setting p_obs=%.3f to obtain a median of %1.0f cases per day', p_obs, median(sim_df$obs)))
# 
#   
# 
#   delay_mean = mean(rdelay(10000))
#   
#   ## Get Rt ests
#   source('../code/cori.R')
#   source('../code/upscale.R')
#   source('../code/rt_boot.R')
#   source('../code/rt_pipeline.R')
#   rt_ests <- upscale_cori_pipeline(df = sim_df, 
#                                    obscolname = 'obs',
#                                    p_obs = p_obs,
#                                    delay_mean = delay_mean,
#                                    gen_int_pars = c(mean = parlist$true_mean_GI, var = parlist$true_var_GI), 
#                                    nboot = nboot, 
#                                    ttl = '', 
#                                    obs_type = 'cases',
#                                    min_window = min_window,
#                                    w.tune = w.tune)
#   
#   ## Return
#   # list(df = merge(sim_df, 
#   #       rt_ests$df %>% select(date, contains('rt.')), 
#   #       by = 'date'),
#   #      plot = rt_ests$rt_plot
#   # )
#   merge(sim_df, 
#         rt_ests$df %>% select(date, contains('rt.')), 
#         by = 'date') %>%
#     mutate(last_day = last_obs_time, 
#            median_cases_per_day = median_cases_per_day,
#            delay_mean = delay_mean)
# }
# 
