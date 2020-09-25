df = sim_df 
obscolname = 'cases'
p_obs = p_obs
delay_mean = delay_mean
gen_int_pars = c(mean = parlist$true_mean_GI, var = parlist$true_var_GI)
nboot = 5
ttl = ''
obs_type = 'cases'
min_window = min_window
w.tune = w.tune


ss_pipeline <- function(df, ## Data frame containing time series of observations
                      obscolname, ## name of the column containing incident observations
                      p_obs,  ## probability of observation (must be a single numberic value between 0 and 1)
                      delay_mean, 
                      gen_int_pars, ## Vector containing the mean and sd of the generation interval, which we assume ~gamma.
                      nboot = 500, ## Number of bootstraps
                      ttl = 'Data',
                      obs_type = 'cases',
                      min_window = 1,
                      w.tune = 50
                      ){
  
  ## Utility function
  get_complete_dates <- function(dates){
    num_dates = date_to_num(min(dates)):date_to_num(max(dates))
    num_to_date(num_dates)
  }
  
  ## Check inputs
  df <- as.data.frame(df)
  df$obs <- df[,obscolname]
  df$obs <- round(df$obs)
  df$obs <- na_to_0(df$obs)
  if(!any(grepl('time', names(df)))) df$time <- as.numeric(df$date-min(df$date))
  stopifnot('obs' %in% colnames(df))
  stopifnot(p_obs >0 & p_obs <= 1)
  stopifnot(delay_mean>0)
  
  ## Plot the data
  # df %>%
  #   select(date, obs) %>%
  #   setNames(c('date', 'observations')) %>%
  #   ggplot() +
  #   geom_line(aes(x = date, y = observations))+
  #   ylab(obscolname)+
  #   ggtitle(ttl) -> datplot
  
  
  ## 3. Shift timeseries by mean delay
  df <- df %>%
    arrange(date) %>%
    complete(date = get_complete_dates(date)) %>%
    mutate(shifted = lead(obs, round(delay_mean)))
  
  ## 4. Repeatedly upscale shifted time series
  if(p_obs < 1){
  # upscaled <- est_total_inf(df, 
  #                 p_obs = p_obs, 
  #                 obs_colname = 'shifted',
  #                 n_replicates = nboot) 
    one_downsample = function(xx){sapply(xx, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs))}
    downsampled <- replicate(n = nboot, expr = one_downsample(sim_df[,obscolname]))
    colnames(downsampled) = paste('infections', 1:nboot, sep = '.')
    cat(sprintf('Setting p_obs=%.3f to obtain a median of %1.0f cases per day', p_obs, median(downsampled)))
    downsampled <- bind_cols(df, as.data.frame(downsampled))
  }else{
    stopifnot(abs(p_obs-1) < 1e-6)
    upscaled <- df %>% mutate(infections.1 = shifted)
  }


  
  ## 5. Estimate Rt
  ## Calculate the window size
  low_inf_count = select(downsampled, contains('infections.')) %>%
    pivot_longer(everything()) %>%
    filter(!is.na(value) & value>0) %>%
    pull(value) %>%
    quantile(.2) %>%
    round %>%
    max(1)
  ww.in = max(min_window, floor(w.tune/low_inf_count))
  cat(sprintf('\nwindow is %.0f\n', ww.in))
  
  
  rt_ests <- rt_boot(infection_ests = downsampled %>% select(time, contains('infections')),
                     p_obs = p_obs, 
                     ww = ww.in,
                     GI_pars = gen_int_pars)
  
  rt_ests$summary <- rt_ests$summary %>%
    merge(select(df, date, time), by = 'time')
  
  
  outs <- df %>% merge(rt_ests$summary, by = c('date', 'time')) 

  
  return(list(df = outs))
  
}
