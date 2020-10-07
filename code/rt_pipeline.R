# df = dat %>% filter(region != 'CHICAGO') %>% group_by(date) %>%
#   summarise(new_cases = sum(new_cases, na.rm = T))
# obscolname ='new_deaths'
# p_obs = .9
# delay_pars = read_rds('../data/fitted_delays/Linton_lognorm_sample.rds') %>% bind_cols %>% select(1:2) %>% bind_cols %>% select(1:2)
# delay_type = 'lognormal'
# gen_int_pars = c(mean = 4.5, var = 1.7) ## From Ganyani et al
# nboot = 25
# ttl = 'IL Overall'
# 
# 


upscale_cori_pipeline <- function(df, ## Data frame containing time series of observations
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
  cat(sprintf('%2.2f', p_obs))
  
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
  df %>%
    select(date, obs) %>%
    setNames(c('date', 'observations')) %>%
    ggplot() +
    geom_line(aes(x = date, y = observations))+
    ylab(obscolname)+
    ggtitle(ttl) -> datplot
  
  
  ## 3. Shift timeseries by mean delay
  df <- df %>%
    arrange(date) %>%
    complete(date = get_complete_dates(date)) %>%
    mutate(shifted = lead(obs, round(delay_mean)))
  
  ## 4. Repeatedly upscale shifted time series
  if(p_obs < 1){
  upscaled <- est_total_inf(df, 
                  p_obs = p_obs, 
                  obs_colname = 'shifted',
                  n_replicates = nboot) 
  }else{
    stopifnot(abs(p_obs-1) < 1e-6)
    upscaled <- df %>% mutate(infections.1 = shifted)
  }


  
  ## 5. Estimate Rt
  ## Calculate the window size
  low_inf_count = filter(df, !is.na(obs) & obs > 0) %>%
    pull(obs) %>%
    quantile(.2) %>%
    round %>%
    max(1)
  ww.in = max(min_window, floor(w.tune/low_inf_count))
  cat(sprintf('\nwindow is %.0f\n', ww.in))
  
  
  rt_ests <- rt_boot(infection_ests = upscaled %>% select(time, contains('infections')),
                     p_obs = p_obs, 
                     ww = ww.in,
                     GI_pars = gen_int_pars)
  
  rt_ests$summary <- rt_ests$summary %>%
    merge(select(df, date, time), by = 'time')
  
  
  ## 6. Format outputs and plot
  upscaled <- upscaled %>%
    pivot_longer(cols = contains('infections'), names_to = 'rep', names_prefix = 'infections.', values_to = 'upscaled_total') %>%
    select(date, obs, shifted, rep, upscaled_total)
  
  upscale_plot <- upscaled %>%
    group_by(date) %>%
    mutate(lower.upscale = quantile(upscaled_total, .025, na.rm = T),
           est_total = quantile(upscaled_total, .5, na.rm = T),
           upper.upscale = quantile(upscaled_total, .975, na.rm = T)) %>%
    select(-upscaled_total, -rep) %>%
    distinct %>%
    pivot_longer(cols = c(obs, shifted, est_total)) %>%
    ggplot()+
    geom_line(aes(x = date, y = value, color = name), alpha = .8) +
    scale_color_viridis_d('') +
    geom_ribbon(aes(x = date, ymin = lower.upscale, ymax = upper.upscale), fill = viridisLite::viridis(3)[1], alpha = .5)
  
  rt_plot <- df %>%
    merge(rt_ests$summary, by = 'date') %>% 
    ggplot()+
    geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper), fill = 'dodgerblue', alpha = .5)+
    geom_line(aes(x = date, y = rt.mean), color = 'blue3')+
    xlim(c(min(df$date), max(df$date)))+
    ylim(c(0, 4))+
    geom_hline(aes(yintercept = 1))+
    ylab(expression(paste(R[t])))
  

  
  outplot <- cowplot::plot_grid(upscale_plot + theme(legend.position = 'bottom'),
                     rt_plot, nrow = 2, ncol = 1)

  # c_p_obs<-data_frame(dd = 0:40) %>%
  #   mutate(p_report = plnorm(dd, median(rep_delay_pars$mu), median(rep_delay_pars$sigma))) 
  # 
  #  clip_end_dates <- c('clip.50' = filter(c_p_obs, p_report <= .5)%>%tail(1)%>%pull(dd),
  #                      'clip.90' = filter(c_p_obs, p_report <= .9)%>%tail(1)%>%pull(dd))
  
  outs <- df %>% merge(rt_ests$summary, by = c('date', 'time')) 
  
 # %>% mutate(robustness = ifelse(date <= (max(date)-clip_end_dates['clip.90']), 'robust', 
  #                              ifelse(date <= (max(date)-clip_end_dates['clip.50']), 'partial data', 'unreliable'))) -> outs
  
  return(list(df = outs,
              upscaled = upscaled,
              rt_ests = rt_ests,
              master_plot = outplot,
              datplot = datplot,
              upscale_plot = upscale_plot,
              rt_plot = rt_plot))
  
}
