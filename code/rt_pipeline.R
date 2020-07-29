# df = dat %>% filter(restore_region == rr)
# obscolname ='smoothed'
# p_obs = .9
# delay_pars = read_rds('../data/fitted_delays/delay_infection_to_test_posterior.rds') %>% bind_cols %>% select(1:2)
# delay_type = 'lognormal'
# gen_int_pars = c(mean = 4.5, var = 1.7) ## From Ganyani et al
# nboot = 25
# ttl = rr
# obs_type = 'cases'




full_rt_pipeline <- function(df, ## Data frame containing time series of observations
                      obscolname, ## name of the column containing incident observations
                      p_obs,  ## probability of observation (must be a single numberic value between 0 and 1)
                      delay_pars, ## Data frame containing a column for each delay distribution parameter, and at least one row representing posterior samples. If multiple rows are provided, the algorithm will sample over rows to incorporate uncertainty.
                      delay_type = 'lognormal', ## Can also be "gamma"
                      gen_int_pars, ## Vector containing the mean and sd of the generation interval, which we assume ~gamma.
                      nboot = 500, ## Number of bootstraps
                      ttl = 'Data',
                      obs_type = 'deaths',
                      min_window = 1
                      ){
  
  ## Check inputs
  df <- as.data.frame(df)
  df$obs <- df[,obscolname]
  df$obs <- round(df$obs)
  df$obs <- na_to_0(df$obs)
  df$time <- as.numeric(df$date-min(df$date))
  stopifnot('obs' %in% colnames(df))
  stopifnot(p_obs >0 & p_obs <= 1)
  
  ## Plot the data
  df %>%
    select(date, obs) %>%
    setNames(c('date', 'observations')) %>%
    ggplot() +
    geom_line(aes(x = date, y = observations))+
    ylab(obscolname)+
    ggtitle(ttl) -> datplot
  
  
  ## 3. Use RL to infer times of infection given the posterior parms of the delay
  deconvolved <- deconvolve(df$obs, times = df$time, delay_posterior = delay_pars, nboot = nboot, delay_type = delay_type) 
  
  ## 4. Upscale from deconvolved times of infection to total infections based on p_obs
  deconvolved <- lapply(deconvolved, FUN = function(df.in) {
    est_total_inf(df.in, 
                  p_obs = p_obs, 
                  obs_colname = 'RL_result',
                  n_replicates = 1) 
  }
  )
  


  
  ## 5. Estimate Rt
  ## Calculate the window size
  low_inf_count = filter(bind_rows(deconvolved), RL_result>0 & !is.na(RL_result)) %>%
    pull(RL_result) %>%
    quantile(.2) %>%
    round %>%
    max(1)
  ww.in = max(min_window, floor(50/low_inf_count))
  cat(sprintf('\noverall window is %.0f\n', ww.in))
  
  
  rt_ests <- rt_boot(infection_ests = deconvolved %>% 
                       bind_rows(.id = 'rep') %>% 
                       mutate(RL_result = round(RL_result)) %>%
                       pivot_wider(id_cols = time, 
                                   names_from = 'rep', 
                                   names_prefix = 'infections.', 
                                   values_from = RL_result),
                     p_obs = p_obs, 
                     ww = ww.in,
                     GI_pars = gen_int_pars)
  
  rt_ests$summary <- rt_ests$summary %>%
    merge(select(df, date, time), by = 'time')
  
  
  ## 6. Format outputs and plot
  deconvolved <- deconvolved %>%
    bind_rows(.id = 'rep') %>%
    merge(select(df, date, time), by = 'time') %>%
    select(date, rep, RL_result, infections.1) %>%
    setNames(c('date', 'rep', 'obs_infections_est', 'total_infections_est')) 
  
  RL_plot <- bind_rows(
    deconvolved %>% select(-total_infections_est) %>% pivot_longer(3),
    df %>% mutate(rep = NA, name = obscolname) %>% rename(value = obs) %>% select(date, rep, name, value)
  )%>% 
    ggplot()+
    geom_line(aes(x = date, y = value, group = rep, color = name), alpha = .8) +
    ylim(c(0, max(df$obs)+20))+
    ggtitle('Observations vs. estimated partial infections')
  
  
  upscale_plot <- bind_rows(
    deconvolved %>% select(-obs_infections_est) %>% pivot_longer(3),
    df %>% mutate(rep = NA, name = obscolname) %>% rename(value = obs) %>% select(date, rep, name, value)
  )%>% 
    ggplot()+
    geom_line(aes(x = date, y = value, group = rep, color = name), alpha = .8) +
    ylab('count per day') +
    ylim(c(0, max(df$obs, na.rm = T)*1.3/p_obs))+
    ggtitle(ttl)+
    scale_color_discrete('', labels = c(sprintf('%s (observed)', obs_type), 'infections (bootstrapped estimates)'))
  
  rt_plot <- df %>%
    merge(rt_ests$summary, by = 'date') %>% 
    ggplot()+
    geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper), fill = 'yellow', alpha = .5)+
    #geom_line(data = bind_rows(rt_ests$all, .id = 'rep') %>% mutate(date = min(df$date)+time), aes(x = date, y = rt.mean, group = rep), alpha = .5, color = 'orange')+
    geom_line(aes(x = date, y = rt.mean), color = 'red')+
    xlim(c(min(deconvolved$date), max(deconvolved$date)))+
    geom_hline(aes(yintercept = 1))+
    ylab(expression(paste(R[t])))
  
  
  delplot <- delay_pars %>% 
    select(1:2) %>%
    pivot_longer(1:2) %>%
    ggplot()+
    geom_density(aes(x = value, fill = name, color = name), alpha = .5)+
    facet_grid(.~name, scales = 'free_x') +
    ggtitle(sprintf('%s delay pars', delay_type))
  
  
  outplot <- cowplot::plot_grid(datplot, 
                     plot_grid(RL_plot+theme(legend.position = 'none'), 
                               upscale_plot+theme(legend.position = 'none'), nrow = 1), 
                     delplot,
                     rt_plot, nrow = 4, ncol = 1)
  # ggsave(sprintf('testfigs/%s/%s.png',
  #                outdir,
  #                outname),
  #        width = 6, height = 8, dpi = 300, units = 'in')
  
  return(list(df = df %>% merge(rt_ests$summary %>% mutate(date = min(df$date)+time), by = 'date'),
              deconvolved = deconvolved,
              rt_ests = rt_ests,
              master_plot = outplot,
              datplot = datplot,
              RL_plot = RL_plot,
              upscale_plot = upscale_plot,
              delay_plot = delplot,
              rt_plot = rt_plot))
  
}