test_sample_size <- function(p_obs = 1, 
                             median_cases_per_day = NULL, 
                             nboot = 25 
){
  
  
  source('../code/util.R')
  source('../code/cori.R')
  source('../code/Richardson_Lucy.R')
  source('../code/deconvolve.R')
  source('../code/upscale.R')
  source('../code/rt_boot.R')
  
  
  stopifnot(length(p_obs) == 0 | (p_obs <= 1 & p_obs >= 0))
  stopifnot((length(p_obs)>0)|length(median_cases_per_day)>0)
  sim_df <- get_sim_df()
  sim_df$incidence <- na_to_0(sim_df$incidence)
  stopifnot('incidence' %in% colnames(sim_df))
  #pairs(delay_posterior)
  
  
  
  
  
  ## 1. Generate synthetic observations ----------
  ##     Set the delay to observation
  source('../code/infer_times_of_infection_observation.R')
  ## Set the delay
  true_delay_pars = c(1.525, 0.395) # From table 2 of Linton et al
  ## Set a range of possible delay parameters, centered around the truth
  delay_pars_df = data_frame(mu = rnorm(100, 1.525, .1),
                             sigma = rnorm(100, .395, .03)) %>%
    mutate_all(.funs = function(xx){ifelse(xx<0, 0, xx)})
  ## Visualize the resulting delay distributions
  true_delays <- data.frame(xx = seq(0, 20, by = .1)) %>%
    mutate(dens = dlnorm(xx, true_delay_pars[1], true_delay_pars[2]))
  delay_plot <- apply(delay_pars_df, MARGIN = 1, function(pp){dlnorm(seq(0, 20, .1), pp[1], pp[2])}) %>%
    as.data.frame() %>%
    mutate(xx = seq(0, 20, .1)) %>%
    pivot_longer(-xx) %>%
    ggplot()+
    geom_line(aes(x = xx, y = value, color = name), alpha = .5, show.legend = F)+
    geom_line(data = true_delays, aes(x = xx, y = dens)) +
    xlab('delay infection to onset')+
    ylab('density') 
  ## Write a function to draw delays for each individual in the data
  rdelay <- function(nn){rlnorm(nn, true_delay_pars[1], true_delay_pars[2])}
  ## Convolve to get synthetic times of observation
  sim_df <-  merge(
    sim_df %>% select(time, incidence, true_rt),
    get_tObs_from_tInf(n_dS = sim_df$incidence, 
                       times = sim_df$time, 
                       r_delay_dist = rdelay, 
                       return_times = T) %>% rename(delayed = n),
    by = 'time') %>%
    filter(time <= 150 & time >= 40) 
  
  
  ## 1.b Downsample to the desired probabilty of observation or sample size ----------
  if(length(median_cases_per_day)>0){
    raw = median(sim_df$delayed)
    p_obs <- median_cases_per_day/raw
  }
  sim_df$obs = sapply(sim_df$delayed, FUN = function(NN) rbinom(n=1, size = NN, prob = p_obs)) #+ min_0(rnorm(nrow(sim_df)))
  sprintf('Setting p_obs=%.3f to obtain a median of %1.0f cases per day', p_obs, median(sim_df$obs))
  
  
  ## 3. Use RL to infer times of infection given the posterior parms of the delay
  deconvolved <- deconvolve(sim_df$obs, times = sim_df$time, delay_posterior = delay_pars_df, nboot = nboot, delay_type = 'lognormal') 
  
  ## 4. Upscale from deconvolved times of infection to total infections based on p_obs
  deconvolved <- lapply(deconvolved, FUN = function(df.in) {
    est_total_inf(df.in, 
                  p_obs = 1, 
                  obs_colname = 'RL_result',
                  n_replicates = 1) 
  }
  )
  
  
  ## 5. Estimate Rt
  rt_ests <- rt_boot(deconvolved %>% 
                       bind_rows(.id = 'rep') %>% 
                       mutate(RL_result = round(RL_result)) %>%
                       pivot_wider(id_cols = time, 
                                   names_from = 'rep', 
                                   names_prefix = 'infections.', 
                                   values_from = RL_result),
                     p_obs = 1, 
                     ww = 1,
                     GI_pars = c(8, 32))
  
  
  
  ## 6. Format outputs and plot
  deconvolution_summary = deconvolved %>% 
    bind_rows(.id = 'rep') %>% 
    mutate(RL_result = round(RL_result)) %>%
    group_by(time) %>%
    summarise(med = median(RL_result),
              lower = min(RL_result),
              upper = max(RL_result))
  ## Plot
  cowplot::plot_grid(
    sim_df %>%
      select(time, incidence, obs) %>%
      setNames(c('time', 'total infections', 'observations')) %>%
      pivot_longer(-time) %>%
      ggplot() +
      geom_line(aes(x = time, y = value, color = name), show.legend = F)+
      ylab('daily count')+
      ggtitle('Total infections and observations', subtitle = sprintf('Median %2.0f observed per day; %2.2f%% of total', median(sim_df$obs), p_obs*100)),
    sim_df %>%
      select(time, obs) %>%
      setNames(c('time', 'observations')) %>%
      ggplot() +
      geom_line(data = deconvolution_summary, aes(x = time, y = med))+
      geom_ribbon(data = deconvolution_summary, aes(x = time, ymin = lower, ymax = upper), alpha = .3)+
      geom_line(aes(x = time, y = observations, color = 'placeholder'), show.legend = F)+
      ylab('daily count')+
      xlim(c(40, 150))+
      ggtitle('Observations and inferred infections', subtitle = sprintf('Mean %2.0fd from infection to onset', exp(true_delay_pars[1]+true_delay_pars[2]^2/2))),
    ncol = 2
  ) -> syndat
  
  ymx <- min(max(rt_ests$summary$rt.upper, na.rm = T), 4)
  rt_plot <- sim_df %>%
    merge(rt_ests$summary, by = 'time') %>% 
    mutate(rt.mean = ifelse(rt.mean>ymx, ymx, rt.mean),
           rt.upper = ifelse(rt.upper>ymx, ymx, rt.upper),
           rt.lower = ifelse(rt.lower>ymx, ymx, rt.lower)) %>%
    ggplot()+
    geom_ribbon(aes(x = time, ymin = rt.lower, ymax = rt.upper), fill = 'green3', alpha = .3)+
    #geom_line(data = bind_rows(rt_ests$all, .id = 'rep') %>% mutate(date = min(df$date)+time), aes(x = date, y = rt.mean, group = rep), alpha = .5, color = 'orange')+
    geom_line(aes(x = time, y = rt.mean), color = 'green3')+
    geom_line(aes(x = time, y = true_rt))+
    geom_hline(aes(yintercept = 1), lty = 2)+
    ylab(expression(paste(R[t])))+
    ggtitle('Rt - truth vs. estimate')
  
  
  
  outplot <- cowplot::plot_grid(syndat, 
                                rt_plot, nrow = 2, ncol = 1)
  
  return(list(rt_df = merge(sim_df, rt_ests$summary, by = 'time', all = T) %>%
                merge(deconvolution_summary %>% setNames(c('time', 'median_inf_est', 'lower_inf_est', 'upper_inf_est')), 
                      by = 'time', all = T),
              outplot = outplot,
              delay_plot = delay_plot))
}