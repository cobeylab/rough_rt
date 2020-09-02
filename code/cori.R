# df.in = ins
# obs_col_name = 'new_deaths'
# window = 3
# out_name = 'rt'
# mean_delay = 15
# SI_mean = 4.5
# SI_var = 3
# wend = F


# df.in = rtdf
# icol_name = 'incidence'
# out_name = 'Cori'
# window = 1
# SI_mean = 8
# SI_var = 2*(parlist$true_mean_SI/2)^2
## Output cori estimate with mean, CI and times given an input df, and the name of the incidence column
get_cori <- function(df.in, 
                     obs_col_name,
                     out_name = 'Cori',
                     window = 1, 
                     mean_delay = 0,
                     SI_mean=parlist$true_mean_SI, 
                     SI_var=2*(parlist$true_mean_SI/2)^2,
                     wend = TRUE){
  
  df.in[,obs_col_name] <- na_to_0(df.in[,obs_col_name]) ## Replace NAs in incidence
  df.in$shifted_obs <- lead(pull(df.in, eval(obs_col_name)), n = round(mean_delay))
  
  
  idat <- df.in %>%
    #filter(get(obs_col_name) > 0 & !is.na(get(obs_col_name))) %>%
    tidyr::complete(time = 2:max(df.in$time)) %>%
    mutate_all(.funs = function(xx){ifelse(is.na(xx), 0, xx)}) %>%
    arrange(time)
  

  st.time <- filter(df.in, shifted_obs>(50/window)) %>%
    pull(time) %>% head(1)
  st.time <- max(2, st.time)
  
  ed.time <- filter(df.in, shifted_obs>0) %>%
    pull(time) %>% tail(1)
  
  ts <- idat$time
  ts <- ts[ts > st.time & ts <= (ed.time-window)]
  te <- ts+(window-1)
  
  EpiEstim::estimate_R(
    incid = pull(idat, shifted_obs),
    method = "uncertain_si",
    config = EpiEstim::make_config(
      list(
        mean_si = SI_mean,
        min_mean_si = SI_mean -1,
        max_mean_si = SI_mean + 1,
        std_mean_si = 1.5,
        std_std_si = 1.5,
        std_si = sqrt(SI_var),
        min_std_si = sqrt(SI_var)*.8,
        max_std_si = sqrt(SI_var)*1.2,
        n1 = 50,
        n2 = 100, 
        t_start=ts,
        t_end=te
      )
    )
  ) -> outs
  
  outs$R %>%
    mutate(time = if(wend == TRUE) t_end else ceiling((t_end+t_start)/2) ) %>%
    dplyr::select(time, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`) %>%
    setNames(c('time', paste0(out_name, '.mean'), paste0(out_name, '.025'), paste0(out_name, '.975')))
}

