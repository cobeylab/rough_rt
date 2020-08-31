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
                     icol_name,
                     out_name = 'Cori',
                     window = 1, 
                     SI_mean=parlist$true_mean_SI, 
                     SI_var=2*(parlist$true_mean_SI/2)^2,
                     wend = TRUE){
  
  ## Extract the last non-NA observation
  max.valid.obs <- (df.in %>% filter(!is.na(!!sym(icol_name))) %>% pull(time) %>% tail(1))
  ## Complete all dates up to the max.valid.obs, and convert any intermediate NAs to 0s
  idat <- df.in %>%
    complete(time = 2:max.valid.obs) %>%
    arrange(time) %>%
    filter(time <= max.valid.obs)
  idat[icol_name] <- na_to_0(idat[icol_name])
  
  ## Get the first window start time for input into Cori.
  ## Must be >2. We delay until a sufficient number observed per day.
  st.time <- filter(df.in, !!sym(icol_name)>(50/window)) %>%
    pull(time) %>% head(1)
  st.time <- max(2, st.time)
  
  ts <- idat$time
  ts <- ts[ts > st.time & ts <= (max.valid.obs-window)]
  te <- ts+(window-1)
  
  EpiEstim::estimate_R(
    incid = pull(idat, !!sym(icol_name)),
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
    select(time, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`) %>%
    setNames(c('time', paste0(out_name, '.mean'), paste0(out_name, '.025'), paste0(out_name, '.975')))
}

