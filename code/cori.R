# df.in = sim_df
# icol_name = 'delayed'
# out_name = 'test'
# window = 5
# GI_mean = parlist$true_mean_GI
# GI_var = parlist$true_var_GI
# wend = F

## Output cori estimate with mean, CI and times given an input df, and the name of the incidence column
get_cori <- function(df.in, 
                     icol_name, 
                     out_name = 'Cori',
                     window = 1, 
                     GI_mean, 
                     GI_var,
                     GI_min_mean = 1.85,
                     GI_max_mean = 5.60,
                     GI_min_sd = 0.87,
                     GI_max_sd = 5.43,
                     wend = TRUE){
  
  df.in <- df.in %>% mutate(cori_time = 1:nrow(.))
  
  max.obs.time <- df.in %>% filter(!is.na(!!sym(icol_name))) %>% pull(cori_time) %>% tail(1)
  
  
  idat <- df.in %>%
    #filter(get(icol_name) > 0 & !is.na(get(icol_name))) %>%
    complete(cori_time = 2:max.obs.time) %>%
    arrange(cori_time) %>%
    filter(cori_time <= max.obs.time)
  idat[icol_name] <- na_to_0(idat[icol_name])
  #mutate(cleaned = ifelse(is.na(!!sym(icol_name)) & time <= max.obs.time, 0, !!sym(icol_name)))
  
  
  ts <- idat$cori_time
  ts <- ts[ts > 1 & ts <= (max(ts)-window+1)]
  te <- ts+(window-1)
  
  
  
  estimate_R(
    incid = pull(idat, !!icol_name),
    method = "uncertain_si",
    config = make_config(
      list(
        mean_si = GI_mean,
        min_mean_si = GI_max_mean,
        max_mean_si = GI_min_mean,
        std_mean_si = 1.5,
        std_std_si = 1.5,
        std_si = sqrt(GI_var),
        min_std_si = GI_max_sd,
        max_std_si = GI_min_sd,
        n1 = 50,
        n2 = 100, 
        t_start=ts,
        t_end=te
      )
    )
  ) -> outs
  
  time_offset = unique(df.in$time - df.in$cori_time)
  
  outs$R %>%
    mutate(cori_time = if(wend == TRUE) t_end else ceiling((t_end+t_start)/2) ) %>%
    mutate(time = cori_time+time_offset) %>%
    select(time, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`) %>%
    setNames(c('time', paste0(out_name, '.mean'), paste0(out_name, '.025'), paste0(out_name, '.975')))
}

