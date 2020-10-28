rt_boot <- function(
  infection_ests,
  p_obs,
  mean_delay=0,
  ww = 3,
  GI_pars = c(parlist$true_mean_GI, parlist$true_var_GI) # Mean and var of gamma GI
){
  
  
  
  ## Estimate Rt and mrege with the cleaned input data frame
  library(doParallel)
  cl <- makeCluster(parallel::detectCores()-1)
  registerDoParallel(cl)
  est_list <- foreach(ii=2:ncol(infection_ests), .packages = c('dplyr', 'EpiEstim', 'tidyr'), .export = c('get_cori', 'na_to_0', 'GI_pars')) %dopar% {
                        
                        ins = infection_ests[,c(1, ii)]
                        
                        df<-get_cori(ins, 
                                 icol_name = paste0('infections.', ii-1), 
                                 window = ww, 
                                 out_name = 'rt',
                                 GI_mean = GI_pars[1], ## Rough estimates from Ganyani et al
                                 GI_var = GI_pars[2],   
                                 wend = F) %>%
                          merge(select(ins, time), by = 'time') 
                        
                        df
                      }
  stopCluster(cl)
  
  ## For each date, get the median of means, the lower .025 of lower bounds and the upper .975 of upper bounds
  summarized_ests <-  bind_rows(est_list, .id = 'replicate') %>%
    group_by(time) %>%
    ## PHIL - for each replicate, we get a mean, upper and lower CI rt estimate.
    ##    I'm not sure what the best way to aggregate is.
    ##    Here I'm taking the median of means, the lower bound of lower bounds and the upper bound of upper bounds.
    summarise(rt.mean = median(rt.mean, na.rm = TRUE),
              rt.lower = quantile(rt.025, probs = .025, na.rm = TRUE),
              rt.upper = quantile(rt.975, probs = 0.975, na.rm = TRUE))
  
  return(list(all = est_list, summary = summarized_ests))
  
}