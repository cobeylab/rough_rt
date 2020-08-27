est_total_inf <- function(df, ## Data frame containing a column with daily observations
                          p_obs, ## Vector of observation probabilities, one entry for each observation. If you provide a single number, assumes constant observation probability
                          obs_colname, ## Name of the column holding the observations
                          n_replicates = 1) ## Number of times to resample total infections 
{
  draw_one_sample <- function(k, p){
    # adjust observations to work with negative binomial, i.e., case with 0 observations
    adjusted_k = replace(k, k==0, 0.1)
    
    # Draw a negative binomial sample
    observations = round(mapply(rnbinom, 1, adjusted_k, p) + adjusted_k)
    as.numeric(observations)
  }
  
  obs = df %>% dplyr::select(obs_colname) %>% unlist(use.names = F)
  obs = ifelse(is.na(obs), 0, obs)
  
  ## For each entry in the observations column, upscale
  infection_samples = replicate(n = n_replicates, draw_one_sample(obs, p_obs))
  
  colnames(infection_samples) = paste0('infections.', 1:n_replicates)
  bind_cols(df, as.data.frame(infection_samples))
}



# est_total_inf_overdispersed <- function(df, ## Data frame containing a column with daily observations
#                           p_obs, ## Vector of observation probabilities, one entry for each observation. If you provide a single number, assumes constant observation probability
#                           k, ## Dispersion parameter (lower -> higher variance)
#                           obs_colname, ## Name of the column holding the observations
#                           n_replicates = 1) ## Number of times to resample total infections 
# {
#   draw_one_sample2 <- function(n_obs, k, p_obs, n_reps = 1){
#     if(is.na(n_obs)){
#       return(rep(NA, n_reps))
#     }
#     # possilbe N values (N = total infections)
#     if(n_obs > 1){
#       Nvec = seq(ceiling(n_obs/p_obs*.1), ceiling(n_obs/p_obs*3))
#     }else{
#       Nvec = 1:100
#     }
#     likelihoods <- sapply(Nvec, FUN = function(nn){dnbinom(n_obs, mu = nn*p_obs, size = k)})
#     #plot(Nvec, likelihoods/sum(likelihoods))
#     sample(x = Nvec, size = n_reps, prob = likelihoods, replace = T) %>%
#       unlist()
#   }
#   
#   
#   obs = df %>% select(obs_colname) %>% unlist(use.names = F) %>% round()
#   
#   ## For each entry in the observations column, upscale
#   # Draw a negative binomial sample
#   total_infections = sapply(obs, draw_one_sample2, k = k, p_obs = p_obs, n_reps = n_replicates) 
#   if(n_replicates>1){
#     total_infections = t(total_infections)
#     colnames(total_infections) = paste0('infections.', 1:n_replicates)
#     df <- bind_cols(df, as.data.frame(infection_samples))
#   }else{
#     stopifnot(n_replicates == 1)
#     df <- df %>% mutate(infections.1 = total_infections)
#   }
#   # ti2 = replicate(n = n_replicates, draw_one_sample(obs, p_obs))
#   # plot(obs, ylim = c(0, 20000))
#   # lines(obs/p_obs, col = 'black')
#   # for(ii in 1:5){
#   #   lines(total_infections[,ii], col = 'red')
#   #   lines(ti2[,ii], col = 'blue')
#   # }
#   
#   df
# 
# }

