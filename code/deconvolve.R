deconvolve <- function(obs,  ## Vector of counts per day
                       times = NULL,  ## Optional numeric vector of times
                       delay_posterior,
                       delay_type = 'lognormal',
                       nboot
){
  ## Check inputs
  if(length(times) == 0){
    times = 1:length(obs)
  }
  stopifnot(length(times) == length(obs))
  stopifnot(is.numeric(times))
  stopifnot(delay_type %in% c('lognormal', 'gamma'))

  ## Sample nboot delay parameters
  spars <- delay_posterior %>%
    slice(sample(1:nrow(delay_posterior), size = nboot, replace = T))

  if(delay_type == 'lognormal'){
  outs <- mapply(FUN = function(mm, ss){
    get_RL(observed = obs, 
           times = times, 
           p_delay = get_discrete_lognormal(k = 0:(length(obs)-1), mu = mm, sigma = ss)) %>%
      filter(time >= 0)
  }, 
  mm = spars$mu, ss = spars$sigma, SIMPLIFY = FALSE)
  }else{
    stopifnot(delay_type == 'gamma')
    outs <- mapply(FUN = function(mm, vv){
      get_RL(observed = obs, 
             times = times, 
             p_delay = discr_si(k = 0:(length(obs)-1), mu = mm, sigma = sqrt(vv))) %>%
        filter(time >= 0)
    }, 
    mm = spars$mean, vv = spars$var, SIMPLIFY = FALSE)
    
    
  }
  
}



deconvolve_smoothed <- function(obs,  ## Vector of counts per day
                       times = NULL,  ## Optional numeric vector of times
                       delay_posterior,
                       delay_type = 'lognormal',
                       nboot,
                       sd.in = 5,
                       add_noise = FALSE,
                       method = 'RL',
                       ww.in = 8
){
  ## Check inputs
  if(length(times) == 0){
    times = 1:length(obs)
  }
  stopifnot(length(times) == length(obs))
  stopifnot(is.numeric(times))
  stopifnot(delay_type %in% c('lognormal', 'gamma'))
  stopifnot(ww.in %% 2 == 0)
  
  if(add_noise){
    obs = obs + rnorm(length(obs), mean = 0, sd = sd.in)
    obs = ifelse(obs < 0, 0, obs)
  }
  
  ## Sample nboot delay parameters
  spars <- delay_posterior %>%
    slice(sample(1:nrow(delay_posterior), size = nboot, replace = T))
  
  if(delay_type == 'lognormal'){
    outs <- mapply(FUN = function(mm, ss){
      get_RL(observed = obs, 
             times = times, 
             p_delay = get_discrete_lognormal(k = 0:(length(obs)-1), mu = mm, sigma = ss),
             ww = ww.in) %>%
        filter(time >= 0)
    }, 
    mm = spars$mu, ss = spars$sigma, SIMPLIFY = FALSE)
  }else{
    stopifnot(delay_type == 'gamma')
    outs <- mapply(FUN = function(mm, vv){
      get_RL(observed = obs, 
             times = times, 
             p_delay = discr_si(k = 0:(length(obs)-1), mu = mm, sigma = sqrt(vv))) %>%
        filter(time >= 0)
    }, 
    mm = spars$mean, vv = spars$var, SIMPLIFY = FALSE)
    
    
  }
  
}
# 
# 
# mm = 3.14; ss = .5
# del_pmf <- get_discrete_lognormal(k = 0:100, mu =  mm, sigma = ss)
# del_pmf <- del_pmf/sum(del_pmf)
# mean_delay <- exp(mm+ss^2/2)
# observed = sim_df$obs_cases
# init <- c(
#   observed[ceiling(mean_delay):length(observed)],  ## Observed vector, with first mean_delay days removed
#   rep(tail(observed, 1), floor(mean_delay)) ## Replace discarded values with 1s at the end of the vector
# )
# outs <- surveillance::backprojNP(sts = surveillance::sts(observed = observed, start = c(2020, min(sim_df$time))),
#                          incu.pmf = del_pmf)
# 
# 
# ,
#                          control = list(
#                            k = 8, # Days in smoothing window
#                            Tmark = max(sim_df$time)-sum(cumsum(del_pmf)<.9),
#                            lambda0 = matrix(init, ncol = 1)
#                          ))
