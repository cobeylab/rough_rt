## This function inputs observed incidence (numbers of cases, deaths, hospitalizations, etc. per day), and uses an adaptation of  Richardson-Lucy deconvolution to recover the underlying incidence curve. 

## ------- Inputs
##  This function follows methods in Goldstein et al. (https://www.pnas.org/content/pnas/106/51/21825.full.pdf)
#' @param observed - a vector of the number of observed cases, deaths, hospitalizations, etc. per day
#' @param times - a numeric vector of times corresponding to the entries in observed. Must be the same length as observed.
#' @param p_delay - a numeric vector whose entries give the probability that the delay from infection to observation is exactly 0, 1, ... N days. Need not sum to one, since not all cases are observed.
#' @param max_iter - maximum number of times to iterate. Following Goldstien et al., the algorithm continues to run until the normalized chi2 statistic comparing the observed and expected number of deaths per day falls below 1, or until the maximum number of iterations is reached.
#' @param out_col_name - string giving the name of the column in which to output the imputed times of infection
## --------- Outputs
#' @value - the function returns a data frame with columns time and out_col_name (which gives the imputed number of infections per timestep)

# # # For testing/debugging
# obs_df <- obs_df %>%
#   complete(time = 0:max(time)) %>%
#   filter(time <= 150)
# times <- obs_df$time
# observed <- obs_df$imputed_outpatient
# p_delay <- hist(outpatient_delay_dist(10000),
#                 plot = FALSE, breaks = 0:(max(times)+1)
# )$density
# max_iter = 5
# out_col_name = 'test'
# right_censor = TRUE

#These libraries interfere with select. Load and unload them each time?

get_RL <- function(observed, ## Vector of observed cases. Let L = length(observed)
                             times,    ## Vector of times of observations. Must be length L.
                             p_delay,  ## Vector of probabilities that the delay from infection to observation is 0, 1, ... days. Can be any length <= L.
                             max_iter = 10,
                             max_MCMC = 5, 
                             burn_in_MCMC = 2,
                             out_col_name = 'RL_result',
                             right_censor = TRUE,
                             verbose = FALSE,
                             ww = 0){
  
  ## Check inputs
  stopifnot(is.vector(observed) & is.vector(times) & is.vector(p_delay))
  stopifnot(length(p_delay) <= length(observed))
  #if(length(p_delay)<length(observed)){
  #  p_delay <- c(p_delay, rep(0, length(observed)-length(p_delay))) ## Pad the end of p_delay with 0s so it's the same length as the observations
  #}
  stopifnot(length(observed) == length(times)) #& length(observed) == length(p_delay))
  #if(abs(sum(p_delay)-1) > 1e-6 & verbose){warning('p delay vector does not sum to 1. Extened max delay?')}
  observed <- na_to_0(observed)
  
  ## From the delay multinomial, extract the mean and max delay from infection to observation
  mean_delay <- sum(p_delay * 0:(length(p_delay)-1))
  max_delay <- tail((0:length(p_delay))[p_delay>0], 1)
  orig_min_t <- min(times)
  orig_ceiling <- observed[1]*10
  
  obs_1 <- max_delay+1
  obs_2 <- length(times)+ max_delay
  obs_dat <- observed
  
  ## Get the cumulative probability of a delay <= x days, which we'll use later to adjust for right censoring
  if(right_censor){
    cum_p_delay <- cumsum(p_delay)
  }else{
    cum_p_delay <- p_delay*0+1
  }
  
  ## Infections could have occured up to max_delay days prior to the start of the observed time series. Pad the beginning of the observed time   series and the times vector with zeros as appropriate.
  observed <- c(
    rep(0, max_delay), ## Pad with 0s @ beginning
    observed
  )
  times <- c(
    min(times)-(max_delay:1), ## Insert times prior to first observed
    times
  )
  stopifnot(length(observed) == length(times))
  
  delay_vec <- p_delay
  ## Changed RL_delay from my code because p_delay already starts from zero days
  p <- RL_delay(delay_vec) #delay distribution vector - notation in keeping with previous notation. this changes the delays to be indexed from 0.
  
  
  conv_matrix <- RL_conv_matrix(p, obs_1, obs_2) #toeplitz matrix for carrying out convolution was defined in RL section of code
  true_dat <-  get_RL_curve(obs_dat, p, conv_matrix, max_iter)  #this initializes the MCMC - give it the RL inferred infections since this is our best guess.
  
  backgrd <- 1 #could add nontrivial background counts. nonzero to normalize log in posterior. integer because all counts must be integers. nonzero to avoid pathologies.
  #hyperprior for lambda: gamma, mean is alpha/beta and variance is alpha/beta^2
  beta <- 1
  alpha <- 1
  dt <- 1 #unit of time steps, in days
  lambda0 <- alpha/beta #start with initial value of lambda = mean of hyperprior
  
  max_N <- max_MCMC
  burn_in <- burn_in_MCMC
  
  #initializing MCMC with the RL-inferred incidence curve at time of infection.
  lambda <- lambda0
  curr_u <- true_dat 
  all_u <- matrix(, nrow = max_N, ncol = length(true_dat)) #samples of the $p(\vec{u}\vert \vec{b})$ distribution (with $\lambda$)
  all_lambda <- matrix(, nrow = max_N, ncol = 1) #samples the $\lambda$ distribution.
  
  all_u[1,] <- curr_u
  all_lambda[1,] <- lambda
  
  for (k in c(2:max_N)) {
    #new_u <-  sample_u_given_lambda_b(conv_matrix, obs_dat, curr_u, lambda, beta)
    new_u <-  sample_u_given_lambda_b(conv_matrix, obs_dat, true_dat, lambda, beta) #the covariance matrix is constructed from the RL curve, true_dat
    new_lambda <- sample_lambda_given_u_b(conv_matrix, obs_dat, curr_u, lambda, beta, alpha, true_dat)
    
    all_u[k,] <- new_u
    all_lambda[k,] <- new_lambda
    
    curr_u <- new_u
    lambda <- new_lambda
  }

  data.frame("time" = times[2:length(times)], "samples" = t(all_u[(burn_in+1):max_N,])) #%>% setNames(c('time', out_col_name)) #large matrix: dimensions (number of MCMC samples after burn-in) x (length(observed))
}

get_u_mean <- function (A, b, u, lambda, beta) {
  C <- diag(b+1)
  inv_mean_1 <- t(A) %*% solve(C) %*% A + lambda * LTV(u, beta)
  mean_1 <- solve(inv_mean_1)
  mean_2 <- t(A) %*% solve(C) %*% b
  mean_1 %*% mean_2
}

get_u_covar <- function (A, b, u, lambda, beta) {
  C <- diag(b+1)
  solve(t(A) %*% solve(C) %*% A + lambda * LTV(u, beta))
}

sample_u_given_lambda_b <- function (A, b, u, lambda, beta, alpha) {
  abs(mvrnorm(n = 1, get_u_mean(A, b, u, lambda, beta), get_u_covar(A, b, u, lambda, beta)))
}

sample_lambda_given_u_b <- function (A, b, u, lambda, beta, alpha, ref_u) {
  rgamma(1, length(u)/2 + alpha, 1/2 * t(u) %*% LTV(ref_u, beta) %*% u + beta)
}

diffmat <- function(len) {
  mat <- matrix(0, nrow = len, ncol = len)
  dt <- 1 #timestep is one day; could change in the future
  for(i in c(1:len)){
    if(i==1) {
      mat[i, i] <- -1/dt
      mat[i, i+1] <- 1/dt
    } else if(i==len){
      mat[i, i-1] <- -1/dt
      mat[i, i] <- 1/dt
    } else {
      mat[i, i+1] <- 1/(2*dt)
      mat[i, i-1] <- -1/(2*dt)
    }
  }
  mat
}

LTV <- function(u, beta){
  Dx <- diffmat(length(u))
  phi <- 1/(sqrt((Dx %*% u)^2) + beta) #the matrix will no longer be constant along the diagonal, but will weight smoother locations higher
  phimat <- diag(c(phi)) 
  t(Dx) %*% phimat %*% Dx  
}

RL_delay <- function(delay_distr){
  #zero_indexed_distr <- c(10^(-8), ifelse(delay_distr_vec==0, 10^(-8), delay_distr_vec)) #can't have exactly 0 probabilities
  zero_indexed_distr <- ifelse(delay_distr==0, 10^(-8), delay_distr) #can't have exactly 0 probabilities; the delay_distr vec we are handing this function is already zero-indexed
  zero_indexed_distr*sum(delay_distr)/sum(zero_indexed_distr) #keeps fraction of cases observed the same 
}

RL_conv_matrix <- function(delay_distr, obs_1, obs_2) {
  p <- delay_distr
  pmat <- circulant(c(p, rep(0, obs_2)))
  t(pmat[(obs_1-length(p)+1):obs_2, obs_1:obs_2])
}

get_RL_curve <- function(obs_curve, delay_distr, conv_matrix, max_it){
  d_obs <- obs_curve
  p <- delay_distr
  u_obs_guess <- c(d_obs, rep(max(d_obs[length(d_obs)],1), length(p)-1))
  
  p_ij_obs <- conv_matrix
  
  q_j <- colSums(p_ij_obs)
  dim_p_ij <- dim(p_ij_obs)
  p_ij_obs_rescaled <- p_ij_obs / matrix(q_j,nrow=dim_p_ij[1],ncol=dim_p_ij[2],byrow=TRUE)
  
  u_obs_guess_rescaled <- u_obs_guess * q_j
  d_obs_rescaled <- d_obs * q_j[(length(p)):length(u_obs_guess_rescaled)]
  
  u_obs_rescaled <- u_obs_guess_rescaled
  ind <- 1
  
  ## Stopping condition: Write a funtion to get the chi2 statistic comparing observed and expected deaths per day
  get_chisq <- function(obs, exp){
    ## If right censoring, drop dates on which the probability of observation is <50% from the stopping condition
    drop_me <- sum(cum_p_delay < 0.5)
    nn = length(obs)-drop_me
    obs = obs[1:nn]
    exp = exp[1:nn]
    ## Get chi2 statistic using reliable values
    1/length(obs) * sum(ifelse(exp == 0, 1, (obs-exp)^2/(exp)))
  }
  while (ind < max_it) {
    c_obs <-p_ij_obs_rescaled %*% u_obs_rescaled
    new_kernel_obs <- d_obs/c_obs
    new_u_obs_rescaled <- u_obs_rescaled * t(t(new_kernel_obs) %*% p_ij_obs_rescaled)
    u_obs_rescaled <- new_u_obs_rescaled
    ind <- ind+1
  }
  u_obs_new <- u_obs_rescaled/q_j
}


## From supplement of Cori et al.
get_discrete_lognormal <- function(k, mu, sigma){
  ff <- function(xx, mu, sigma){xx*dlnorm(xx, mu, sigma)}
  sapply(X = k, FUN = function(k, mu, sigma){
    (1+k)*plnorm(k+1, mu, sigma) -
      2*k*plnorm(k, mu, sigma) +
      (k-1)*plnorm(k-1, mu, sigma)+
      integrate(f = ff, lower = k-1, upper = k, mu = mu, sigma = sigma)$value-
      integrate(f = ff, lower = k, upper = k+1, mu = mu, sigma = sigma)$value
  }, mu = mu, sigma = sigma)
}
