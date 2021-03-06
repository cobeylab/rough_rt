## Utility functions for simulating data and estimating R0


## For generating synthetic data ----------------------------------------

## Output underlying R0 values for input into the SIR or SEIR model across once change (increase or decrease)
get_arnaught_step <- function(start_R0, 
                                  final_R0, 
                                  start_change,  ## Time at which R0 first departs from its initial value, start_R0
                                  end_change,   ## Time at which R0 first reaches its final value, final_R0
                                  n_t){  ## total timesteps
  arnaught <- if(start_R0==final_R0){
    #constant arnaught
    start_R0
  } else if(start_change==end_change){
    #step function arnaught
    swap_time <- start_change + 1
    c(rep(start_R0, swap_time), rep(final_R0, n_t-swap_time))

  } else {
    #linearly decreasing intervention arnaught
    c(rep(start_R0, start_change), seq(start_R0, final_R0, length.out=(end_change-start_change+1)), rep(final_R0, n_t - end_change))
  } 
}

## Repeatedly call get_arnaught step to specify R0 across an arbitrary number of changes
specify_arnaught <- function(R0_vec, ## Vector of equlibrium R0 values. Should be 1 greater in length than the desired number of changes.
                             change_start_vec, ## Vector of timepoints at which R0 starts to change. Should be length(R0_vec) - 1
                             change_end_vec, ## Vector of timepoints at which R0 first reaches its new value. Should be length(R0_vec) - 1
                             NT){ ## Scalar - total number of timepoints.
  stopifnot(length(change_end_vec) == length(change_start_vec))
  stopifnot(length(change_start_vec) == length(R0_vec)-1)
  stopifnot(all(diff(change_start_vec)>0))
  stopifnot(all(diff(change_end_vec)>0))
  arnaught <- NULL
  n.changes <- length(R0_vec)-1
  breakpoints <- c(0, change_start_vec[-1]-1, NT+1)
  stopifnot(all(breakpoints[-1]>change_end_vec))
  for(ii in 1:n.changes){
    arnaught <- c(arnaught,
                  get_arnaught_step(start_R0 = R0_vec[ii], 
                                  final_R0 = R0_vec[ii+1], 
                                  start_change = change_start_vec[ii]-breakpoints[ii], 
                                  end_change = change_end_vec[ii]-breakpoints[ii], 
                                  n_t = diff(breakpoints)[ii]-1)
    )
  }
  #cbind(arnaught, 0:NT)
  arnaught
}
# # Test 1
# specify_arnaught(R0_vec = c(2.5, .7, 1.1), change_start_vec = 45+c(0, 45), change_end_vec = 45+c(0, 45)+7, NT = 150)
# 
# ## Test 2
# specify_arnaught(R0_vec = c(2.5, .7, 1.1, 2.0), change_start_vec = 45+c(0, 45, 45+30), change_end_vec = 45+c(0, 45, 45+30)+7, NT = 150) -> test
# plot(0:150, test)
# abline(v = c(45+c(0, 45, 45+30)), lty = 2, col = 'red')
# abline(v = c(45+7+c(0, 45, 45+30)), lty = 2, col = 'red')
# 
# # Test 3 - decreasing change points, should throw error
# specify_arnaught(R0_vec = c(2.5, .7, 1.1, 2.0), change_start_vec = 45+c(0, 45, 30), change_end_vec = 45+c(0, 45, 45+30)+7, NT = 150)
# 
# #Test 4 - endpoint is after breakpoint - should throw error
# specify_arnaught(R0_vec = c(2.5, .7, 1.1, 2.0), change_start_vec = 45+c(0, 45, 90), change_end_vec = 45+c(0, 45, 45+30)+50, NT = 150)



## Wrappers that call simulate_sir defined in simulation.R
simulate_sir_example <- function(
  arnaught, t_I, N, I_init, n_t, n_steps_per_t = 10,
  method = 'stochastic'
) {
  simulate_seir(
    arnaught = arnaught,
    t_E = 0,
    t_I = t_I,
    N = N,
    S_init = N - I_init,
    E_init = 0,
    I_init = I_init,
    n_t = n_t, n_steps_per_t = n_steps_per_t,
    method = method
  )
}


## Wrappers that call simulate_seir defined in simulation.R
simulate_seir_example <- function(
  arnaught, t_E, t_I, N, E_init, I_init, n_t, n_steps_per_t = 10,
  method = 'stochastic'
) {
  simulate_seir(
    arnaught = arnaught,
    t_E = t_E,
    t_I = t_I,
    N = N,
    S_init = N - E_init - I_init,
    E_init = E_init,
    I_init = I_init,
    n_t = n_t, n_steps_per_t = n_steps_per_t,
    method = method
  )
}


## Function to load saved outputs form a simulation run
load_sims_for_one_R0 <-  function(arnaught, model_type = 'seir', method = 'stochastic'){
  data.frame(fns = list.files(path = sprintf('R0-%.1f', arnaught))) %>%
    filter(grepl(fns, pattern = method)) %>%
    pull(fns) %>%
    lapply(FUN = function(xx){
      readRDS(paste0(sprintf('R0-%.1f/', arnaught), xx)) -> tmp
      tmp$sim_df %>% 
        mutate(int_time = as.character(tmp$intervention_time), 
               dec_dur = as.character(tmp$decrease_duration))
    }) %>%
    bind_rows()
}



## Write a function to extract the simulation results as a data frame
get_sim_df <- function(method = 'ode'){  ## can also be 'stochastic'
  readRDS(sprintf('R0-%.1f/seir_%s_dec%.0f-%.0f_sim.rds', 
                  parlist$pre_intervention_R0, 
                  method,
                  parlist$intervention_time_1, 
                  parlist$days_intervention_to_min))$sim_df %>%
    mutate_all(.funs = function(xx){ifelse(is.na(xx), 0, xx)}) %>%
    mutate(incidence = round(dS))
}


## Function to replace NAs with 0s in simulation output
na_to_0 <- function(vec){
  if(any(is.na(vec))){
    warning(sprintf('WARNING: Replacing NAs in %s with 0s\n', deparse(substitute(vec))))
    vec[is.na(vec)] = 0
  }
  vec
}





## Wrapper to save a png using ggsave, without having to specify units and dpi
gg_png <- function(ww, ## width (in)
                   hh, ## height (in)
                   fn, ## filename
                   pp = last_plot()){ ## name of plot to save. default is the last plot in the working device
  ggsave(filename = fn, width =  ww, height = hh, plot = pp, units = 'in', dpi = 300, device = png())
}


min_0 <- function(xx) ifelse(xx<0, 0, xx)



get_shape <- function(mean, vv){
  rate = mean/vv
  shape = mean*rate
  shape
}

get_rate <- function(mean, vv){
  rate = mean/vv
  shape = mean*rate
  rate
}

dir_check <- function(dirname) if(!dir.exists(dirname)) dir.create(dirname)

date_to_num <- function(xx){
  as.numeric(as.Date(xx, origin = '1970-01-01'))
}

num_to_date <- function(xx){
  as.Date(xx, origin = '1970-01-01')
}
