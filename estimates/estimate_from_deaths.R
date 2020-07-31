## Estimate Rt using incident deaths
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(EpiEstim)
theme_set(theme_bw())
if(!dir.exists(sprintf('../figs/%s/', Sys.Date()))) dir.create(sprintf('../figs/%s/', Sys.Date()))

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/deconvolve.R')
source('../code/Richardson_Lucy.R')
source('../code/upscale.R')
source('../code/rt_boot.R')


## Load data
dat <- read_csv('../data/idph_linelist_timeseries.csv') %>%
  filter(restore_region != 'unknown') %>%
  group_by(date, restore_region) %>%
  summarise(new_deaths = sum(new_deaths))
dat %>%
  ggplot()+
  geom_line(aes(x = date, y = new_deaths))+
  facet_wrap(.~restore_region, scales = 'free_y')
ggsave(sprintf('../figs/%s/deaths.png', Sys.Date()))



## Estimate Rt by region -----------------------------------------
rt_by_region <- function(rr){
  sprintf('restore region is %s', rr)
  out = full_rt_pipeline(df = dat %>% filter(restore_region == rr), 
                         obscolname ='new_deaths',
                         p_obs = .9,
                         delay_pars = read_rds('../data/fitted_delays/Linton_lognorm_sample.rds') %>% bind_cols %>% select(1:2),
                         delay_type = 'lognormal',
                         gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                         nboot = 25, 
                         ttl = rr)
  sprintf('%s - done\n', rr)
  return(out)
}

# Takes a few minutes to run, depending on size of nboot
# Can't be parallelized because internal operations are already running in parallel
regional_estimates <- lapply(unique(dat$restore_region), rt_by_region) 
names(regional_estimates)  = unique(dat$restore_region)

pdf(file = sprintf('../figs/%s/Regional_rt_from_idph_deaths.pdf', Sys.Date()))
lapply(regional_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = 'bottom'), ll$rt_plot, ncol = 1)) 
dev.off()




## Estimate using the raw, shifted tim series ----------------------------
  # Visualize the distribution of mean delays
read_rds('../data/fitted_delays/Linton_lognorm_sample.rds') %>% bind_cols %>% select(1:2) %>% 
  mutate(mean = exp(mu+sigma^2/2)) %>% 
  pull(mean) %>% 
  mean

cori_by_region <- function(rr){
  
  if(rr != 'overall'){
  ## Subset and reformat the data frame
  ins <- filter(dat, restore_region == rr) %>% 
    ungroup() %>%
    arrange(date) %>%
    mutate(time = 1:nrow(.)) # Create a numeric time column
  }else{
    ins <- filter(dat, restore_region != 'chicago') %>% 
      group_by(date) %>%
      summarise(new_deaths = sum(new_deaths)) %>%
      ungroup() %>%
      arrange(date) %>%
      mutate(time = 1:nrow(.)) # Create a numeric time column
    
  }
  
  ## Calculate the appropriate window size
  ##   This is kind of arbitrary, but it scales with the 20th percentile of daily sample size
  ww = floor(50/max(1, quantile(filter(ins, new_deaths>0)$new_deaths, .2)))
  cat(sprintf('\nregion is %s, window is %.0f\n', rr, ww))
  
  ## Calculate the mean delay
  md = read_rds('../data/fitted_delays/Linton_lognorm_sample.rds') %>% bind_cols %>% select(1:2) %>% 
    mutate(mean = exp(mu+sigma^2/2)) %>% 
    pull(mean) %>% 
    mean
  
  ## Estimate Rt and merge with the original data frame for the region
  merge(
    ins,
    get_cori(ins, 
             obs_col_name = 'new_deaths', 
             window = ww, 
             out_name = 'rt',
             mean_delay = md, 
             SI_mean = 4.5, ## Rough estimates from Ganyani et al
             SI_var = 1.7,   
             wend = F),
    by = 'time'
  ) %>%
    select(-time) %>% ## Drop the arbitrary time vector
    mutate(window = ww,
           mean_delay = md) ## Create a column to record the window size
}

regional_cori<- lapply(unique(dat$restore_region), FUN = cori_by_region) %>%
  bind_rows %>%
  rename(rt.lower = rt.025,
         rt.upper = rt.975)
overall_cori <- cori_by_region('overall')




## Estimate Rt overall --------------------------------------------
rt_overall <- function(rr){
  sprintf('restore region is %s', rr)
 out = full_rt_pipeline(df = dat %>% filter(restore_region != 'chicago') %>% group_by(date) %>% 
                          summarise(new_deaths = sum(new_deaths, na.rm = T)), 
                  obscolname ='new_deaths',
                   p_obs = .9,
                   delay_pars = read_rds('../data/fitted_delays/Linton_lognorm_sample.rds') %>% bind_cols %>% select(1:2) %>% bind_cols %>% select(1:2),
                   delay_type = 'lognormal',
                   gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                   nboot = 25, 
                   ttl = 'IL Overall')
 sprintf('%s - done\n', rr)
return(out)
}
overall_estimates <- rt_overall(dat)

## Plot
cowplot::plot_grid(overall_estimates$upscale_plot + theme(legend.position = c(.8, .8)), overall_estimates$rt_plot, ncol = 1)
ggsave(width = 4, height = 3, filename = sprintf('../figs/%s/Illinois_rt_from_idph_deaths.png', Sys.Date()), dpi = 300)



## Save results -------------------------------------------------------
write_csv(lapply(regional_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('csv/deaths_regional_%s.csv', Sys.Date()))
write_csv(regional_cori, sprintf('csv/shifted_deaths_regional_%s.csv', Sys.Date()))
write_csv(overall_cori, sprintf('csv/shifted_deaths_overall_%s.csv', Sys.Date()))
write_csv(overall_estimates$rt_ests$summary, sprintf('csv/deaths_overall_%s.csv', Sys.Date()))
write_rds(regional_estimates, sprintf('csv/regional_estimates_deaths_%s.rds', Sys.Date()))
write_rds(overall_estimates, sprintf('csv/overall_estimates_deaths_%s.rds', Sys.Date()))
write_csv(dat, sprintf('csv/dat_deaths_%s.csv', Sys.Date()))
