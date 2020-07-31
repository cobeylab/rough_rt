## Estimate Rt from incident cases

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
source('../code/epinow2.R')




## Load data --------------------------------------------
min_0 <- function(xx){ifelse(xx<0, 0, xx)}

## IDPH linelist
## Often dealys to reporting at end of time series
dat1 <- read_csv('../data/idph_cases_timseries.csv') %>%
  filter(restore_region != 'unknown') %>%
  rename(date = specimen_collection_date) %>%
  group_by(date, restore_region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  ungroup() %>%
  group_by(restore_region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .5)$y %>% min_0) %>%
  filter(date <= max(date))
# dat1 %>%
#   ggplot()+
#   geom_line(aes(x = date, y = new_cases))+
#   geom_line(aes(x = date, y = smoothed), color = 'red')+
#   facet_wrap(.~restore_region, scales = 'free_y')+
#   ggtitle('idph cases')

## More up to date, public linelist
dat <- read_csv('../data/idph_public_cases.csv') %>%
  filter(restore_region != 'unknown') %>%
  group_by(date, restore_region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
         restore_region = toupper(restore_region),
         restore_region = ifelse(restore_region=='NORTH-CENTRAL', 'NORTHCENTRAL', restore_region)) %>%
  ungroup() %>%
  group_by(restore_region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .5)$y %>% min_0) %>%
  filter(date <= max(date))

bind_rows(list(linelist = dat1, public = dat), .id = 'dataset') %>%
  ggplot()+
  geom_line(aes(x = date, y = new_cases, color = dataset))+
  #geom_line(aes(x = date, y = smoothed, color = dataset))+
  facet_wrap(.~restore_region, scales = 'free_y')+
  ggtitle('idph cases')

dat %>%
  ggplot()+
  geom_line(aes(x = date, y = new_cases))+
  geom_line(aes(x = date, y = smoothed), color = 'red')+
  facet_wrap(.~restore_region, scales = 'free_y')+
  ggtitle('idph cases - public linelist')
ggsave(sprintf('../figs/%s/cases.png', Sys.Date()))

## Estimate Rt by region ------------------------------
rt_by_region <- function(rr){
  sprintf('restore region is %s', rr)
  out = full_rt_pipeline(df = dat %>% filter(restore_region == rr), 
                         obscolname ='smoothed',
                         p_obs = .9,
                         delay_pars = read_rds('../data/fitted_delays/delay_infection_to_test_posterior.rds') %>% bind_cols %>% select(1:2),
                         delay_type = 'lognormal',
                         gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                         nboot = 25, 
                         ttl = rr, 
                         obs_type = 'cases')
  sprintf('%s - done\n', rr)
  return(out)
}
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
regional_estimates <- lapply(unique(dat$restore_region), rt_by_region) 
names(regional_estimates)  = unique(dat$restore_region)

pdf(file = sprintf('../figs/%s/Regional_rt_from_idph_cases.pdf', Sys.Date()))
lapply(regional_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()

 
## Estimate Rt overall ----------------------------------
rt_overall <- function(rr){
  sprintf('restore region is %s', rr)
 out = full_rt_pipeline(df = dat %>% filter(restore_region != 'CHICAGO') %>% group_by(date) %>% 
                          summarise(smoothed = sum(smoothed, na.rm = T)), 
                   obscolname ='smoothed',
                   p_obs = .9,
                   delay_pars = read_rds('../data/fitted_delays/delay_infection_to_test_posterior.rds') %>% bind_cols %>% select(1:2) %>% bind_cols %>% select(1:2),
                   delay_type = 'lognormal',
                   gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                   nboot = 25, 
                   ttl = 'IL Overall', obs_type = 'cases')
 sprintf('%s - done\n', rr)
return(out)
}

overall_estimates <- rt_overall(dat)

## Plot
cowplot::plot_grid(overall_estimates$upscale_plot + theme(legend.position = c(.8, .8)), overall_estimates$rt_plot, ncol = 1)
ggsave(width = 4, height = 3, filename = sprintf('../figs/%s/Illinois_rt_from_idph_cases.png', Sys.Date()), dpi = 300)








## Estimate using the raw, shifted tim series ----------------------------
cori_by_region <- function(rr){
  
  if(rr != 'overall'){
    ## Subset and reformat the data frame
    ins <- filter(dat, restore_region == rr) %>% 
      ungroup() %>%
      arrange(date) %>%
      mutate(time = 1:nrow(.)) # Create a numeric time column
  }else{
    ins <- filter(dat, restore_region != 'CHICAGO') %>% 
      group_by(date) %>%
      summarise(smoothed = sum(smoothed)) %>%
      ungroup() %>%
      arrange(date) %>%
      mutate(time = 1:nrow(.)) # Create a numeric time column
    
  }
   
  ## Calculate the appropriate window size
  ##   This is kind of arbitrary, but it scales with the 20th percentile of daily sample size
  ww = max(1, floor(50/quantile(filter(ins, smoothed>0)$smoothed, .2)))
  cat(sprintf('\nregion is %s, window is %.0f\n', rr, ww))
  
  ## Calculate the mean delay
  md = read_rds('../data/fitted_delays/delay_infection_to_test_posterior.rds') %>% bind_cols %>% select(1:2) %>% 
    mutate(mean = exp(mu+sigma^2/2)) %>% 
    pull(mean) %>% 
    mean
  
  ## Estimate Rt and merge with the original data frame for the region
  merge(
    ins,
    get_cori(ins, 
             obs_col_name = 'smoothed', 
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




## Save results ------------------------------------
write_csv(lapply(regional_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('csv/cases_regional_%s.csv', Sys.Date()))
write_csv(regional_cori, sprintf('csv/shifted_cases_regional_%s.csv', Sys.Date()))
write_csv(overall_cori, sprintf('csv/shifted_cases_overall_%s.csv', Sys.Date()))
write_csv(overall_estimates$rt_ests$summary, sprintf('csv/cases_overall_%s.csv', Sys.Date()))
write_rds(regional_estimates, sprintf('csv/regional_estimates_cases_%s.rds', Sys.Date()))
write_rds(overall_estimates, sprintf('csv/overall_estimates_cases_%s.rds', Sys.Date()))
write_csv(dat, sprintf('csv/dat_cases_%s.csv', Sys.Date()))


## Estimate rt using epinow2
library('EpiNow2')
get_overall <- function(dat){
  dat %>% filter(restore_region != 'CHICAGO') %>% 
    group_by(date) %>% 
    summarise(new_cases = sum(new_cases),
              smoothed = sum(smoothed))
}
run_epinow2(dat_df = get_overall(dat), 
            obs_colname = 'new_cases', 
            dat_type = 'cases', 
            prior_smoothing_window = 7, 
            output_folder = 'test_epinow2')
