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
  rename(region = restore_region) %>%
  filter(region != 'unknown') %>%
  rename(date = specimen_collection_date) %>%
  group_by(date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  ungroup() %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .5)$y %>% min_0) %>%
  filter(date <= max(date))


## More up to date, public linelist
raw_dat <- read_csv('../data/idph_public_restore_region.csv') %>%
  rename(region = restore_region) %>%
  filter(region != 'unknown') %>%
  group_by(date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
         region = toupper(region),
         region = ifelse(region=='NORTH-CENTRAL', 'NORTHCENTRAL', region)) %>%
  ungroup() 
overall_dat <- raw_dat %>%
  filter(region != 'CHICAGO') %>%
  group_by(date) %>%
  summarise(region = 'IL_Overall',
            new_cases = sum(new_cases))
dat <- bind_rows(raw_dat, overall_dat) %>%
  ungroup() %>% group_by(region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .6)$y %>% min_0,
         avged = zoo::rollmean(new_cases, k = 7, fill = c(new_cases[1], NA, new_cases[length(new_cases)]))) %>%
  filter(date <= max(date))
  

## Compare the public and idph linelists
bind_rows(list(linelist = dat1, public = dat), .id = 'dataset') %>%
  ggplot()+
  geom_line(aes(x = date, y = new_cases, color = dataset))+
  #geom_line(aes(x = date, y = smoothed, color = dataset))+
  facet_wrap(.~region, scales = 'free_y')+
  ggtitle('idph cases')

## Visualize the case counts by restore region. (4 regions)
dat %>%
  ggplot()+
  geom_line(aes(x = date, y = new_cases))+
  geom_line(aes(x = date, y = smoothed), color = 'red')+
  geom_line(aes(x = date, y = avged), color = 'blue')+
  facet_wrap(.~region, scales = 'free_y')+
  ggtitle('idph cases - public linelist')
ggsave(sprintf('../figs/%s/cases_restore_region.png', Sys.Date()))

## Load the case counts from the public line list by covid region (11 regions)
raw_dat_cr <- read_csv('../data/idph_public_covid_region.csv')  %>%
  rename(region = new_restore_region) %>%
  filter(region != 'unknown') %>%
  group_by(date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
         region = toupper(region)) 
overall_dat_cr <- raw_dat_cr %>%
  group_by(date) %>%
  summarise(region = 'IL_Overall',
            new_cases = sum(new_cases))
dat_11r <- bind_rows(raw_dat_cr, overall_dat_cr)%>%
  ungroup() %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .6)$y %>% min_0,
         roll_avg_7d = zoo::rollmean(new_cases, k = 7, fill = c(new_cases[1], NA, new_cases[length(new_cases)]))) %>%
  filter(date <= max(date))

dat_11r %>%
  pivot_longer(c(new_cases, smoothed, roll_avg_7d)) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  facet_wrap(.~region, scales = 'free_y')+
  scale_color_manual("", values = c('black', 'red', 'yellow'))+
  theme(legend.position = 'bottom')+
  ggtitle('idph cases - public linelist')
ggsave(sprintf('../figs/%s/cases_covid_region.png', Sys.Date()))




## Estimate Rt by region ------------------------------
rt_by_region <- function(rr, dat){
  cat(sprintf('restore region is %s\n', rr))
  out = full_rt_pipeline(df = dat %>% filter(region == rr), 
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
## 1. by restore(4) region ------------------------------
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
restore_region_estimates <- lapply(unique(dat$region), rt_by_region, dat = dat) 
names(restore_region_estimates)  = unique(dat$region)

pdf(file = sprintf('../figs/%s/restore_region_rt_from_idph_cases.pdf', Sys.Date()))
lapply(restore_region_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()

## 2. by covid (11) region ------------------------------
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
covid_region_estimates <- lapply(unique(dat_11r$region), rt_by_region, dat = dat_11r) 
names(covid_region_estimates)  = unique(dat_11r$region)

pdf(file = sprintf('../figs/%s/covid_region_rt_from_idph_cases.pdf', Sys.Date()))
lapply(covid_region_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()

# 
#  
# ## Estimate Rt overall ----------------------------------
# rt_overall <- function(rr){
#   sprintf('restore region is %s', rr)
#  out = full_rt_pipeline(df = dat %>% filter(region != 'CHICAGO') %>% group_by(date) %>% 
#                           summarise(smoothed = sum(smoothed, na.rm = T)), 
#                    obscolname ='smoothed',
#                    p_obs = .9,
#                    delay_pars = read_rds('../data/fitted_delays/delay_infection_to_test_posterior.rds') %>% bind_cols %>% select(1:2) %>% bind_cols %>% select(1:2),
#                    delay_type = 'lognormal',
#                    gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
#                    nboot = 25, 
#                    ttl = 'IL Overall', obs_type = 'cases')
#  sprintf('%s - done\n', rr)
# return(out)
# }
# 
# overall_estimates <- rt_overall(dat)
# 
# ## Plot
# cowplot::plot_grid(overall_estimates$upscale_plot + theme(legend.position = c(.8, .8)), overall_estimates$rt_plot, ncol = 1)
# ggsave(width = 4, height = 3, filename = sprintf('../figs/%s/Illinois_rt_from_idph_cases.png', Sys.Date()), dpi = 300)
# 







## Estimate using the raw, shifted tim series ----------------------------
cori_by_region <- function(rr, dat){
  
  if(rr != 'overall'){
    ## Subset and reformat the data frame
    ins <- filter(dat, region == rr) %>% 
      ungroup() %>%
      arrange(date) %>%
      mutate(time = 1:nrow(.)) # Create a numeric time column
  }else{
    ins <- filter(dat, region != 'CHICAGO') %>% 
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

restore_region_cori<- lapply(unique(dat$region), FUN = cori_by_region, dat = dat) %>%
  bind_rows %>%
  rename(rt.lower = rt.025,
         rt.upper = rt.975)
covid_region_cori<- lapply(unique(dat_11r$region), FUN = cori_by_region, dat = dat_11r) %>%
  bind_rows %>%
  rename(rt.lower = rt.025,
         rt.upper = rt.975)
#overall_cori <- cori_by_region('overall', dat = dat)




## Save results ------------------------------------
write_csv(lapply(restore_region_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/cases_restore_region.csv', Sys.Date()))
write_csv(lapply(covid_region_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/cases_covid_region.csv', Sys.Date()))
write_csv(restore_region_cori, sprintf('../figs/%s/shifted_cases_restore_region.csv', Sys.Date()))
write_csv(covid_region_cori, sprintf('../figs/%s/shifted_cases_covid_region.csv', Sys.Date()))
#write_csv(overall_cori, sprintf('../figs/%s/shifted_cases_overall.csv', Sys.Date()))
#write_csv(overall_estimates$df, sprintf('../figs/%s/cases_overall.csv', Sys.Date()))
write_rds(restore_region_estimates, sprintf('../figs/%s/restore_region_estimates_cases.rds', Sys.Date()))
write_rds(covid_region_estimates, sprintf('../figs/%s/covid_region_estimates_cases.rds', Sys.Date()))
#write_rds(overall_estimates, sprintf('../figs/%s/overall_estimates_cases.rds', Sys.Date()))
#write_csv(dat, sprintf('../figs/%s/dat_cases.csv', Sys.Date()))

# 
# ## Estimate rt using epinow2
# library('EpiNow2')
# get_overall <- function(dat){
#   dat %>% filter(restore_region != 'CHICAGO') %>% 
#     group_by(date) %>% 
#     summarise(new_cases = sum(new_cases),
#               smoothed = sum(smoothed))
# }
# run_epinow2(dat_df = get_overall(dat), 
#             obs_colname = 'new_cases', 
#             dat_type = 'cases', 
#             prior_smoothing_window = 7, 
#             output_folder = 'test_epinow2')