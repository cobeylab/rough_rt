## Estimate Rt from incident cases

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(EpiEstim)
theme_set(theme_bw())

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/deconvolve.R')
source('../code/Richardson_Lucy.R')
source('../code/upscale.R')
source('../code/rt_boot.R')



## Load data --------------------------------------------
min_0 <- function(xx){ifelse(xx<0, 0, xx)}
dat <- read_csv('../data/idph_cases_timseries.csv') %>%
  filter(restore_region != 'unknown') %>%
  rename(date = specimen_collection_date) %>%
  group_by(date, restore_region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  ungroup() %>%
  group_by(restore_region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .5)$y %>% min_0) %>%
  filter(date <= max(date)-14)

dat %>%
  ggplot()+
  geom_line(aes(x = date, y = new_cases))+
  geom_line(aes(x = date, y = smoothed), color = 'red')+
  facet_wrap(.~restore_region, scales = 'free_y')+
  ggtitle('idph cases')
ggsave(sprintf('../figs/cases_%s.png', Sys.Date()))


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

pdf(file = sprintf('../figs/%s-Regional_rt_from_idph_cases.pdf', Sys.Date()))
lapply(regional_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()

 
## Estimate Rt overall ----------------------------------
rt_overall <- function(rr){
  sprintf('restore region is %s', rr)
 out = full_rt_pipeline(df = dat %>% group_by(date) %>% 
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
ggsave(width = 4, height = 3, filename = sprintf('../figs/%s-Illinois_rt_from_idph_cases.png', Sys.Date()), dpi = 300)


## Save results ------------------------------------
write_csv(lapply(regional_estimates, function(ll) ll$rt_ests$summary) %>% bind_rows(.id = 'region'), sprintf('csv/cases_regional_%s.csv', Sys.Date()))
write_csv(overall_estimates$rt_ests$summary, sprintf('csv/cases_overall_%s.csv', Sys.Date()))
write_rds(regional_estimates, sprintf('csv/regional_estimates_cases_%s.rds', Sys.Date()))
write_rds(overall_estimates, sprintf('csv/overall_estimates_cases_%s.rds', Sys.Date()))
