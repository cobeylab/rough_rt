## Estimate Rt from incident hospitalizations

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
dat <- read_csv('../data/idph_cli_admissions_ts.csv') %>%
  filter(restore_region != 'unknown') %>%
  mutate(date = epi_week_start) %>%
  group_by(date, restore_region) %>%
  summarise(n_admissions = sum(n_admissions)) %>%
  ungroup() %>%
  group_by(restore_region) %>%
  arrange(date) 

dat %>%
  ggplot()+
  geom_line(aes(x = date, y = n_admissions))+
  facet_wrap(.~restore_region, scales = 'free_y')+
  ggtitle('idph hospital cli')
ggsave(sprintf('../figs/hospital_cli_data_%s.png', Sys.Date()))


## Estimate Rt by region ------------------------------
rt_by_region <- function(rr){
  sprintf('restore region is %s', rr)
  out = full_rt_pipeline(df = dat %>% filter(restore_region == rr), 
                         obscolname ='n_admissions',
                         p_obs = .9,
                         delay_pars = read_rds('../data/fitted_delays/delay_infection_to_hosp_posterior.rds') %>% bind_cols %>% select(1:2),
                         delay_type = 'lognormal',
                         gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                         nboot = 25, 
                         ttl = rr, 
                         obs_type = 'hospitalizations')
  sprintf('%s - done\n', rr)
  return(out)
}
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
regional_estimates <- lapply(unique(dat$restore_region), rt_by_region) 
names(regional_estimates)  = unique(dat$restore_region)

pdf(file = sprintf('../figs/%s-Regional_rt_from_idph_hospital_cli.pdf', Sys.Date()))
lapply(regional_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()

 
## Estimate Rt overall ----------------------------------
rt_overall <- function(rr){
  sprintf('restore region is %s', rr)
 out = full_rt_pipeline(df = dat %>% group_by(date) %>% 
                          summarise(n_admissions = sum(n_admissions, na.rm = T)), 
                   obscolname ='n_admissions',
                   p_obs = .9,
                   delay_pars = read_rds('../data/fitted_delays/delay_infection_to_hosp_posterior.rds') %>% bind_cols %>% select(1:2) %>% bind_cols %>% select(1:2),
                   delay_type = 'lognormal',
                   gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                   nboot = 25, 
                   ttl = 'IL Overall', obs_type = 'hospitalizations')
 sprintf('%s - done\n', rr)
return(out)
}

overall_estimates <- rt_overall(dat)

## Plot
cowplot::plot_grid(overall_estimates$upscale_plot + theme(legend.position = c(.8, .8)), overall_estimates$rt_plot, ncol = 1)
ggsave(width = 4, height = 3, filename = sprintf('../figs/%s-Illinois_rt_from_idph_hospitalizations.png', Sys.Date()), dpi = 300)


## Save results ------------------------------------
write_csv(lapply(regional_estimates, function(ll) ll$rt_ests$summary) %>% bind_rows(.id = 'region'), sprintf('csv/hospitalizations_regional_%s.csv', Sys.Date()))
write_csv(overall_estimates$rt_ests$summary, sprintf('csv/hospitalizations_overall_%s.csv', Sys.Date()))
write_rds(regional_estimates, sprintf('csv/regional_estimates_hospitalizations_%s.rds', Sys.Date()))
write_rds(overall_estimates, sprintf('csv/overall_estimates_hospitalizations_%s.rds', Sys.Date()))

