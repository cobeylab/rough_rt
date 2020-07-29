## Estimate Rt using incident deaths
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


## Load data
dat <- read_csv('../data/idph_linelist_timeseries.csv') %>%
  filter(restore_region != 'unknown') %>%
  group_by(date, restore_region) %>%
  summarise(new_deaths = sum(new_deaths))
dat %>%
  ggplot()+
  geom_line(aes(x = date, y = new_deaths))+
  facet_wrap(.~restore_region, scales = 'free_y')




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

pdf(file = sprintf('../figs/%s-Regional_rt_from_idph_deaths.pdf', Sys.Date()))
lapply(regional_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = 'bottom'), ll$rt_plot, ncol = 1)) 
dev.off()





## Estimate Rt overall --------------------------------------------
rt_overall <- function(rr){
  sprintf('restore region is %s', rr)
 out = full_rt_pipeline(df = dat %>% group_by(date) %>% 
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
ggsave(width = 4, height = 3, filename = sprintf('../figs/%s-Illinois_rt_from_idph_deaths.png', Sys.Date()), dpi = 300)



## Save results -------------------------------------------------------
write_csv(lapply(regional_estimates, function(ll) ll$rt_ests$summary) %>% bind_rows(.id = 'region'), sprintf('csv/deaths_regional_%s.csv', Sys.Date()))
write_csv(overall_estimates$rt_ests$summary, sprintf('csv/deaths_overall_%s.csv', Sys.Date()))
write_rds(regional_estimates, sprintf('csv/regional_estimates_deaths_%s.rds', Sys.Date()))
write_rds(overall_estimates, sprintf('csv/overall_estimates_deaths_%s.rds', Sys.Date()))
