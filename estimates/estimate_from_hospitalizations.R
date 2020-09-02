## Estimate Rt from incident cases

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(EpiEstim)
theme_set(theme_bw())

## Set the type of smoothing ----------------------------
ts_colname = 'nadmit'
#ts_colname = 'smoothed'
cat(sprintf('Estimating from %s raw data', ts_colname))

## Set the name of the output directory ---------------------------------
out_dir <- paste('hospitalizations', ts_colname, Sys.Date(), sep = '_')
if(!dir.exists(sprintf('../figs/%s/', out_dir))) dir.create(sprintf('../figs/%s/', out_dir))

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/upscale.R')
source('../code/rt_boot.R')







## Load data --------------------------------------------
source('../code/load_timeseries.R')
dat <- load_EPIC_admissions()
## Visualize the case counts by restore region. (4 regions)
dat %>%
  pivot_longer(c(nadmit, smoothed, avg_7d)) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  scale_color_manual('', values = c('salmon', 'black', 'dodgerblue'), labels = c('7d average', 'observed', 'smoothed'))
  ggtitle('idph cases - public linelist')
ggsave(sprintf('../figs/%s/hospitalizations.png', out_dir), height = 4, width = 7, units = 'in', dpi = 300)

mean_delay <- read_rds('../data/fitted_delays/delay_infection_to_hosp_admit_posterior.rds') %>% bind_cols %>% select(1:2) %>%
  mutate(mean = exp(mu+sigma^2/2)) %>%
  pull(mean) %>%
  mean() %>% round()

## Estimate Rt by region ------------------------------
rt_by_region <- function(rr, dat){
  cat(sprintf('region is %s\n', rr))
  out = upscale_cori_pipeline(df = dat %>% filter(region == rr), 
                              obscolname = ts_colname,
                              p_obs = 1,
                              delay_mean = mean_delay,
                              gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                              nboot = 500, 
                              ttl = rr, 
                              obs_type = 'hospitalizations',
                              min_window = 7)
  sprintf('%s - done\n', rr)
  return(out)
}


## 1. by restore(4) region ------------------------------
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
EPIC_estimates <- lapply(unique(dat$region), rt_by_region, dat = dat) 
names(EPIC_estimates)  = unique(dat$region)





## Save results ------------------------------------
write_csv(lapply(EPIC_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/pipeline_hosp.csv', out_dir))
write_rds(EPIC_estimates, sprintf('../figs/%s/pipeline_hosp.rds', out_dir))

## Plot ------------

p1<-cowplot::plot_grid(
  EPIC_estimates$ALL_EPIC_HOSPITALS$upscale_plot + theme(legend.position = c(.8, .8)) + ggtitle('All EPIC hospitals'), 
  EPIC_estimates$ALL_EPIC_HOSPITALS$rt_plot + ylim(c(0, 3)), 
  ncol = 2) 
ggsave(file = sprintf('../figs/%s/rt_from_EPIC_hospitalizations.png', out_dir), p1, width = 7, height = 3, dpi = 300)

