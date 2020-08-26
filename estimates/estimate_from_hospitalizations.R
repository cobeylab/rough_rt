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
ts_colname = 'avg_7d'
#ts_colname = 'smoothed'
cat(sprintf('Estimating from %s raw data', ts_colname))

## Set the name of the output directory ---------------------------------
out_dir <- paste('hospitalizations', ts_colname, Sys.Date(), sep = '_')
if(!dir.exists(sprintf('../figs/%s/', out_dir))) dir.create(sprintf('../figs/%s/', out_dir))

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/deconvolve.R')
source('../code/Richardson_Lucy.R')
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


## Estimate Rt by region ------------------------------
rt_by_region <- function(rr, dat){
  cat(sprintf('region is %s\n', rr))
  out = full_rt_pipeline(df = dat %>% filter(region == rr), 
                         obscolname = ts_colname,
                         p_obs = .9,
                         delay_pars = read_rds('../data/fitted_delays/delay_infection_to_hosp_admit_posterior.rds') %>% bind_cols %>% select(1:2),
                         rep_delay_pars = read_rds('../data/fitted_delays/delay_infection_to_hosp_report_posterior.rds') %>% bind_cols %>% select(1:2),
                         delay_type = 'lognormal',
                         gen_int_pars = c(mean = 4.5, var = 1.7), ## From Ganyani et al
                         nboot = 1000, 
                         ttl = rr, 
                         obs_type = 'hospitalizations',
                         min_window = 1)
  sprintf('%s - done\n', rr)
  return(out)
}


## 1. by restore(4) region ------------------------------
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
EPIC_estimates <- lapply(unique(dat$region), rt_by_region, dat = dat) 
names(EPIC_estimates)  = unique(dat$region)

pdf(file = sprintf('../figs/%s/rt_from_EPIC_hospitalizations.pdf', out_dir))
lapply(EPIC_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()


## Estimate using the raw, shifted time series ----------------------------
cori_by_region <- function(rr, dat){

    ins <- filter(dat, region == rr) %>% 
      ungroup() %>%
      arrange(date) %>%
      mutate(time = 1:nrow(.)) # Create a numeric time column
    
   
  ## Calculate the appropriate window size
  ##   This is kind of arbitrary, but it scales with the 20th percentile of daily sample size
  ww = max(1, floor(50/quantile(filter(ins, smoothed>0)$smoothed, .2)))
  cat(sprintf('\nregion is %s, window is %.0f\n', rr, ww))
  
  ## Calculate the mean delay
  md = read_rds('../data/fitted_delays/delay_infection_to_hosp_admit_posterior.rds') %>% bind_cols %>% select(1:2) %>% 
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

EPIC_cori<- lapply(unique(dat$region), FUN = cori_by_region, dat = dat) %>%
  bind_rows %>%
  rename(rt.lower = rt.025,
         rt.upper = rt.975)





## Save results ------------------------------------
write_csv(lapply(EPIC_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/pipeline_hosp.csv', out_dir))
write_csv(EPIC_cori, sprintf('../figs/%s/shifted_cori_hosp.csv', out_dir))
write_rds(EPIC_estimates, sprintf('../figs/%s/pipeline_hosp.rds', out_dir))
write_rds(EPIC_cori, sprintf('../figs/%s/shifted_cori_hosp.rds', out_dir))

