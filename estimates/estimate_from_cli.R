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

## Set reporting fraction, currently based on the mean IHR from Verity, et al., for 70-79 and 80+ year-olds
reporting_frac = 0.17

## Set the name of the output directory ---------------------------------
out_dir <- paste('cli', ts_colname, Sys.Date(), sep = '_')
if(!dir.exists(sprintf('../figs/%s/', out_dir))) dir.create(sprintf('../figs/%s/', out_dir))

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/upscale.R')
source('../code/rt_boot.R')

load_cli <- function(){
  ## Load the public linelist data by restore region ------------
  ## More up to date, public linelist
  region_cli = read.csv('../data/cli_admissions_2020-10-28.csv') %>%
    mutate(date = as.Date(date)) %>%
    rename(
           nadmit = cli) %>%
    mutate(
           region = as.character(covid_region)) %>%
    mutate(nadmit = ifelse(is.na(nadmit), 0, nadmit)) %>%
    select(-covid_region)

  statewide_cli = region_cli %>%
  group_by(date) %>%
  summarise(region = 'illinois',
            nadmit = sum(nadmit)) %>%
  ungroup()

  bind_rows(region_cli, statewide_cli) %>%
    group_by(region) %>%
    arrange(date) %>%
    mutate(smoothed = smooth.spline(nadmit, spar = .5)$y %>% min_0,
           avg_7d = zoo::rollmean(nadmit, k = 7, fill = c(mean(nadmit[1:7], na.rm = T), 
                                                             NA, 
                                                             mean(nadmit[length(nadmit)-(0:6)], na.rm = T)))
    )%>%
    ungroup() 
}





## Load data --------------------------------------------
dat <- load_cli()
print(head(dat))
## Visualize the case counts by restore region. (4 regions)
dat %>%
  pivot_longer(c(nadmit, smoothed, avg_7d)) %>%
  mutate(region = factor(region, levels = c('illinois', as.character(1:11)))) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  facet_wrap(.~region, scales = 'free_y') +
  scale_color_manual("", values = c('blue', 'gray', 'orange'))+
  theme(legend.position = 'bottom')+
  ggtitle('CLI admissions')
ggsave(sprintf('../figs/%s/hospitalizations.png', out_dir), height = 6, width = 7, units = 'in', dpi = 300)


mean_delay <- read_rds('../data/fitted_delays/delay_infection_to_hosp_admit_posterior.rds') %>% bind_cols %>% select(1:2) %>%
  mutate(mean = exp(mu+sigma^2/2)) %>%
  pull(mean) %>%
  mean() %>% round()

## Load Ganyani GI pars
GI_parlist <- read_rds('../data/GI_pars_Ganyani.rds')

## Estimate Rt by region ------------------------------
rt_by_region <- function(rr, dat){
  cat(sprintf('region is %s\n', rr))
  out = upscale_cori_pipeline(df = dat %>% filter(region == rr), 
                              obscolname = ts_colname,
                              p_obs = reporting_frac,
                              delay_mean = mean_delay,
                              gen_int_pars = c(mean = GI_parlist$mean, var = GI_parlist$sd^2), ## From Ganyani et al
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
cli_estimates <- lapply(unique(dat$region), rt_by_region, dat = dat) 
names(cli_estimates)  = unique(dat$region)





## Save results ------------------------------------
write_csv(lapply(cli_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/pipeline_hosp.csv', out_dir))
write_rds(cli_estimates, sprintf('../figs/%s/pipeline_hosp.rds', out_dir))

## Plot ------------

plot_summary <- function(estlist, fname){
  p1 <- cowplot::plot_grid(plotlist =
                             mapply(FUN = function(ll, nm) {
                               ll$upscale_plot+theme(legend.position = 'none')+ggtitle(nm)
                             }, ll = estlist, nm = names(estlist), SIMPLIFY = F),
                           ncol = 1)
  p2 <- cowplot::plot_grid(plotlist = lapply(estlist, function(ll) ll$rt_plot + ylim(c(0,5))),
                           ncol = 1)
  ggsave(sprintf('../figs/%s/%s', out_dir, fname), 
         cowplot::plot_grid(
           cowplot::plot_grid(p1, p2, ncol = 2),
           get_legend(estlist[[1]]$upscale_plot+theme(legend.position = 'bottom')),
           nrow = 2, rel_heights = c(15, 1)
         ),
         width = 6, height = 12, dpi = 300)
}


plot_summary(cli_estimates[1:6], 'covid_region_1-6_summary.png')
plot_summary(cli_estimates[7:12], 'covid_region_7-ILOverall_summary.png')

