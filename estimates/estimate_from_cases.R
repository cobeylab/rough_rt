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
ts_colname = 'new_cases'
#ts_colname = 'smoothed'
cat(sprintf('Estimating from %s raw data', ts_colname))

## Set the name of the output directory ---------------------------------
out_dir <- paste('cases', ts_colname, Sys.Date(), sep = '_')
#out_dir <- paste('cases', ts_colname, '2020-08-26_retrospective', sep = '_')
if(!dir.exists(sprintf('../figs/%s/', out_dir))) dir.create(sprintf('../figs/%s/', out_dir))

source('../code/util.R')
source('../code/cori.R')
source('../code/rt_pipeline.R')
source('../code/upscale.R')
source('../code/rt_boot.R')







## Load data --------------------------------------------
source('../code/load_timeseries.R')
dat_11r<-load_idph_public_cases_covid_region()
dat_11r %>%
  pivot_longer(c(new_cases, smoothed, avg_7d)) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name), alpha = .6)+
  facet_wrap(.~region, scales = 'free_y')+
  scale_color_manual("", values = c('blue', 'gray', 'orange'))+
  theme(legend.position = 'bottom')+
  ggtitle('idph cases - public linelist - UIUC removed')
ggsave(sprintf('../figs/%s/cases_covid_region.png', out_dir), height = 6, width = 7, units = 'in', dpi = 300)


## Calculate the mean delay
mean_delay <- round(12.11358)


## Load GI pars from Ganyani table 3
GI_parlist <- read_rds('../data/GI_pars_Ganyani.rds')


## Estimate Rt by region ------------------------------
rt_by_region <- function(rr, dat){
  cat(sprintf('restore region is %s\n', rr))
  out = upscale_cori_pipeline(df = dat %>% filter(region == rr), 
                         obscolname = ts_colname,
                         p_obs = .25,
                         delay_mean = mean_delay,
                         gen_int_pars = c(mean = GI_parlist$mean, var = GI_parlist$sd^2), ## From Ganyani et al
                         nboot = 500, 
                         ttl = rr, 
                         obs_type = 'cases',
                         min_window = 7)
  sprintf('%s - done\n', rr)
  return(out)
}


## 2. by covid (11) region ------------------------------
#Takes a few minutes to run, depending on size of nboot
#Can't be parallelized because internal operations are already running in parallel
covid_region_estimates <- lapply(unique(dat_11r$region), rt_by_region, dat = dat_11r) 
names(covid_region_estimates)  = unique(dat_11r$region)

pdf(file = sprintf('../figs/%s/covid_region_rt_from_idph_cases.pdf', out_dir))
lapply(covid_region_estimates, function(ll) cowplot::plot_grid(ll$upscale_plot + theme(legend.position = c(.8, .8)), ll$rt_plot, ncol = 1)) 
dev.off()



plot_summary <- function(estlist, fname){
  p1 <- cowplot::plot_grid(plotlist =
                             mapply(FUN = function(ll, nm) {
                               ll$upscale_plot+theme(legend.position = 'none')+ggtitle(nm)
                             }, ll = estlist, nm = names(estlist), SIMPLIFY = F),
                           ncol = 1)
  p2 <- cowplot::plot_grid(plotlist = lapply(estlist, function(ll) ll$rt_plot + ylim(c(0,3))),
                           ncol = 1)
  ggsave(sprintf('../figs/%s/%s', out_dir, fname), 
         cowplot::plot_grid(
           cowplot::plot_grid(p1, p2, ncol = 2),
           get_legend(estlist[[1]]$upscale_plot+theme(legend.position = 'bottom')),
           nrow = 2, rel_heights = c(15, 1)
         ),
         width = 6, height = 12, dpi = 300)
}




## Save results ------------------------------------
write_csv(lapply(covid_region_estimates, function(ll) ll$df) %>% bind_rows(.id = 'region'), sprintf('../figs/%s/shift_pipeline_cases_covid_region.csv', out_dir))
write_rds(covid_region_estimates, sprintf('../figs/%s/shift_pipeline_cases_covid_region.rds', out_dir))

## Generate plots
plot_summary(covid_region_estimates[1:6], 'covid_region_1-6_summary.png')
plot_summary(covid_region_estimates[7:12], 'covid_region_7-ILOverall_summary.png')
