## Estimate with epinow2
outpath = '../epinow2_cli_estimates'
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(EpiEstim)
theme_set(theme_bw())
source('../code/util.R')
source('../code/load_timeseries.R')
dt <- max(load_cli()$date) ## Use the last date in the timeseries to set the output folder name.
dir_check(outpath)
dir_check(sprintf('%s/%s', outpath, dt))



## Load data --------------------------------------------
dat <- load_cli()
## Plot data -------
dat %>%
  pivot_longer(c(nadmit, smoothed)) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  facet_wrap(.~region, scales = 'free_y')+
  scale_color_manual("", values = c('black', 'red', 'yellow'))+
  theme(legend.position = 'bottom')+
  ggtitle('idph cases - public linelist')
ggsave(sprintf('%s/%s/cli_input.png', outpath, dt))

## Estimate rt using epinow2
library('EpiNow2')
source('../code/epinow2.R')
get_region <- function(rr){
  dat %>% filter(region == rr) %>%
    group_by(date) %>%
    summarise(nadmit = sum(nadmit),
              smoothed = sum(smoothed))
}


regions = unique(dat$region)
if(slurmR::Slurm_env(x="SLURM_ARRAY_TASK_ID")== 1){ ## If running on midway
  regions = regions[slurmR::Slurm_env(x="SLURM_ARRAY_TASK_ID")-1] ## Only run for the current slurm array task id
} ## Else, run for all regions.

for(region.in in regions){
  cat(sprintf('Running for region %s', region.in))
  dir_check(sprintf('%s/%s/%s', outpath, dt, region.in))
  run_epinow2(dat_df = get_region(region.in),
              obs_colname = 'nadmit',
              dat_type = 'hospitalizations',
              prior_smoothing_window = 1,
              output_folder = sprintf('%s/%s', outpath, region.in))
}
