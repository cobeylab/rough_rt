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



cli_estimates = readRDS('../figs/cli_nadmit_2020-10-08_changing_smoothing_window/pipeline_hosp.rds')
out_dir = '../figs/cli_nadmit_2020-10-08_changing_smoothing_window/'

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

