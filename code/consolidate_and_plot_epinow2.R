library(ggplot2)
library(dplyr)
library(tidyr)
library(foreach)

regions = c(as.character(1:11), 'illinois')

foreach (r=1:length(regions), .combine='rbind') %do%{
    region = regions[r]
    geog = if_else(region == 'illinois', 'illinois', paste0('covidregion_',region))
    summary_file = sprintf('../epinow2_cli_estimates/%s/latest/summarised_estimates.rds', region)
    rt = readRDS(summary_file) %>%
        filter(variable == 'R') %>%
        mutate(geography_modelled = geog) %>%
        rename(rt_median = median, rt_lower=bottom, rt_upper=top) %>%
        select(date, geography_modelled, rt_median, rt_lower, rt_upper, type) 
    rt
} ->outdf

write.csv(outdf, 'epinow2_estimates.csv', row.names=F)