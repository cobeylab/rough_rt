## Estimate with epinow2
region.in = commandArgs()[1]
outpath = '../epinow2_estimates'
dt <- Sys.Date()
dir_check(outpath)
dir_check(sprintf('%s/%s', outpath, dt))
source('../code/util.R')


## Load data --------------------------------------------
source('../code/load_timeseries.R')
dat_rr <- load_idph_public_cases_restore_region()
dat_cr<-load_idph_public_cases_covid_region()
## Combine the restore region data frame with the covid region data frame, leaving only one iteration of the IL_overall data
dat <- bind_rows(dat_rr %>% filter(region != 'IL_Overall'), 
                 dat_cr) %>%
  mutate(region = factor(region, levels = c(as.character(1:11), unique(dat_rr$region))))
dat %>%
  pivot_longer(c(new_cases, smoothed)) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  facet_wrap(.~region, scales = 'free_y')+
  scale_color_manual("", values = c('black', 'red', 'yellow'))+
  theme(legend.position = 'bottom')+
  ggtitle('idph cases - public linelist')
ggsave(sprintf('%s/%s/cases_input.png', outpath, Sys.Date()))

## Estimate rt using epinow2
library('EpiNow2')
source('../code/epinow2.R')
get_region <- function(rr){
  regions = unique(dat$region)
  dat %>% filter(region == regions[rr]) %>%
    group_by(date) %>%
    summarise(new_cases = sum(new_cases),
              smoothed = sum(smoothed))
}
sprintf('Running for region %s', region.in)
run_epinow2(dat_df = get_region(region.in),
            obs_colname = 'new_cases',
            dat_type = 'cases',
            prior_smoothing_window = 7,
            output_folder = sprintf('%s/%s', outpath, dt))
