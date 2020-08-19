## Estimate Rt using the latest version of case data 
## WORKFLOW

## 1. Pull the latest version of idph_public_cases.csv from Midway into your local ../data directory. Be careful NOT to push data to Github.
## 2. Source the script 'estimate_from_cases.R'. Rt estimates for each covid region, each restore region and IL_Overall will be saved to a .csv in the directory ../figs/TODAYS-DATE
## 3. Run the rest of the code in this script to generate figures. Figures will save to the same directory as in 2.

##  Estimates are saved to a directory labeled by the date on which they were generated in the ../figs/ directory
source('estimate_from_cases.R')


## Plot results for dates of interest
ANALYZE_DATE <- Sys.Date() ## Set the date 
## Load data by covid region
source('../code/load_timeseries.R')
## Load raw cases
raw_cases_cr <- load_idph_public_cases_covid_region()

## Load rt estimates by restore region
cases_rt_restore_region <- read_csv(sprintf('../figs/%s/cases_restore_region.csv', ANALYZE_DATE)) %>% 
  mutate(obs = 'cases',
         region = toupper(region),
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_cases,
         smoothed = smoothed,
         shifted = NA) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) 

## Load rt estimates by covid region
cases_rt_covid_region <- read_csv(sprintf('../figs/%s/cases_covid_region.csv', ANALYZE_DATE), col_types = cols(
  date = col_date(format = ""),
  region = col_character(),
  new_cases = col_double(),
  smoothed = col_double(),
  avg_7d = col_double(),
  obs = col_double(),
  time = col_double(),
  rt.mean = col_double(),
  rt.lower = col_double(),
  rt.upper = col_double(),
  robustness = col_character()
)) %>% 
  mutate(obs = 'cases',
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_cases,
         smoothed = smoothed,
         shifted = NA) %>% 
  mutate(region = factor(region, levels = c(as.character(1:11), 'IL_Overall'))) %>%
  select(date, region, obs, method, robustness, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) 

## Load rt estimates using shifted observations and the cori method for restore regions
cases_cori_restore_region <- read_csv(sprintf('../figs/%s/shifted_cases_restore_region.csv', ANALYZE_DATE)) %>% 
  mutate(obs = 'cases',
         method = 'shifted_cori',
         mean_delay = mean_delay,
         region = toupper(region),
         raw = new_cases,
         smoothed = smoothed,
         shifted = lead(smoothed, n = round(unique(mean_delay)))) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)

## Load rt estimates using shifted observations and the cori method for covid regions
cases_cori_covid_region <- read_csv(sprintf('../figs/%s/shifted_cases_covid_region.csv', ANALYZE_DATE), col_types = cols(
  date = col_date(format = ""),
  region = col_character(),
  new_cases = col_double(),
  smoothed = col_double(),
  roll_avg_7d = col_double(),
  rt.mean = col_double(),
  rt.lower = col_double(),
  rt.upper = col_double(),
  window = col_double(),
  mean_delay = col_double()
)) %>% 
  mutate(region = factor(region, levels = c(as.character(1:11), 'IL_Overall'))) %>%
  mutate(obs = 'cases',
         method = 'shifted_cori',
         mean_delay = mean_delay,
         raw = new_cases,
         smoothed = smoothed,
         shifted = lead(smoothed, n = round(unique(mean_delay)))) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)




## Plot shifted estimates (cori) vs estimates from full pipeline
bind_rows(cases_cori_restore_region, cases_rt_restore_region) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  scale_color_manual('', values = c('salmon', 'yellow'), aesthetics = c('color', 'fill')) +
  ylim(c(0,2.5))+
  ylab('Rt') -> case_compare
case_compare
ggsave(plot = case_compare, sprintf('../figs/%s/compare_cases_restore_region_rt.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)

# ## Load epinow2 estimates
# load_epinow2 <- function(region){
# read_rds(sprintf('~/R/rough-rt-approach/epinow2_estimates/%s/latest/summarised_estimates.rds', region)) %>%
#   mutate(region = region)
# }
# cr_epinow2 <- lapply(unique(cases_rt_covid_region$region), load_epinow2) %>% bind_rows %>% filter(variable == 'R')


## RESTORE REGION ------------------------
##  Plot cases vs. deconvolved cases
ymn = .25; ymx = 3.5;
raw_cases <- read_csv(sprintf('../figs/%s/cases_restore_region.csv', ANALYZE_DATE)) %>%
  mutate(region = toupper(region))
deconvolved_cases <- read_rds(sprintf('../figs/%s/restore_region_estimates_cases.rds', ANALYZE_DATE)) %>%
  lapply(function(ll) ll$deconvolved) %>% bind_rows(.id = 'region') %>%
  group_by(date, region) %>%
  summarize(deconvolved = mean(total_infections_est, na.rm = T),
            dc.lower = quantile(total_infections_est, .025),
            dc.upper = quantile(total_infections_est, .975)) %>%
  ungroup() %>%
  mutate(region = toupper(region)) 
deconvolved_cases %>%
  ggplot()+
  geom_point(data = raw_cases, aes(x = date, y = new_cases), color = 'red', pch = 21)+
  geom_line(aes(x = date, y = deconvolved))+
  geom_ribbon(aes(x = date, ymin = dc.lower, ymax = dc.upper), alpha = .2) +
  ylab('new cases')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  facet_wrap(.~region, scales = 'free_y', nrow = 1) -> case_dat
case_dat
ggsave(plot = case_dat, sprintf('../figs/%s/compare_case_data.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)

## Plot restore region summary
cowplot::plot_grid(case_dat, death_dat, case_death, case_compare, deaht_compare, nrow = 5, rel_heights = c(1, 1, 1.5, 1.5, 1.5))
ggsave(sprintf('../figs/%s/summary_restore_region_estimates.png', ANALYZE_DATE), width = 9, height = 2.5, units = 'in', dpi = 300)





## COVID REGIONS ------------------------
## Function to plot Rt estimates for the subset of regions given in the input vector
plot_cr_rt <- function(regions){
  cases_rt_covid_region %>%
    filter(region %in% regions) %>%
    mutate_at(.vars = vars(contains('rt')), .funs = function(xx) ifelse(xx<ymn, ymn, xx)) %>%
    mutate_at(vars(contains('rt')), .funs = function(xx) ifelse(xx>ymx, ymx, xx)) %>%
    ggplot()+
    geom_line(aes(x = date, y = rt.mean, color = robustness))+
    geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = robustness), alpha = .3)+
    geom_hline(aes(yintercept = 1))+
    geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
    facet_grid(region~.)+
    theme(legend.position = 'bottom')+
    scale_color_manual('', values = c('green3', 'yellow3', 'turquoise', 'orange'), aesthetics = c('color', 'fill')) +
    ylim(c(ymn, ymx))+
    xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
    ylab('Rt') 
}

## Plot and save Rt estimates for regions 1-6
plot_cr_rt(as.character(1:6))
ggsave(sprintf('../figs/%s/cases_rt_regions_1-6.png', ANALYZE_DATE), height = 12, width = 3, units = 'in', dpi = 300)
## Repeat for regions 7-11
plot_cr_rt(c(as.character(7:11), 'IL_Overall'))
ggsave(sprintf('../figs/%s/cases_rt_regions_7-Overall.png', ANALYZE_DATE), height = 12, width = 3, units = 'in', dpi = 300)



## Function to plot raw data by covid region by the subset of regions given on input
plot_cr_data <- function(regions){
raw_cases_cr %>%
    filter(region %in% regions) %>%
  pivot_longer(c(new_cases, avg_7d)) %>%
  filter(value > 0) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  ylab('cases')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  facet_grid(region~., scales = 'free_y') +
  theme(legend.position = 'bottom', legend.title = element_blank())
  
}
plot_cr_data(as.character(1:6))
ggsave(sprintf('../figs/%s/covid_region_1-6_case_data.png', ANALYZE_DATE), width = 2.5, height = 9, units = 'in', dpi = 300)
plot_cr_rt(c(as.character(7:11), 'IL_Overall'))
ggsave(sprintf('../figs/%s/covid_region_7-Overall_case_data.png', ANALYZE_DATE), width = 2.5, height = 9, units = 'in', dpi = 300)




## Arrange plots of data and rt estimates by covid region, and save.
## Plot covid region summary
cowplot::plot_grid(plot_cr_rt(as.character(1:6)), 
                   plot_cr_data(as.character(1:6)), ncol = 2, align = 'h')
ggsave(sprintf('../figs/%s/summary_covid_region_estimates_1-6.png', ANALYZE_DATE), width = 8, height = 12, units = 'in', dpi = 300)

cowplot::plot_grid(plot_cr_rt(c(as.character(7:11), 'IL_Overall')),
                   plot_cr_data(c(as.character(7:11), 'IL_Overall')), ncol = 2)
ggsave(sprintf('../figs/%s/summary_covid_region_estimates_7-Overall.png', ANALYZE_DATE), width = 8, height = 12, units = 'in', dpi = 300)

