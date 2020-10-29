## Estimate Rt using the latest version of case data 
## WORKFLOW

## 1. Pull the latest version of idph_public_cases.csv from Midway into your local ../data directory. Be careful NOT to push data to Github.
## 2. Source the script 'estimate_from_cases.R'. Rt estimates for each covid region, each restore region and IL_Overall will be saved to a .csv in the directory ../figs/TODAYS-DATE
## 3. Run the rest of the code in this script to generate figures. Figures will save to the same directory as in 2.

##  Estimates are saved to a directory labeled by the date on which they were generated in the ../figs/ directory
source('estimate_from_hospitalizations.R')


## Plot results for dates of interest
path <- out_dir ## Set the date 
## Load data by covid region
source('../code/load_timeseries.R')
## Load raw cases
raw_cases_cr <- load_EPIC_admissions()


## Load rt estimates by covid region
hosp_rt <- read_csv(sprintf('../figs/%s/pipeline_hosp.csv', path)) %>% 
  mutate(obs = 'hospitalizations',
         method = 'full_pipeline',
         mean_delay = NA,
         raw = nadmit,
         smoothed = smoothed,
         shifted = NA) %>% 
  select(date, region, obs, method, robustness, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) 


## Load rt estimates using shifted observations and the cori method for covid regions
cori_hosp_rt <- read_csv(sprintf('../figs/%s/shifted_cori_hosp.csv', path)) %>% 
  mutate(obs = 'hospitalizations',
         method = 'shifted_cori',
         mean_delay = mean_delay,
         raw = nadmit,
         smoothed = smoothed,
         shifted = lead(smoothed, n = round(unique(mean_delay)))) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)




## Plot shifted estimates (cori) vs estimates from full pipeline
bind_rows(hosp_rt, cori_hosp_rt) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  scale_color_manual('', values = c('salmon', 'blue3'), aesthetics = c('color', 'fill')) +
  ylim(c(0,2.5))+
  ylab('Rt') -> compare
compare
ggsave(plot = compare, sprintf('../figs/%s/compare_hosp_rt.png', path), height = 2.5, width = 3, units = 'in', dpi = 300)


## RESTORE REGION ------------------------
##  Plot cases vs. deconvolved cases
ymn = .25; ymx = 3.5;
obs <- load_EPIC_admissions()
deconvolved <- read_rds(sprintf('../figs/%s/pipeline_hosp.rds', path)) %>%
  lapply(function(ll) ll$deconvolved) %>% bind_rows(.id = 'region') %>%
  group_by(date, region) %>%
  summarize(deconvolved = mean(total_infections_est, na.rm = T),
            dc.lower = quantile(total_infections_est, .025),
            dc.upper = quantile(total_infections_est, .975)) %>%
  ungroup() 
deconvolved %>%
  ggplot()+
  geom_point(data = obs, aes(x = date, y = nadmit), color = 'red', pch = 21)+
  geom_line(aes(x = date, y = deconvolved))+
  geom_ribbon(aes(x = date, ymin = dc.lower, ymax = dc.upper), alpha = .2) +
  ylab('COVID-19 admissions')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  facet_wrap(.~region, scales = 'free_y', nrow = 1) -> hosp_dat
hosp_dat
ggsave(plot = hosp_dat, sprintf('../figs/%s/hosp_obs_deconvolved.png', path), height = 2.5, width = 3, units = 'in', dpi = 300)


## Plot Rt ests
plot_rt <- function(regions = 'ALL_EPIC_HOSPITALS'){
  hosp_rt %>%
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

## Function to plot raw data by covid region by the subset of regions given on input
plot_data <- function(regions = 'ALL_EPIC_HOSPITALS'){
obs %>%
  filter(region %in% regions) %>%
  pivot_longer(c(nadmit, !!sym(ts_colname))) %>%
  filter(value > 0) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name))+
  ylab('hospital admissions')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  #facet_grid(region~., scales = 'free_y') +
  theme(legend.position = 'bottom', legend.title = element_blank())
  
}
plot_data()
ggsave(sprintf('../figs/%s/hosp_data.png', path), width = 2.5, height = 2.5, units = 'in', dpi = 300)




## Arrange plots of data and rt estimates by covid region, and save.
## Plot covid region summary
cowplot::plot_grid(plot_rt(), 
                   plot_data(), ncol = 2, align = 'h')
ggsave(sprintf('../figs/%s/summary_hosp_rt.png', path), width = 8, height = 3, units = 'in', dpi = 300)
