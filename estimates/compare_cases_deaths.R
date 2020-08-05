#source('estimate_from_cases.R')
# source('estimate_from_deaths.R')


## Plot comparison for date of intertest
ANALYZE_DATE <- lubridate::as_date('2020_07_31')
## Load data by covid region
source('../code/load_timeseries.R')
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
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) %>%
  filter(date < max(date)-7)
## Load rt estimates by covid region
cases_rt_covid_region <- read_csv(sprintf('../figs/%s/cases_covid_region.csv', ANALYZE_DATE), col_types = cols(
  date = col_date(format = ""),
  region = col_character(),
  new_cases = col_double(),
  smoothed = col_double(),
  roll_avg_7d = col_double(),
  obs = col_double(),
  time.x = col_double(),
  time.y = col_double(),
  rt.mean = col_double(),
  rt.lower = col_double(),
  rt.upper = col_double()
)) %>% 
  mutate(obs = 'cases',
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_cases,
         smoothed = smoothed,
         shifted = NA) %>% 
  mutate(region = factor(region, levels = c(as.character(1:11), 'IL_Overall'))) %>%
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) 
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
## Load rt estimates for restore regions using deaths (covid regions not run)
deaths_rt <- read_csv(sprintf('../figs/%s/deaths_covid_region.csv', ANALYZE_DATE)) %>% 
  mutate(obs = 'deaths',
         region = toupper(region),
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_deaths,
         smoothed = NA,
         shifted = NA) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)
## Load rt estimates using shifted deaths and the cori method for restore regions
deaths_cori <- read_csv(sprintf('../figs/%s/shifted_deaths_covid_region.csv', ANALYZE_DATE)) %>% 
  group_by(restore_region) %>%
  arrange(date) %>%
  mutate(obs = 'deaths',
         method = 'shifted_cori',
         mean_delay = mean_delay,
         region = toupper(restore_region),
         raw = new_deaths,
         smoothed = NA,
         shifted = lead(new_deaths, n = round(unique(mean_delay)))) %>% 
  ungroup %>%
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) 


## Plot estimates from cases against estimates from deaths
bind_rows(cases_rt_restore_region, deaths_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .2)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  ylab('Rt')+
  ylim(c(0,2.5))+
  scale_color_manual('', values = c('salmon', 'dodgerblue'), aesthetics = c('color', 'fill')) -> case_death
case_death
ggsave(plot = case_death, sprintf('../figs/%s/compare_cases_deaths_rt.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)


bind_rows(deaths_cori, deaths_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  scale_color_manual('', values = c('dodgerblue', 'green'), aesthetics = c('color', 'fill')) +
  ylim(c(0,2.5))+
  ylab('Rt') -> deaht_compare
deaht_compare
ggsave(plot = deaht_compare, sprintf('../figs/%s/compare_deaths_rt.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)


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

## Load epinow2 estimates
ymn = .25; ymx = 3.5;
load_epinow2 <- function(region){
read_rds(sprintf('~/R/rough-rt-approach/epinow2_estimates/%s/latest/summarised_estimates.rds', region)) %>%
  mutate(region = region)
}
cr_epinow2 <- lapply(unique(cases_rt_covid_region$region), load_epinow2) %>% bind_rows %>% filter(variable == 'R')


bind_rows( cases_rt_covid_region %>% mutate(method = 'KG_pipeline', type = ifelse(date <= max(date)-10, 'estimate', 'estimate based on partial data')) %>% select(date, region, type, method, rt.mean, rt.lower, rt.upper),
           cr_epinow2 %>% rename(rt.mean = mean, rt.lower = bottom, rt.upper = top) %>% mutate(method = 'epinow2') %>% select(date, region, type, method, rt.mean, rt.lower, rt.upper)
) %>%
  mutate(type = ifelse(type == 'estimate', '', 'partial_data')) %>%
  mutate_at(.vars = vars(contains('rt')), .funs = function(xx) ifelse(xx<ymn, ymn, xx)) %>%
  mutate_at(vars(contains('rt')), .funs = function(xx) ifelse(xx>ymx, ymx, xx)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(method, type)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(method, type)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(region~.)+
  theme(legend.position = 'bottom')+
  scale_color_manual('', values = c('green3', 'yellow3', 'turquoise', 'orange'), aesthetics = c('color', 'fill')) +
  ylim(c(ymn, ymx))+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  ylab('Rt') -> case_cr
case_cr
ggsave(plot = case_cr, sprintf('../figs/%s/cases_covid_region_rt.png', ANALYZE_DATE), height = 12, width = 3, units = 'in', dpi = 300)



# raw_deaths <- read_csv(sprintf('../figs/%s/')) %>%
#   mutate(region = toupper(restore_region))
deconvolved_deaths <- read_rds(sprintf('../figs/%s/restore_region_estimates_deaths.rds', ANALYZE_DATE)) %>%
  lapply(function(ll) ll$deconvolved) %>% bind_rows(.id = 'region') %>%
  group_by(date, region) %>%
  summarize(deconvolved = mean(total_infections_est, na.rm = T),
            dc.lower = quantile(total_infections_est, .025),
            dc.upper = quantile(total_infections_est, .975)) %>%
  ungroup() %>%
  mutate(region = toupper(region)) %>%
  filter(deconvolved < 200)
deconvolved_deaths %>%
  ggplot()+
  geom_point(data = deaths_rt, aes(x = date, y = raw), color = 'blue', pch = 21)+
  geom_line(aes(x = date, y = deconvolved))+
  geom_ribbon(aes(x = date, ymin = dc.lower, ymax = dc.upper), alpha = .2) +
  ylab('new deaths')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  facet_wrap(.~region, scales = 'free_y', nrow = 1) -> death_dat
ggsave(plot = death_dat, sprintf('../figs/%s/compare_death_data.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)



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



raw_cases_cr %>%
  pivot_longer(c(new_cases, smoothed)) %>%
  filter(value > 0) %>%
  ggplot()+
  geom_line(aes(x = date, y = value, color = name), show.legend = F)+
  ylab('cases')+
  xlim(c(lubridate::as_date('2020-04-01'), Sys.Date()))+
  facet_grid(region~., scales = 'free_y') -> cr_case_dat
ggsave(plot = cr_case_dat, sprintf('../figs/%s/covid_region_case_data.png', ANALYZE_DATE), height = 2.5, width = 9, units = 'in', dpi = 300)



## Plot restore region summary
cowplot::plot_grid(case_dat, death_dat, case_death, case_compare, deaht_compare, nrow = 5, rel_heights = c(1, 1, 1.5, 1.5, 1.5))
ggsave(sprintf('../figs/%s/summary_restore_region_estimates.png', ANALYZE_DATE), width = 9, height = 12, units = 'in', dpi = 300)

## Plot covid region summary
cowplot::plot_grid(cr_case_dat, case_cr, ncol = 2)
ggsave(sprintf('../figs/%s/summary_covid_region_estimates.png', ANALYZE_DATE), width = 8, height = 12, units = 'in', dpi = 300)

