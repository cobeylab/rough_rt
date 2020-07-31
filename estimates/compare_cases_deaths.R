source('estimate_from_cases.R')
source('estimate_from_deaths.R')


## Plot comparison for date of intertest
ANALYZE_DATE <- Sys.Date()
cases_rt <- read_csv(sprintf('csv/cases_regional_%s.csv', ANALYZE_DATE)) %>% 
  mutate(obs = 'cases',
         region = toupper(region),
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_cases,
         smoothed = smoothed,
         shifted = NA) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper) %>%
  filter(date < max(date)-7)
cases_cori <- read_csv(sprintf('csv/shifted_cases_regional_%s.csv', Sys.Date())) %>% 
  mutate(obs = 'cases',
         method = 'shifted_cori',
         mean_delay = mean_delay,
         region = toupper(restore_region),
         raw = new_cases,
         smoothed = smoothed,
         shifted = lead(smoothed, n = round(unique(mean_delay)))) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)
deaths_rt <- read_csv(sprintf('csv/deaths_regional_%s.csv', ANALYZE_DATE)) %>% 
  mutate(obs = 'deaths',
         region = toupper(region),
         method = 'full_pipeline',
         mean_delay = NA,
         raw = new_deaths,
         smoothed = NA,
         shifted = NA) %>% 
  select(date, region, obs, method, mean_delay, raw, smoothed, shifted, rt.mean, rt.lower, rt.upper)
deaths_cori <- read_csv(sprintf('csv/shifted_deaths_regional_%s.csv', Sys.Date())) %>% 
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
#hosp_rt <- read_csv(sprintf('csv/hospitalizations_regional_%s.csv', ANALYZE_DATE)) %>% mutate(kind = 'hospitalizations')


bind_rows(cases_rt, deaths_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .2)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(as_date('2020-04-01'), Sys.Date()))+
  ylab('Rt')+
  ylim(c(0,2.5))+
  scale_color_manual('', values = c('salmon', 'dodgerblue'), aesthetics = c('color', 'fill')) -> case_death
case_death
ggsave(plot = case_death, sprintf('../figs/%s/compare_cases_deaths_rt.png', ANALYZE_DATE))


bind_rows(deaths_cori, deaths_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(as_date('2020-04-01'), Sys.Date()))+
  scale_color_manual('', values = c('dodgerblue', 'green'), aesthetics = c('color', 'fill')) +
  ylim(c(0,2.5))+
  ylab('Rt') -> deaht_compare
deaht_compare
ggsave(plot = deaht_compare, sprintf('../figs/%s/compare_deaths_rt.png', ANALYZE_DATE))


bind_rows(cases_cori, cases_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = interaction(obs, method)))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = interaction(obs, method)), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_grid(.~region)+
  theme(legend.position = 'bottom')+
  xlim(c(as_date('2020-04-01'), Sys.Date()))+
  scale_color_manual('', values = c('salmon', 'yellow'), aesthetics = c('color', 'fill')) +
  ylim(c(0,2.5))+
  ylab('Rt') -> case_compare
case_compare
ggsave(plot = case_compare, sprintf('../figs/%s/compare_cases_rt.png', ANALYZE_DATE))


raw_deaths <- read_csv('csv/dat_deaths_2020-07-29.rds') %>%
  mutate(region = toupper(restore_region))
deconvolved_deaths <- read_rds(sprintf('csv/regional_estimates_deaths_%s.rds', ANALYZE_DATE)) %>%
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
  geom_point(data = raw_deaths, aes(x = date, y = new_deaths), color = 'blue', pch = 21)+
  geom_line(aes(x = date, y = deconvolved))+
  geom_ribbon(aes(x = date, ymin = dc.lower, ymax = dc.upper), alpha = .2) +
  ylab('new deaths')+
  xlim(c(as_date('2020-04-01'), Sys.Date()))+
  facet_wrap(.~region, scales = 'free_y', nrow = 1) -> death_dat
ggsave(plot = death_dat, sprintf('../figs/%s/compare_death_data.png', ANALYZE_DATE))



raw_cases <- read_csv('csv/dat_cases_2020-07-29.csv') %>%
  mutate(region = toupper(restore_region))
deconvolved_cases <- read_rds(sprintf('csv/regional_estimates_cases_%s.rds', ANALYZE_DATE)) %>%
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
  xlim(c(as_date('2020-04-01'), Sys.Date()))+
  facet_wrap(.~region, scales = 'free_y', nrow = 1) -> case_dat
case_dat
ggsave(plot = case_dat, sprintf('../figs/%s/compare_case_data.png', ANALYZE_DATE))



cowplot::plot_grid(case_dat, death_dat, case_death, case_compare, deaht_compare, nrow = 5, rel_heights = c(1, 1, 1.5, 1.5, 1.5))
ggsave(sprintf('../figs/%s/summary_estimates.png', ANALYZE_DATE), width = 9, height = 12, units = 'in', dpi = 300)



## Plot comparison of inferred case time series
overall_rt <- bind_rows(list(cases = read_csv('csv/cases_overall_2020-07-29.csv'), 
                             deaths = read_csv('csv/deaths_overall_2020-07-29.csv')),
                        .id = 'data_type')
overall_plot <- overall_rt %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = data_type))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = data_type), alpha = .5)

