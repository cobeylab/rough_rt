source('estimate_from_cases.R')
source('estimate_from_deaths.R')
source('estimate_from_hospitalizations.R')

## Plot comparison for date of intertest
ANALYZE_DATE <- Sys.Date()
cases_rt <- read_csv(sprintf('csv/cases_regional_%s.csv', ANALYZE_DATE)) %>% mutate(kind = 'cases')
deaths_rt <- read_csv(sprintf('csv/deaths_regional_%s.csv', ANALYZE_DATE)) %>% mutate(kind = 'deaths')
#hosp_rt <- read_csv(sprintf('csv/hospitalizations_regional_%s.csv', ANALYZE_DATE)) %>% mutate(kind = 'hospitalizations')

bind_rows(cases_rt, deaths_rt) %>%
  mutate(region = toupper(region)) %>%
  ggplot()+
  geom_line(aes(x = date, y = rt.mean, color = kind))+
  geom_ribbon(aes(x = date, ymin = rt.lower, ymax = rt.upper, fill = kind), alpha = .3)+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = Sys.Date()), lty = 2)+
  facet_wrap(.~region)+
  ylim(c(0,3.5))
ggsave(sprintf('../figs/compare_rt_%s.png', ANALYZE_DATE))


## Plot comparison of inferred case time series
cases_list <- read_rds(sprintf('csv/regional_estimates_cases_%s.rds', ANALYZE_DATE))
deaths_list <- read_rds(sprintf('csv/regional_estimates_deaths_%s.rds', ANALYZE_DATE))


infections <- bind_rows(
lapply(cases_list, function(ll) ll$deconvolved) %>% bind_rows(.id = 'region') %>% mutate(kind = 'cases', total_infections_est = total_infections_est/15),
lapply(deaths_list, function(ll) ll$deconvolved) %>% bind_rows(.id = 'region') %>% mutate(kind = 'deaths')
) %>%
  mutate(region = toupper(region))

infections %>%
  ggplot() +
  geom_line(aes(x = date, y = total_infections_est, color = kind, group = interaction(kind, rep)), alpha = .1) +
  facet_wrap(.~ region, scales = 'free_y')
ggsave(sprintf('../figs/compare_infections_%s.png', ANALYZE_DATE))
