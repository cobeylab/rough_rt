source('../code/load_timeseries.R')

hh <- load_EPIC_admissions()

cc <- load_idph_public_cases_restore_region()

merge(
  load_EPIC_admissions() %>%
    rename('hosp admissions' = nadmit) %>%
    select(date, `hosp admissions`),
  load_idph_public_cases_restore_region() %>%
    rename('new cases' = new_cases) %>%
    select(date, region, `new cases`),
  by = 'date', all = T
) -> df

df %>%
  filter(!is.na(region))%>%
  mutate(`new cases` = `new cases`/mean(`new cases`, na.rm = T)*mean(`hosp admissions`, na.rm = T)) %>%
  ggplot()+
  geom_line(aes(x = date, y = `hosp admissions`), color = 'salmon')+
  geom_line(aes(x = date, y = `new cases`), color = 'dodgerblue')+
  scale_y_continuous('EPIC hosp admissions',
      sec.axis = sec_axis( trans=~./mean(df$`hosp admissions`, na.rm = T)*mean(df$`new cases`, na.rm = T), name="idph public new cases")
)+
  facet_wrap(.~region) +
  ggtitle('Blue - incident cases by region \nRed - all EPIC admissions')
ggsave('../figs/compare_case_hospital_data.png', width = 7, height = 4, dpi = 300)
