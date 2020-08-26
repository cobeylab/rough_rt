min_0 <- function(xx){ifelse(xx<0, 0, xx)}

load_idph_public_cases_restore_region <- function(){
## Load the public linelist data by restore region ------------
## More up to date, public linelist
raw_dat <- read_csv('../data/idph_public_restore_region.csv') %>%
  rename(region = restore_region) %>%
  filter(region != 'unknown') %>%
  group_by(date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
         region = toupper(region),
         region = ifelse(region=='NORTH-CENTRAL', 'NORTHCENTRAL', region)) %>%
  ungroup() 
## Get all IL data
overall_dat <- raw_dat %>%
  filter(region != 'CHICAGO') %>%
  group_by(date) %>%
  summarise(region = 'IL_Overall',
            new_cases = sum(new_cases))
## Add IL overall data as its own region
bind_rows(raw_dat, overall_dat) %>%
  ungroup() %>% group_by(region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .6)$y %>% min_0,
         avg_7d = zoo::rollmean(new_cases, k = 7, fill = c(new_cases[1], NA, new_cases[length(new_cases)]))) %>%
  filter(date <= max(date))%>%
  filter(date >= lubridate::as_date('2020-04-01')) %>%
  mutate(new_cases = ifelse(new_cases<0, 0, new_cases))
}





load_idph_public_cases_covid_region <- function(){
## Load the public linelist data by covid region ------------
##  (11 regions)
raw_dat_cr <- read_csv('../data/idph_public_covid_region.csv')  %>%
  rename(region = new_restore_region) %>%
  filter(region != 'unknown') %>%
  group_by(date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
         region = toupper(region)) 
overall_dat_cr <- raw_dat_cr %>%
  group_by(date) %>%
  summarise(region = 'IL_Overall',
            new_cases = sum(new_cases))
bind_rows(raw_dat_cr, overall_dat_cr)%>%
  ungroup() %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(smoothed = smooth.spline(new_cases, spar = .6)$y %>% min_0,
         avg_7d = zoo::rollmean(new_cases, k = 7, fill = c(new_cases[1], NA, new_cases[length(new_cases)]))) %>%
  filter(date <= max(date)) %>%
  mutate(region = factor(region, levels = c(as.character(1:11), 'IL_Overall'))) %>%
  filter(date >= lubridate::as_date('2020-04-01'))%>%
  mutate(new_cases = ifelse(new_cases<0, 0, new_cases))
}

load_EPIC_admissions <- function(){
  ## Load the public linelist data by restore region ------------
  ## More up to date, public linelist
  read_csv('../data/EPIC_AdmissionsCountsByAgeDate.csv') %>%
    rename(age = AgeCategory, 
           date = AdmitDate,
           nadmit = NumberOfAdmissions) %>%
    group_by(date) %>%
    summarise(nadmit = sum(nadmit)) %>%
    ungroup %>%
    tidyr::extract(date, into = c('month', 'day', 'year'), '(\\d\\d?)/(\\d\\d?)/(\\d\\d)') %>%
    mutate(nadmit = ifelse(is.na(nadmit), 0, nadmit),
           region = 'ALL_EPIC_HOSPITALS',
           date = as.Date(sprintf('%2i20-%2i-%2i', as.numeric(year), as.numeric(month), as.numeric(day)))) %>%
    mutate(smoothed = smooth.spline(nadmit, spar = .5)$y %>% min_0,
           avg_7d = zoo::rollmean(nadmit, k = 7, fill = list(left = nadmit[1], center = NA, right = nadmit[length(nadmit)]), align = 'center'))  %>%
    select(-month,-day,-year) %>%
    ungroup() 
}
