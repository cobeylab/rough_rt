## Aggregate epinow2 estimates for each covid region, copy into one data frame and then save to the ../figs/... directory
library(readr)
## Source function used to summarise outputs
source('../code/util.R')
source('../code/summarise-epinow2-outputs.R')

## Read in job information saved from script used to generate estimates
runpars <- read_rds(sprintf('run_params.rds'))

## Summarise the estimates
summarise_all_estimates(path = runpars$outpath, dt = runpars$dt, tooday = runpars$today)

## Delete the temporary .rds file
file.remove(sprintf('run_params.rds'))

## Fin. 