## Aggregate epinow2 estimates for each covid region, copy into one data frame and then save to the ../figs/... directory
library(readr)
library(rlang)
library(dplyr)
library(optparse)

## Read in options from midway
option_list = list(make_option("--debug", type = "logical", default = FALSE, help = 'optional outpath spec for testing. Default is epinow2_cli_estimates/')); 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser); # Now you have a list called "opt" with elements opt$var and opt$out

## Source function used to summarise outputs
source('../code/summarise-epinow2-estimates.R')

## Read in job information saved from script used to generate estimates
runpars <- read_rds(sprintf('run_params.rds'))

## Summarise the estimates
summarise_all_estimates(path = runpars$outpath, dt = runpars$dt, tooday = runpars$today, debug = opt$debug)

## Delete the temporary .rds file
file.remove(sprintf('run_params.rds'))

## Fin. 