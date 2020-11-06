#!/bin/bash
#SBATCH --time=20:00:00
#SBATCH --array=1-12
#SBATCH --ntasks-per-cpu=4
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=kgostic
#SBATCH --partition=cobey
#SBATCH --job-name="Rt_cases_v1.2.1"

## #SBATCH --output=midway/o_%A_%a.out
## #SBATCH --error=midway/e_%A_%a.err
## #SBATCH --qos=covid-19
## #SBATCH --account=covid-19 -p broadwl

## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Load R
module load R/4.0.0

## Run the Rt estimation pipeline
Rscript estimate_cases_epinow2.R --var=$SLURM_ARRAY_TASK_ID --midway=TRUE --debug=FALSE --outpath="../epinow2_cases_estimates/2020-11-05_cases_v1.2.1/"

## Reformat the outputs, and copy them into a single .csv that lives in:
## if debug=FALSE -  ../figs/cli_nadmit_TODAY/, with the corresponding estimates from our EpiEstim pipeline
## if true, save output .csv files to the outpath specified above
Rscript summarize_epinow2.R --debug=TRUE
