#!/bin/bash
#SBATCH --time=04:30:00
#SBATCH --output=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.out
#SBATCH	--error=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.err
#SBATCH --nodes=12
#SBATCH --array=1-12
#SBATCH --ntasks-per-node=4
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=kgostic
#SBATCH --qos=covid-19
#SBATCH --account=covid-19 -p broadwl


## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Load R
module load R/4.0.0

## Get latest data
##  !!!!! YOU MUST UPDATE THE DATE IN THIS LINE WITH EACH RUN !!!!!!!!!
cp /project2/cobey/covid-modeling/rt-pipeline-09-2020/data/cli_admissions_2929-10-28.csv ../data/cli_admissions_latest.csv 

## Run the Rt estimation pipeline
Rscript estimate_cli_epinow2.R --var=$SLURM_ARRAY_TASK_ID --midway=TRUE

## Reformat the outputs, and copy them into a single .csv that lives in:
##  ../figs/cli_nadmit_TODAY/, with the corresponding estimates from our EpiEstim pipeline
Rscript summarize_epinow2_cli.R
