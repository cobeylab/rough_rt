#!/bin/bash
#SBATCH --time=04:30:00
#SBATCH --output=midway/%A_%a.out
#SBATCH	--error=midway/%A_%a.err
#SBATCH --array=1-12
#SBATCH --ntasks-per-cpu=4
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=kgostic
#SBATCH --partition=cobey


## #SBATCH --qos=covid-19
## #SBATCH --account=covid-19 -p broadwl


## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Load R
module load R/4.0.0

## Get latest data
cp /project2/cobey/covid-modeling/rt-pipeline-09-2020/data/cli_admissions_2020-10-28.csv ../data/cli_admissions_latest.csv 

## Run the Rt estimation pipeline

Rscript estimate_cli_epinow2.R --var=$SLURM_ARRAY_TASK_ID --midway=TRUE --debug=FALSE
