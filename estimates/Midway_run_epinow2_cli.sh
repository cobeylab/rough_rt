#!/bin/bash
#SBATCH --time=03:00:00
#SBATCH --output=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.out
#SBATCH	--error=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.err
#SBATCH --nodes=12
#SBATCH --array=2-13
#SBATCH --ntasks-per-node=4
#SBATCH --mem-per-cpu=4000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=kgostic
#SBATCH --partition=cobey


## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Load R
module load R/4.0.0

## Get latest data
cp /project2/cobey/covid-modeling/rt-pipeline-09-2020/data/cli_admissions.csv ../data/cli_hosp.csv 

## Run the Rt estimation pipeline

Rscript estimate_cli_epinow2.R