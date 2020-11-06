#!/bin/bash
#SBATCH --time=20:00:00
#SBATCH --array=1-12
#SBATCH --ntasks-per-cpu=4
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=kgostic
#SBATCH --partition=cobey
#SBATCH --job-name=v1.2.1_exact

## #SBATCH --output=midway/o_%A_%a.out
## #SBATCH	--error=midway/e_%A_%a.err
## #SBATCH --qos=covid-19
## #SBATCH --account=covid-19 -p broadwl

## Remove local copy of epinow2 from rpath
rm -r /home/kgostic/R/x86_64-pc-linux-gnu-library/4.0/EpiNow2

## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Set outpath
OP='../2020-11-04_EpiNow2v1.2.1_cli_exact'

## Load R
module load R/4.0.0

## Get latest data
##  !!!!! YOU MUST UPDATE THE DATE IN THIS LINE WITH EACH RUN !!!!!!!!!
cp /project2/cobey/covid-modeling/rt-pipeline-09-2020/data/cli_admissions_2020-11-04.csv ../data/cli_admissions_latest.csv 
echo "copied latest data to latest.csv"

## Run the Rt estimation pipeline
Rscript estimate_cli_epinow2.R --var=$SLURM_ARRAY_TASK_ID --midway=TRUE --debug=FALSE --outpath=$OP
echo "Ran epinow2"

## Reformat the outputs, and copy them into a single .csv that lives in:
## if debug=FALSE -  ../figs/cli_nadmit_TODAY/, with the corresponding estimates from our EpiEstim pipeline
## if true, save output .csv files to the outpath specified above
Rscript summarize_epinow2.R --outpath=$OP

