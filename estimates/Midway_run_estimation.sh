#!/bin/bash
#SBATCH --time=02:00:00
#SBATCH --output=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.out
#SBATCH	--error=/project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/midway/%A.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=14
#SBATCH --mem-per-cpu=4000
#SBATCH --mail-type=END,FAIL,TIME_LIMIT
#SBATCH --mail-user=parevalo
#SBATCH --partition=cobey


## Set working directory
cd /project2/cobey/covid-modeling/rt-pipeline-09-2020/estimates/

## Load R
module load R/4.0.0

## Get latest data
cp /project2/cobey/covid-civis/raw_data/case_data_public/idph_public_restore_region_UIUC_removed.csv ../data/idph_public_restore_region.csv 
cp /project2/cobey/covid-civis/raw_data/case_data_public/idph_public_covid_region_UIUC_removed.csv ../data/idph_public_covid_region.csv

## Run the Rt estimation pipeline
#Rscript estimate_from_cases.R
#Rscript estimate_from_hospitalizations.R
Rscript estimate_from_cli.R