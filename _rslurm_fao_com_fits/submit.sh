#!/bin/bash
#
#SBATCH --array=0-1
#SBATCH --job-name=fao_com_fits
#SBATCH --output=slurm_%a.out

#SBATCH --partition=sesynctest

#SBATCH --time=1:00:00
/usr/lib/R/bin/Rscript --vanilla slurm_run.R
