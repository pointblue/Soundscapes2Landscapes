#!/bin/bash

# warbleR_XC.sh
# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 2020-Dec-7
# PURPOSE: use two primary Xeno-Canto audio directories (A-M and N-Z) to autodetect possible ROIs using warbleR

# ----- INPUTS -----
# - SLURM -
#SBATCH --job-name=NZ                                     # dataset analyzed
#SBATCH --output=warbleR_%A_%a.log
#SBATCH --partition=core
#SBATCH --time=06:00:00                                   # long processing time here
#SBATCH --mem=16GB                                        # tasks are highly variable but rarely hit 16 GB  
#SBATCH --cpus-per-task=4                                 # allow 4 parallel CPUs; hardcoded line 61 in warbler_XC.R                             
#SBATCH --chdir=/scratch/cq73/projects/S2L/cnn_pretrain/
#SBATCH --array=[1-106]                                   # number of classes/folders in XC dataset (AM = 153 or NZ = 106)

# ----- PROCESSING -----
# - STARTING -
start=`date +%s`
echo "Started at:"
date
echo
echo "SLURM_JOBID: "$SLURM_JOBID
echo

#AM
#data_dir="/scratch/cq73/projects/S2L/cnn_pretrain/data/A-M/" #n = 153      # where XC data are located
#out_dir="/scratch/cq73/projects/S2L/cnn_pretrain/results/XC_warbleR/AM/"   # where warbleR IDs will be saved for melspec generation

#NZ
data_dir="/scratch/cq73/projects/S2L/cnn_pretrain/data/N-Z/" #n = 106       # where XC data are located
out_dir="/scratch/cq73/projects/S2L/cnn_pretrain/results/XC_warbleR/NZ/"    # where warbleR IDs will be saved for melspec generation

# - MODULES -
module load R

# - Scripts -
echo "---------Entering R Script---------"
Rscript /scratch/cq73/projects/S2L/cnn_pretrain/code/melspec_gen/warbleR_XC.R $SLURM_ARRAY_TASK_ID $data_dir $out_dir

# - ENDING -
echo "Ended at:"
date
echo
end=`date +%s`
echo $end
