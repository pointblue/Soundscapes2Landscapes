#!/bin/bash

# ----- ABOUT -----
# Title: S2L_SDMensemble_Sbatch.sh
# Author: Patrick Burns [pb463@nau.edu]
# Purpose: Uses SLURM arrays to split observations and predictor variables into training/testing sets and then run various SDMs. Requires a CSV with input parameters for each model scenario
# TODO:

# ----- INPUTS -----
# - SLURM -
#SBATCH --job-name=S2L_SDMbatch
#SBATCH --output=/scratch/pb463/projects/S2L/SDM/slurm/S2L_SDMbatch_%A_%a.log
#SBATCH --partition=all
#SBATCH --time=00:45:00
#SBATCH --mem=3000M
#SBATCH --array=1-3750%500


# The git path
gitPath='/scratch/pb463/projects/S2L/repos/Soundscapes2Landscapes/'

# The list of scenarios for SDMs
batchFile='/scratch/pb463/projects/S2L/SDM/results/s20200113/SDMbatchList_2yr_NoBal_AllwoutGEDI_20200113.csv'

# Output path
baseResPath='/scratch/pb463/projects/S2L/SDM/results/s20200113/All_woutGEDI/'

# Log path
logsPath='/scratch/pb463/projects/S2L/SDM/logs/'


# ----- PROCESSING -----
# - STARTING -
start=`date +%s`
echo "Started at:"
date
echo "SLURM INFO"
echo "SLURM_JOBID: "$SLURM_JOBID
echo "SLURM_ARRAY_JOB_ID: "$SLURM_ARRAY_JOB_ID
echo "SLURM_ARRAY_TASK_ID: "$SLURM_ARRAY_TASK_ID
echo

# - MODULES -
cd /home/pb463/
module load R

# Get the inputs for the SDM
lineNum=$SLURM_ARRAY_TASK_ID

runNo=$(sed $lineNum'q;d' $batchFile | cut -d "," -f1)
species=$(sed $lineNum'q;d' $batchFile | cut -d "," -f2)
spatialRes=$(sed $lineNum'q;d' $batchFile | cut -d "," -f3)
tempRes=$(sed $lineNum'q;d' $batchFile | cut -d "," -f4)
useGEDI=$(sed $lineNum'q;d' $batchFile | cut -d "," -f5)
useVIF=$(sed $lineNum'q;d' $batchFile | cut -d "," -f6)
useBal=$(sed $lineNum'q;d' $batchFile | cut -d "," -f7)
varType=$(sed $lineNum'q;d' $batchFile | cut -d "," -f8)
# Specify the output path
fullResPath=$baseResPath

# Run the SDM script
Rscript /scratch/pb463/projects/S2L/repos/Soundscapes2Landscapes/sdmTool/scripts/SDMensemble_batch.R -g $gitPath -p $fullResPath -l $logsPath -s $species -r $spatialRes -y $tempRes -w $useGEDI -v $useVIF -b $useBal -i $runNo --varType $varType
echo

# - ENDING -
echo "Ended at:"
date
echo
end=`date +%s`
totTime=$((end-start))
echo Total time: $totTime sec