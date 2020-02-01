#!/bin/bash

# ----- ABOUT -----
# Title: S2L_combineResults_Sbatch.sh
# Author: Patrick Burns [pb463@nau.edu]
# Purpose: Use bash to combine Importance and GOF CSV results
# TODO:

# ----- INPUTS -----
# - SLURM -
#SBATCH --job-name=S2L_combineRes
#SBATCH --output=/scratch/pb463/projects/S2L/SDM/slurm/S2L_SDMbatch_combRes_%A_%a.log
#SBATCH --partition=all
#SBATCH --time=00:10:00
#SBATCH --mem=50M
#SBATCH --array=1-25

# Input path
baseResPath='/scratch/pb463/projects/S2L/SDM/results/s20200113/All_woutGEDI/'

# Species 
specRun="ACWO AMGO BEWR BHGR BLPH BRBL BUSH CALT CAQU CBCH DEJU HOFI LEGO MODO NOFL NOMO NUWO OATI RWBL SOSP SPTO STJA WCSP WEBL WESJ"


# ----- PROCESSING -----
# Switch to species results directory
cd $baseResPath
cd mergedResults'/'

# Which species
s=$(echo $specRun | cut -d " " -f $SLURM_ARRAY_TASK_ID)

# Importance by species 
outImp=$s'_imp_merged.csv'
head -1 ../250M/$s*'_250M_'*'i1_modelResults_Optimized_imp.csv' > $outImp
tail -n +2 -q ../250M/$s*'_imp.csv' >> $outImp
tail -n +2 -q ../500M/$s*'_imp.csv' >> $outImp
tail -n +2 -q ../1000M/$s*'_imp.csv' >> $outImp

# GOF by species 
outGOF=$s'_gof_merged.csv'
head -1 ../250M/$s'_250M_'*'i1_modelResults_Optimized_gof.csv' > $outGOF
tail -n +2 -q ../250M/$s*'_gof.csv' >> $outGOF
tail -n +2 -q ../500M/$s*'_gof.csv' >> $outGOF
tail -n +2 -q ../1000M/$s*'_gof.csv' >> $outGOF


