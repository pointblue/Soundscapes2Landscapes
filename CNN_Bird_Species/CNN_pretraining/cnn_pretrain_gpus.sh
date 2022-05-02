#!/bin/bash

# cnn_test.sh
# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 2020/Nov/9
#
# Purpose: pretrain IMAGENET weighted CNN architectures (resnet50, resnet101, mobilenet) with XC data
# you need to always have a cpu per gpu


# ----- INPUTS -----
# - SLURM -
#SBATCH --job-name=RES1
#SBATCH --output=/scratch/cq73/projects/S2L/cnn_pretrain/results/cnn_pretrain_%A.log
#SBATCH --partition=core
#SBATCH --time=24:00:00
#SBATCH --mem=350GB
#SBATCH --cpus-per-task=10
#SBATCH --chdir=/scratch/cq73/projects/S2L/cnn_pretrain/
#SBATCH --gres=gpu:tesla:1


# ----- PROCESSING -----
# - STARTING -
start=`date +%s`
echo "Started at:"
date
echo
echo "SLURM_JOBID: "$SLURM_JOBID
echo

# - MODULES -
module load anaconda3/2020.07 #.02 for older envs
module load cuda/10.2

# - PYTHON ENV -
echo "Loading conda environment"
conda activate tfgpu
echo

# - Scripts -
echo "---------Entering Python Script---------"
# One script for each CNN architecture
#python3.7 /scratch/cq73/projects/S2L/cnn_pretrain/code/cnn_train/mobnet_pretrain.py
#python3.7 /scratch/cq73/projects/S2L/cnn_pretrain/code/cnn_train/resnet50v2_pretrain.py
python3.7 /scratch/cq73/projects/S2L/cnn_pretrain/code/cnn_train/resnet101v2_pretrain.py

# - ENDING -
echo "Ended at:"
date
echo
end=`date +%s`
echo $end