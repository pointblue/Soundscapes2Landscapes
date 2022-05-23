#!/bin/bash
#

# inference.sh
# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 8/18/2020
# PURPOSE: create slurm job (part1) based on number of files to compute bird species probabilities (part2)
# NOTE: user inputs include lines 31 - 41

# Part 1: Create SLURM array based on nuber of recordings to derive probs
# - SLURM -
#SBATCH --time=00:02:00
#SBATCH --output='/projects/tropics/users/cquinn/s2l/code/bird_cnn_inference/inference.out'

echo "Entering first slurm script"

# --- SLURM Settings ---
arrayLim=2000        # how many array jobs to have at once
files_per_task=100   # how large each job should be, number of files
timeReq="01:00:00"   # HH:mm:ss
memoryReq="2GB"      # low memory usage regardless of files_per_task or time

# where output, first .sh script will be written to
script_dir='/projects/tropics/users/cquinn/s2l/code/bird_cnn_inference/'


# --- USER INPUTS ---
# --- MODEL BUILDS ---
# CNN trained on XC_warbler and S2L bird data. Comment out desired model for each run.
#cnn_path='/scratch/cq73/projects/S2L/cnn_inference/data/cols_54cls_mobnet_full_finetune/my_model/' # MobileNet v2
#cnn_path='/scratch/cq73/projects/S2L/cnn_inference/data/cols_54cls_Resnet101/my_model/'            # ResNet v2 101
cnn_path='/scratch/cq73/projects/S2L/cnn_inference/data/cols_54cls_Resnet50/my_model/'              # ResNet v2 50

# --- DATA & RESULT DIR Settings ---
# results dir should have a sigmoid folder within the target already built
results_dir='/projects/tropics/users/cquinn/s2l/S2L_RA_work/cnn_inference/2021/cols_54cls_Resnet50/' # Model output folder matches model name
todo_csv='/projects/tropics/users/cquinn/s2l/S2L_RA_work/cnn_inference/2021/toDo_RN50_toDo.csv'      # CSV listing files still requiring inference (difference completed vs all files)
logs_path='/projects/tropics/users/cquinn/s2l/S2L_RA_work/cnn_inference/2021/logs/'                  # job logs
num_files=749296                                                                                     # number of remaining files for inference (rows in csv)


# --- Data preprocessing ---
date_time=`date +%Y%m%d_%H%M%S`
echo "Working data dir : "$data_dir
echo "Number of files = "$num_files

ntask=$(echo "scale=2 ; $num_files / $files_per_task" | bc) # calculate how many jobs there should be
echo "Number of tasks ="$ntask

njobs=$(echo "($ntask+0.999)/1" | bc) # ceiling round number of tasks
echo "Number of jobs ="$njobs

slurmLogName=$script_dir'logs/%a.out' # Specify the basename for the output log file


# PART 2: create SLURM array based on ntasks for inference
# --- SLURM ARRAY ---
# Create the slurm array job script in the slurm folder
cd $script_dir
scriptName='sbatch/sbatch_inference_'$date_time'.sh'

cat > $scriptName <<EOT
#!/bin/bash

# - SLURM -
#SBATCH --job-name=inf                   # defined name for jobstats
#SBATCH --output=$slurmLogName           # location for .out file
#SBATCH --partition=core                 # partition name
#SBATCH --time=$timeReq                  # walltime defined above
#SBATCH --cpus-per-task=1                # no parallel CPU proc needed
#SBATCH --mem=$memoryReq                 # mem in GB
#SBATCH --array=[1-$njobs]%$arrayLim     # number of array tasks with throttle arrayLim 



start=$(date +%s)
date_time_inner=`date +%Y%m%d_%H%M%S`
echo "The starting date_time: " \$date_time_inner
echo
echo "SLURM_JOBID: "\$SLURM_JOBID
echo "SLURM_ARRAY_JOB_ID: "\$SLURM_ARRAY_JOB_ID
echo "SLURM ARRAY TASK ID: "\$SLURM_ARRAY_TASK_ID
echo


# - MODULES -
module load anaconda3/2020.07  # load anaconda
conda activate tfgpu           # activate cnn env

echo
echo "---------Entering script---------"
# --- SCRIPT ---
python3.7 /projects/tropics/users/cquinn/s2l/code/bird_cnn_inference/5b_inference_HPC_model_with_csv.py \$SLURM_ARRAY_TASK_ID $files_per_task $todo_csv $results_dir $cnn_path $logs_path

# - ENDING -
echo "Ended at:"
date
echo
end=$(date +%s)
totTime=\$((end-start))
echo Total time: \$totTime sec

EOT

# Run the slurm array job script
sbatch $scriptName
