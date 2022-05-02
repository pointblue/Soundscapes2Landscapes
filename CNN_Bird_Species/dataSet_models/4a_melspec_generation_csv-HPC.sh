#!/bin/bash
#

# melspec_generation_csv-HPC.sh
# ----- ABOUT -----
# By Colin Quinn, NAU
# cq73@gmail.com
# Created: 7/15/2020
# Updated: July-2021

# - SLURM -
#SBATCH --time=00:02:00
#SBATCH --output='/projects/tropics/users/cquinn/s2l/code/melspec_generation/melspec_generation.out'

echo "Entering first slurm script"
# --- SLURM Settings ---
arrayLim=2500 # how many array jobs to have at once
files_per_task=1 # how large each job should be, number of files
timeReq="00:15:00" # time for each task
memoryReq="1GB" # Mem for each task

# where output, first .sh script will be written to
script_dir='/projects/tropics/users/cquinn/s2l/code/melspec_generation/bash/'

# --- DATA & RESULT DIR Settings ---
# CSV                                                 
todo_csv='/projects/tropics/users/cquinn/s2l/melspecs/toDo_melspecs.csv' 
num_files=200
audio_dir='/scratch/cq73/projects/S2L/audio/'
wav_ext='.WAV'
results_dir='/projects/tropics/users/cquinn/s2l/melspecs/melspecs_2020-2021/'


# --- Data preprocessing ---
date_time=`date +%Y%m%d_%H%M%S`
#num_files=$(find $data_dir -type f | wc -l) # the number of files
echo "Working data dir : "$todo_csv
echo "Number of files = "$num_files

ntask=$(echo "scale=2 ; $num_files / $files_per_task" | bc) # calculate how many jobs there should be
echo "Number of tasks ="$ntask

njobs=$(echo "($ntask+0.999)/1" | bc)
echo "Number of jobs ="$njobs


# Specify the basename for the output log file
slurmLogName=$script_dir'logs/'$date_time'_%a.out'


# --- SLURM ARRAY ---
# Create the slurm array job script in the slurm folder
scriptName='sbatch_melspec_gen_'$date_time'.sh'

cat > $scriptName <<EOT
#!/bin/bash

# Submit melspec python generation tasks based on total files / task size
# - SLURM -
#SBATCH --job-name=melGen               # SLURM name
#SBATCH --output=$slurmLogName          # output file for each SLURM task
#SBATCH --partition=core                # HPC spec
#SBATCH --time=$timeReq                 # pass time req
#SBATCH --mem=$memoryReq                # pass mem req
#SBATCH --array=[1-$njobs]%$arrayLim    # how many tasks will run throttled by arrayLim (aka number of tasks running at once)

start=$(date +%s)
date_time_inner=`date +%Y%m%d_%H%M%S`
echo "The starting date_time: " \$date_time_inner
echo
echo "SLURM_JOBID: "\$SLURM_JOBID
echo "SLURM_ARRAY_JOB_ID: "\$SLURM_ARRAY_JOB_ID
echo "SLURM ARRAY TASK ID: "\$SLURM_ARRAY_TASK_ID
echo


# - MODULES -
module load anaconda3/2020.07
conda activate melspec


echo
echo "---------Entering script---------"
# --- SCRIPT ---
python3.7 /projects/tropics/users/cquinn/s2l/code/mfcc_generation/4b_melspec_generation_csv.py \$SLURM_ARRAY_TASK_ID $files_per_task $todo_csv $results_dir $audio_dir $wav_ext

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
