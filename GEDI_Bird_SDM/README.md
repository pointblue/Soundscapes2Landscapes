Github documentation for S2L sdmTool

We use a combination of R and bash scripts to run species distribution models (SDM). 
The following R packages are required for running a single bootstrap SDM:
caret, data.table, dismo, doParallel, dplyr, fmsb, ggplot2, kernlab, lmtest, optparse, plyr, psych, randomForest, raster, rminer, spThin, unmarked, xgboost, XLConnect 


Bash scripts utilize SLURM Workload Manager (https://slurm.schedmd.com/documentation.html) for distributing individual bootstraps across available processors on Northern Arizona University’s High Performance Computing System (Monsoon; https://in.nau.edu/hpc/). These bash scripts should also work on other HPC systems that use SLURM, however we have not tested these scripts on another HPC. Please let us know if you try on your HPC and run into issues. 


Below we outline the steps for running SDMs

0. Gather bird species observation (getAKNdata.R)
This script queries the Avian Knowledge Network (AKN) database, hosted by Point Blue. We constrained the query with certain date and survey filters. We also include survey effort information. Since the database is managed by Point Blue and not accessible by public query we do not include this script. The map linked below, however, may be useful for visualizing observations in the AKN database:
http://avianknowledge.net/index.php/observations-map/
 
 
1. Associate presence and absence with raster grids (filterSpeciesData.R)
This script attributes grid cells from 250-m, 500-m, and 1000-m rasters with presence and absence observations. This information is saved as a table for each species. The output from this script is saved in Data/Birds/.


2. Combine bird observation data with predictor variables (attributeBirds_wStack.R)
This script attributes a table of bird observations (at each grid resolution) with predictor variables from the same grid cell. Initially we start with 55 scaled (subtract the mean and divide by the standard deviation) predictor variables. We focus on the 250-m grid and then run a variance inflation factor (VIF) function to find the most informative predictor variables. The most informative predictor variables (n = 23 in our analysis) from the 250-m grid are then selected from the 500-m and 1000-m grids.  


3. Prepare batch file for processing (makeSDMbatchCSV.R)
This script prepares a batch .csv file where each row contains a unique combination of arguments to be passed to the SDM ensemble script. For 25 species, 3 spatial resolutions, and 500 bootstraps we have a batch file with 37,500 model scenarios. 


4. Generate ensemble models for each species, resolution, and bootstrap. There are two ways to do this: with SLURM on HPC (fast and preferred, see a. below) or with FOR loop (see b. below)
a. Distributed processing with SLURM on an HPC (SDMensemble_batch.R,  SDMensemble_singleBoot.R, S2L_SDMensemble_Sbatch.sh).
Basically we use SLURM arrays on an HPC to distribute jobs across available machines. The .sh script creates the arrays using a batch file and then passes arguments to SDMensemble_batch.R which parses the arguments and then generates an ensemble using code from SDMensemble_singleBoot.R.

b. Loop (fitSDMensemble_loop.R)
This code uses a simple loop and is mostly for demonstration purposes for users who might not have access to an HPC. This is designed for a single machine but could be parallelized using an R package like doParallel


5. Aggregate results from all bootstraps (S2L_combineResults_Sbatch.sh)
This script combine goodness of fit (GOF) and importance (imp) CSV files from all bootstraps. The script utilizes SLURM arrays so users without access to an HPC with SLURM may need to either aggregate these files by hand or write code in another language such as R.  


6. Generate prediction and uncertainty maps (SDMmaps_weightedPredErr.R)
This script takes predictions from 7 different models and all bootstraps and, for each cell, calculates the weighted average prediction values based on adjusted AUC of the individual model-bootstrap. We also calculate plus/minus one standard error from the mean predicted value and map magnitude of the difference between the upper and lower standard error bound. 


7. Make figures (Burns_etal_20xx_SonomaSDM_FinalFigures.Rmd)
This R notebook includes code for some figures found in Burns et al. 2020. Incorporating canopy structure from simulated GEDI lidar into bird species distribution models. Environmental Research Letters.
