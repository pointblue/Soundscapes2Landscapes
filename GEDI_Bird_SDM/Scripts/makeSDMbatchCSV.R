# Title: makeSDMbatchCSV.R
# Author: Patrick Burns [pb463@nau.edu]
# Purpose: make a batch files with all combinations of different arguments for SDMensemble_batch.R
# TODO: 
###############################################################################



####
#### 1. Inputs
####

# Path and name of CSV batch file to be written
outFile = file("//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/results/s20200113/SDMbatchList_2yr_Bal_AllwGEDI_20200113.csv", open = "wb")

# Number of bootstraps
nboot = 500

# 25 species of interest
specs = c("ACWO", "AMGO", "BEWR", "BHGR", "BLPH", 
          "BRBL", "BUSH", "CALT", "CAQU", "CBCH", 
          "DEJU", "HOFI", "LEGO", "MODO", "NOFL", 
          "NOMO", "NUWO", "OATI", "RWBL", "SOSP", 
          "SPTO", "STJA", "WCSP", "WEBL", "WESJ")

# 3 different spatial resolutions for the analysis. Options: "250M", "500M", "1000M"
spatRes = c("250M", "500M", "1000M")

# 2 years of simulated GEDI data. Keep at "2yr"
tempRes = c("2yr")

# Is GEDI simulation being used? Options: "TRUE", "FALSE"
gediUsed = c("TRUE")

# Whether or not to use VIF-reduced predictor variables. Options: "VIF", "NoVIF"
vifUsed = c("VIF")

# How to handle presence/absence ratios? Options: "NoBal", "Bal", "varA"
balanced = c("Bal")

# Which group of variables to use? Options: "All_wGEDI", "All_woutGEDI", "justBCM", "justGEDI", "justNDVI"
varType = c("All_wGEDI")



####
#### 2. Processing
####

# All combinations of the above vectors
boots = seq(1,nboot,1)
allCombos = expand.grid(boots, specs, spatRes, tempRes, gediUsed, vifUsed, balanced, varType)

# Output csv without CR and the end of every line
write.table(x = allCombos, file = outFile, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE, eol = "\n")
close(outFile)
