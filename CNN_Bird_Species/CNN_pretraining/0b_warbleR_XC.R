ll = "/home/cq73/R/3.5/"
library(warbleR, lib.loc = ll)
library(dplyr, lib.loc = ll)
library(tidyr, lib.loc = ll)

# FEED IN EACH CLASS FROM SBATCH - index related to the number of classes in the parent dir 
##########################################
## Use this section on monsoon
args = commandArgs(TRUE)
# load array folder
## load directories and index on Slurm ID
slurm_id = args[1]
slurm_id = as.integer(slurm_id)
parent_dir = args[2] 
outdir = args[3]

# where .wav files live within class subfolders
workdir = list.dirs(parent_dir)[slurm_id + 1]

# get class name
temp = strsplit(workdir,"/")[[1]]
temp_len = length(temp)
temp = temp[[temp_len]]

# where outputs will live
outfile = paste0(outdir, temp, "_warbleR_ad.csv") #dir, class, file name

# list sound files to analyze
targets = list.files(workdir, full.names = FALSE, pattern = c(".mp3")) # list mp3 files
print(paste("Slurm ID:", slurm_id))
print(paste("Working dir:", workdir))
print(paste("Number of files in dir = ", length(targets)))
print(paste("Results dir:", outdir))
print(paste("Results files:", outfile))

# convert all mp3s to wavs in same dir
tryCatch({
  mp32wav(path = workdir)
  checkwavs(path = workdir) # make sure that all of the converted wavs are not corrupted
  }, error = function(e) {
  cat("ERROR :", conditionMessage(e), "\n")
  }
)

# relist files which should be only .wav files now for warbleR
targets = list.files(workdir, full.names = FALSE, pattern = c(".wav"))

# enter warbler AutoDetect function
ad = list()
if(file.exists(outfile)){
  print(paste(outfile, "already exists, skipping process..."))
  print("--------------------------------------------------")
  
} else {  
  for(i in 1:length(targets)){
    tryCatch({
      temp_target = targets[i]
      print(paste0(i, " : ", temp_target))
      
      ad[[i]] = autodetec(threshold = 5, env = "hil", ssmooth = 300, power=1,
                        bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, parallel = 4,
                        wl = 300, ls = FALSE, sxrow = 2, rows = 4, mindur = 0.1, maxdur = 2, set = TRUE, 
                        path = workdir, 
                        flist = temp_target,
                        redo = TRUE)
      
      }, error = function(e){
        cat("ERROR :", conditionMessage(e), "\n")
        ad[[i]] = c(sound.file = temp_target, selec = NA, start = NA, end = NA)
        
      }, warning = function(w){
        cat("WARN :", conditionMessage(w), "\n")
        ad[[i]] = c(sound.file = temp_target, selec = NA, start = NA, end = NA)
        
      }
      
      )
  }
  
  # write df of processed files
  ad = do.call(rbind, ad)
  write.csv(ad, file = outfile, row.names = FALSE)
}
