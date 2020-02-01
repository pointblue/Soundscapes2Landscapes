# Title: fitSDMensemble_loop.R
# Author: Patrick Burns [pb463@nau.edu] & Leo Salas [lsalas@pointblue.org]
# Purpose: accepts a variety of arguments which are then passed to SDMensemble_singleBoot.R
# TODO: 
###############################################################################



####
#### 1. Inputs
####

# Path of github repo
gitpath = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/repos/Soundscapes2Landscapes/"

# Read in batch file
batchFile = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/results/s20200124/SDMbatchList_2yr_NoBal_AllwGEDI_20200124.csv"

# Path to save results
svpath = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/results/s20200124/"

# Log path
logdir = paste0(svpath,"logs/")


####
#### 2. Processing
####

# some checks
if(substr(gitpath,nchar(gitpath),nchar(gitpath))!="/"){gitpath<-paste0(gitpath,"/")}
if(!is.null(svpath) && substr(svpath,nchar(svpath),nchar(svpath))!="/"){svpath<-paste0(svpath,"/")}
if(!is.null(logdir) && substr(logdir,nchar(logdir),nchar(logdir))!="/"){logdir<-paste0(logdir,"/")}

# read batch file
jobs = read.csv(batchFile, header = FALSE)
colnames(jobs) = c("boots", "specs", "spatRes", "tempRes", "gediUsed", "vifUsed", "balanced", "varType")

# Loop through jobs. This is designed for a single machine but could be parallelized using a package like doParallel
for (i in 1:nrow(jobs)){
  spp<-as.character(jobs$specs[i])
  rez<-as.character(jobs$spatRes[i])
  yrsp<-as.character(jobs$tempRes[i])
  gedi<-as.character(jobs$gediUsed[i])
  sinf<-FALSE
  tst<-FALSE 
  vif<-as.character(jobs$vifUsed[i]) 
  bal<-as.character(jobs$balanced[i])
  iter<-as.character(jobs$boots[i])
  varT<-as.character(jobs$varType[i])
  
  ## check that the git folder exist
  if(!dir.exists(gitpath)){	# no gitpath info - can't go further
    print("The path to the Soundscapes2Landscapes directory is incorrect or does not exist. Please provide a correct path.", quote=FALSE)
    print("No tests performed; no logs generated.", quote=FALSE)
  }else{	#check/create the logs folder, see if we can start log for the test...
    print("Found git directory...", quote=FALSE)
    ldt<-0
    if(is.null(logdir)){	#no log dir provided
      logdir<-paste0(gitpath,"logs/")
      if(!dir.exists(logdir)){
        zz <- try(dir.create(logdir),silent=T)
        if(inherits(zz,"try-error")){	#failed to create dir
          print("Wrong log directory path. Could not create log directory in the Soundscapes2Landscapes folder. Please check access permissions, or run test with appropriate credentials, or provide a valid path.", quote=FALSE)
          print("No tests performed; no logs generated.", quote=FALSE)
        }else{	#success creating log dir
          print(paste0("No logs directory provided, so created '",logdir," directory."), quote=FALSE)
          ldt<-1
        }
      }else{
        print(paste("Found logs directory:",logdir), quote=FALSE)
        ldt<-1
      }
    }else{	# valid log dir provided
      print("Valid logs directory found...", quote=FALSE)
      ldt<-1
    }
    if(ldt==1){	# have valid log dir, then... 
      ## open connection to log file
      filen<-paste("SDMensemble",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
      logfile<-paste(logdir,filen,".log",sep="")
      zz <- try(file(logfile, "w"),silent=T)
      if(inherits(zz,"try-error")){
        print("Could not create log file. Please check access permissions or run test with appropriate credentials.", quote=FALSE)
        print("No tests performed; no logs generated.", quote=FALSE)
      }else{	#successful creating log file - start log
        print("Starting log file and tests...", quote=FALSE)
        ## continue with tests....
        cat("Log report testing the optimized SDM fitting script", paste("Started", format(Sys.time(),"%Y-%m-%d %H:%M:%S")), file = zz, sep = "\n", append=TRUE)
        cat("\n","\n",file = zz, append=TRUE)
        
        cat(paste("Valid git directory:",gitpath), file = zz, sep = "\n", append=TRUE)
        cat(paste("Found or created logs directory:",logdir), file = zz, sep = "\n", append=TRUE)
        cat("Testing validity of results directory:", file = zz, sep = "\n", append=TRUE)
        #test that the results dir is there or that can create in gitpath
        if(is.null(svpath) || !dir.exists(svpath)){	
          cat("   Missing or invalid directory provided where to store results.", file = zz, sep = "\n", append=TRUE)
          svpath<-paste0(gitpath,"results/")
          if(!dir.exists(svpath)){
            rr <- try(dir.create(svpath),silent=T)
            if(!inherits(rr,"try-error")){
              restest<-"SUCCESS"
            }else{
              restest<-"FAILED - WARNING!!!"
            }
            cat(paste0("   Testing that one can be created at ",gitpath,"results/ ...",restest), file = zz, sep = "\n", append=TRUE)
          }else{
            cat(paste0("   Found directory where to save results at ",svpath), file = zz, sep = "\n", append=TRUE)
          }
          
        }else{
          cat("   Valid directory where to save results provided", file = zz, sep = "\n\n", append=TRUE)
        }
        
        #test presence of all libraries needed
        cat("Testing that all needed libraries are installed and can be loaded", file = zz, sep = "\n", append=TRUE)
        libs<-c("rminer","raster","dismo","plyr","data.table","xgboost","doParallel","caret","kernlab","psych","compiler","dplyr","spThin","randomForest");
        libtest<-as.data.frame(sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE));names(libtest)<-"installed"
        write.table(libtest, row.names = TRUE, col.names = FALSE, file=zz, append=TRUE)
        cat("\n","\n",file = zz, append=TRUE)
        
        #report the arguments passed in the test call
        cat("Arguments passed or created in script call:", file = zz, sep = "\n", append=TRUE)
        Parameter<-c("testonly","gitpath","savepath","logdir","species","resolution","yearspan","VIF", "Balance", "sessionInfo")
        Value<-c(tst,gitpath,svpath,logdir,spp,rez,yrsp,vif,bal,sinf)
        optdf<-data.frame(Parameter,Value)
        write.table(optdf, row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
        cat("\n","\n",file = zz, append=TRUE)
        
        #test the presence of the data files
        if (vif == "VIF"){
          pth250<-paste0(gitpath,"sdmTool/data/Birds/250M/deflated_250M.RData")
          pth500<-paste0(gitpath,"sdmTool/data/Birds/500M/deflated_500M.RData")
          pth1000<-paste0(gitpath,"sdmTool/data/Birds/1000M/deflated_1000M.RData")
          if(!file.exists(pth250) || !file.exists(pth500) || !file.exists(pth1000)){
            cat("Testing presence of data files... WARNING: Some of the data files were not found", file = zz, sep = "\n", append=TRUE)
          }else{
            cat("Testing presence of data files... Found all the needed data files", file = zz, sep = "\n", append=TRUE)
          }
        } else if (vif == "NoVIF") {
          pth250<-paste0(gitpath,"sdmTool/data/Birds_NoVIF/250M/deflated_250M.RData")
          pth500<-paste0(gitpath,"sdmTool/data/Birds_NoVIF/500M/deflated_500M.RData")
          pth1000<-paste0(gitpath,"sdmTool/data/Birds_NoVIF/1000M/deflated_1000M.RData")
          if(!file.exists(pth250) || !file.exists(pth500) || !file.exists(pth1000)){
            cat("Testing presence of data files... WARNING: Some of the data files were not found", file = zz, sep = "\n", append=TRUE)
          }else{
            cat("Testing presence of data files... Found all the needed data files", file = zz, sep = "\n", append=TRUE)
          }
        } else {
          print("Error interpreting VIF input")
        }
        #HERE determine if running the model fitting script (testonly=FALSE)
        #If so, source the sdmfit file and pre-compile the sdm fitting function
        if(tst==FALSE){		
          source(paste0(gitpath,"sdmTool/scripts/SDMensemble_singleBoot.R"))
          fitCaseModelCmp <- try(cmpfun(fitCaseModel,options=list(suppressAll=TRUE)),silent=T)
          cmpflag<-1
          if(inherits(fitCaseModelCmp,"try-error")){
            cat("Could not compile function: fitCaseModel. Please review the function.", file = zz, sep = "\n")
            cmpflag<-0
          }else{
            cat("Successfuly compiled function: fitCaseModel", file = zz, sep = "\n")
          }
          X<-list(gitpath=gitpath,svpath=svpath,rez=rez,spp=spp,yrsp=yrsp,gedi=gedi,vif=vif,bal=bal,iter=iter,varT=varT)
          if(cmpflag==1){
            res<-fitCaseModelCmp(X=X, logf=zz, percent.train=0.8)
          }else{
            res<-fitCaseModel(X=X, logf=zz, percent.train=0.8)
          }
          
          
        }else{
          cat("Not performing a model fit - just testing requirements", file = zz, sep = "\n", append=TRUE)
          res<-"No model fit requested"
        }
        ###
        
        #end the log
        cat("\n","End of test.","\n","\n",file=zz, append=TRUE)
        
        if(sinf==TRUE){
          w<-unlist(sessionInfo())
          tdf<-data.frame(param=names(w),value=w);row.names(tdf)<-NULL
          cat("SessionInfo:",file=zz,sep="\n", append=TRUE)
          write.table(tdf, row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
        }
        close(zz)
        
        print(res)
        print(paste("Batch call completed. Check file",logfile,"for results \n"), quote=FALSE)
        
      }
    }
    
  }
  
} # end jobs loop