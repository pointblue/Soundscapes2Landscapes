# TODO: Add comment
# 
# Author: lsalas
###############################################################################


suppressPackageStartupMessages(require(optparse))	#need to load this library first

## describe the scritp function's batch call arguments
option_list = list(
		make_option(c("-t", "--testonly"), action="store", default=FALSE, type='logical', help="only test that the script can run? Defaults to FALSE"),
		make_option(c("-g", "--gitpath"), action="store", default="/home/ubuntu/Soundscapes2Landscapes/", type="character", help="path to the git directory. Default:"),
		make_option(c("-p", "--svpath"), action="store", default=NULL, type="character", help="path to the directory where results are stored."),
		make_option(c("-l", "--logdir"), action="store", default=NULL, type="character", help="path to the directory where logs are stored."),
		make_option(c("-s", "--species"), action="store", default="WESJ", type="character", help="species code; e.g., WESJ (default)"),
		make_option(c("-r", "--resolution"), action="store", default="1000M", type="character", help="spatial resolution; either 1000M (default), 500M or 250M"),
		make_option(c("-y", "--yearspan"), action="store", default="3yr", type="character", help="year span; either 1yr, 2yr or 3yr (default)"),
		make_option(c("-w", "--withgedi"), action="store", default=FALSE, type="logical", help="logical: include gedi variables? Defaults to FALSE"),
		make_option(c("-i", "--sessinfo"), action="store", default=FALSE, type="logical", help="include sessionInfo() in the log? Defaults to FALSE")
)


## parse the arguments 
opt = parse_args(OptionParser(option_list=option_list))
gitpath<-opt$g;if(substr(gitpath,nchar(gitpath),nchar(gitpath))!="/"){gitpath<-paste0(gitpath,"/")}
svpath<-opt$p;if(!is.null(svpath) && substr(svpath,nchar(svpath),nchar(svpath))!="/"){svpath<-paste0(svpath,"/")}
logdir<-opt$l;if(!is.null(logdir) && substr(logdir,nchar(logdir),nchar(logdir))!="/"){logdir<-paste0(logdir,"/")}
spp<-opt$s;rez<-opt$r;yrsp<-opt$y;gedi<-opt$w;sinf<-opt$i

## check that the git folder exist
if(!dir.exists(gitpath)){	# no gitpath info - can't go further
	print("The path to the Soundscapes2Landscapes directory is incorrect or does not exist. Please provide a correct path.", quote=FALSE)
	print("No tests performed; no logs generated.", quote=FALSE)
}else{	#check/create the logs folder, see if we can start log for the test...
	print("Found git directory...", quote=FALSE)
	ldt<-0
	if(is.null(logdir)){	#no log dir provided
		print("1") #############################################
		logdir<-paste0(gitpath,"logs/")
		if(!dir.exists(logdir)){
			print("1.1") #############################################
			zz <- try(dir.create(logdir),silent=T)
			if(inherits(zz,"try-error")){	#failed to create dir
				print("Wrong log directory path. Could not create log directory in the Soundscapes2Landscapes folder. Please check access permissions, or run test with appropriate credentials, or provide a valid path.", quote=FALSE)
				print("No tests performed; no logs generated.", quote=FALSE)
			}else{	#success creating log dir
				print(paste0("No logs directory provided, so created '",gitpath,"logs/' directory."), quote=FALSE)
				ldt<-1
			}
		}
	}else{	# valid log dir provided
		print("2") #############################################
		print("Found logs directory...", quote=FALSE)
		ldt<-1
	}
	if(ldt==1){	# have valid log dir, then... 
		print("3") #############################################
		## open connection to log file
		filen<-paste("FitSDMscriptTest",format(Sys.time(),"%Y%m%d-%H%M"),sep="_")
		logfile<-paste(logdir,filen,".log",sep="")
		zz <- try(file(logfile, "w"),silent=T)
		if(inherits(zz,"try-error")){
			print("Could not create log file. Please check access permissions or run test with appropriate credentials.", quote=FALSE)
			print("No tests performed; no logs generated.", quote=FALSE)
		}else{	#successful creating log file - start log
			print("Starting log file and tests...", quote=FALSE)
			## continue with tests....
			cat("Log report testing the SDM fitting script", paste("Started", format(Sys.time(),"%Y-%m-%d %H:%M:%S")), file = zz, sep = "\n", append=TRUE)
			cat("\n","\n",file = zz, append=TRUE)
			
			cat(paste("Valid git directory:",gitpath), file = zz, sep = "\n", append=TRUE)
			cat(paste("Found or created logs directory:",logdir), file = zz, sep = "\n", append=TRUE)
			cat("Testing validity of results directory:", file = zz, sep = "\n", append=TRUE)
			#test that the results dir is there or that can create in gitpath
			if(is.null(svpath) || !dir.exists(svpath)){	
				cat("   Missing or invalid directory where to store results.", file = zz, sep = "\n", append=TRUE)
				svpath<-paste0(gitpath,"results/")
				zz <- try(dir.create(svpath),silent=T)
				if(!inherits(zz,"try-error")){
					restest<-"SUCCESS"
				}else{
					restest<-"FAILED - WARNING!!!"
				}
				cat(paste0("   Testing that one can be created at ",gitpath,"results/ ...",restest), file = zz, sep = "\n", append=TRUE)
			}else{
				cat("   Found directory where to save results...", file = zz, sep = "\n\n", append=TRUE)
			}
			
			#test presence of all libraries needed
			cat("Testing that all needed libraries are installed and can be loaded", file = zz, sep = "\n", append=TRUE)
			libs<-c("rminer","raster","dismo","plyr","data.table","xgboost","doParallel","caret","kernlab");
			libtest<-as.data.frame(sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE));names(libtest)<-"installed"
			write.table(libtest, row.names = TRUE, col.names = FALSE, file=zz, append=TRUE)
			cat("\n","\n",file = zz, append=TRUE)
			
			#report the arguments passed in the test call
			cat("Arguments passed in script test call:", file = zz, sep = "\n", append=TRUE)
			optvals<-as.data.frame(opt)
			write.table(optvals, row.names = TRUE, col.names = FALSE, file=zz, append=TRUE)
			cat("\n","\n",file = zz, append=TRUE)
			
			#test the presence of the data files
			pth250<-paste0(gitpath,"sdmTool/data/Birds/250M/deflated_250M.RData")
			pth500<-paste0(gitpath,"sdmTool/data/Birds/500M/deflated_500M.RData")
			pth1000<-paste0(gitpath,"sdmTool/data/Birds/1000M/deflated_1000M.RData")
			if(!file.exists(pth250) || !file.exists(pth500) || !file.exists(pth1000)){
				cat("Testing presence of data files... WARNING: Some of the data files were not found", file = zz, sep = "\n", append=TRUE)
			}else{
				cat("Testing presence of data files... Found all the needed data files", file = zz, sep = "\n", append=TRUE)
			}
			
			#HERE determine if running the model fitting script
			#If so, source the sdmfit file and pre-compile the sdm fitting function
			
			#end the log
			cat("\n","End of test.","\n","\n",file=zz, append=TRUE)
			if(sinf){
				w<-unlist(sessionInfo())
				tdf<-data.frame(param=names(w),value=w);row.names(tdf)<-NULL
				cat("SessionInfo:",file=zz,sep="\n", append=TRUE)
				write.table(tdf, row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
			}
			close(zz)
			
			print(paste("Tests completed. Check file",logfile,"for results"), quote=FALSE)
			
		}
	}
	
}

