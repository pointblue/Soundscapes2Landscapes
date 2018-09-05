# TODO: Add comment
# 
# Author: lsalas
###############################################################################


suppressPackageStartupMessages(require(optparse))

option_list = list(
		make_option(c("-p", "--path"), action="store", default="/home/ubuntu/s2l/sdmTool", type='character', help="path to the tool directory"),
		make_option(c("-s", "--session"), action="store_true", default=FALSE, type='logical', help="include sessionInfo() in the log?")
)
opt = parse_args(OptionParser(option_list=option_list))

pth<-opt$p;if(substr(pth,nchar(pth),nchar(pth))=="/"){pth<-substr(pth,1,nchar(pth)-1)}

## check that the logs folder exists, if not then create
dlst<-list.dirs(pth);dc<-1
if(TRUE %in% grepl("/logs",dlst)){
	lgpth<-paste(pth,"/logs/",sep="")
}else{
	q<-try(dir.create(paste(pth,"logs",sep="")),silent=T)
	if(inherits(q,"try-error")){
		print(paste("Logs directory",paste(pth,"/logs",sep=""),"does not exist and this application could not create it. Please check access permissions or run test with appropriate credentials. \n", quote=FALSE))
		print("No tests performed; no logs generated.", quote=FALSE)
		dc<-0
	}else{
		lgpth<-paste(pth,"/logs/",sep="")
	}
}

if(dc==1){
	## open connection to log file
	filen<-paste("sdmToolTest",format(Sys.time(),"%Y%m%d-%H%M"),sep="_")
	logfile<-paste(lgpth,filen,".log",sep="")
	zz <- try(file(logfile, "w"),silent=T)
	if(inherits(zz,"try-error")){
		print("Could not create log file. Please check access permissions or run test with appropriate credentials.", quote=FALSE)
		print("No tests performed; no logs generated.", quote=FALSE)
	}else{
		## continue with tests....
		cat("Log report testing the S2L sdm tool", paste("Started", format(Sys.time(),"%Y-%m-%d %H:%M:%S")), file = zz, sep = "\n")
		cat("\n","\n",file = zz)
		
		## set flag for minimum requirements
		minrec<-1
		
		## test that the libraries needed are installed and can be loaded
		cat("Testing load of required libraries... \n",file = zz)
		libs<-c("rminer","raster","dismo","plyr","data.table","yaml")
		lt<-unlist(lapply(libs, require, character.only = TRUE,quietly=TRUE))
		dflibs<-data.frame(library=libs,test=lt);dflibs$result<-ifelse(dflibs$test==TRUE,"Loaded","Not loaded")
		if(FALSE %in% lt){
			cat("Some required libraries could not be loaded.", file = zz, sep = "\n")
			write.table(dflibs[,c(1,3)], row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
			minrec<-0
		}else{
			cat("\n","\n",file = zz)
			
			## report the assumed file location
			cat("Data files and scripting code assumed to be", 
					"in the provided (or default) location for the tool:", file = zz, sep = "\n")
			cat(pth, file=zz, sep = "\n")
			cat("\n","\n",file = zz)
			
			## tests that the data files exist
			cat("Checking for the presence of the BCM data... \n",file = zz)
			if(file.exists(paste(pth,"data/BCM/1000M/aet",sep="/"))){
				cfn<-list.files(paste(pth,"data/BCM/1000M/aet",sep="/"))
				#... do the same for the other directories under BCM - should all have 60 files each
			}else{
				cat("Some BCM data files may be missing.", file = zz, sep = "\n")
				#report which ones don't add up
				cat(pth, file=zz)
				minrec<-0
			}
			cat("\n","\n",file = zz)
			#test the next data directories
			
		}
		
		# test loading the bird data

		#then test reading the yaml...

		#Done
	}
}