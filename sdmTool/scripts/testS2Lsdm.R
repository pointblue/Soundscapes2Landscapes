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

## Need this dir contents check function...
checkDirContents<-function(pth,varn,filt=NA,check){
	if(file.exists(paste(pth,"/data/",varn,sep=""))){
		dircont<-list.files(paste(pth,"/data/",varn,sep=""))
		if(!is.na(filt)){
			dircont<-subset(dircont,!grepl(".tif.",dircont))
		}else{
			if(NROW(dircont)==check){
				tmpdf<-data.frame(variable=varn,status="OK")
			}else{
				tmpdf<-data.frame(variable=varn,status="Missing some files")
			}
		}
		
	}else{
		tmpdf<-data.frame(variable=varn,status="Directory not found")
	}
	return(tmpdf)
}

## making sure the path does not end in a forward slash /
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
		options(warn=-1)
		cat("Testing load of required libraries... \n",file = zz)
		libs<-c("rminer","raster","dismo","plyr","data.table","yaml","data.table")
		lt<-unlist(lapply(libs, require, character.only = TRUE,quietly=TRUE))
		dflibs<-data.frame(library=libs,test=lt);dflibs$result<-ifelse(dflibs$test==TRUE,"Loaded","Not loaded")
		options(warn=0)
		if(FALSE %in% lt){
			cat("Some required libraries could not be loaded.", file = zz, sep = "\n")
			write.table(dflibs[,c(1,3)], row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
			minrec<-0
		}else{
			cat("All required libraries successfully loaded.", file = zz, sep = "\n")
			cat("\n","\n",file = zz)
			
			## report the assumed file location
			cat("Data files and scripting code assumed to be", 
					"in the provided (or default) location for the tool:", file = zz, sep = "\n")
			cat(pth, file=zz, sep = "\n")
			cat("\n","\n",file = zz)
			
			## tests that the data files exist
			## BCM
			cat("Checking for the presence of the covariate data... \n",file = zz)
			varnams<-c("aet","cwd","pet","ppt","tmn","tmx")
			chkdf<-data.frame()
			for(dd in c("1000M","500M","250M")){
				for(vv in varnams){
					tmpdf<-checkDirContents(pth=pth,varn=paste("BCM",dd,vv,sep="/"),filt=NA,check=60)
					chkdf<-rbind(chkdf,tmpdf)
				}
			}
			## Coast_Distance
			for(dd in c("1000M","500M","250M")){
				tmpdf<-checkDirContents(pth=pth,varn=paste("Coast_Distance",dd,sep="/"),filt=".tif.",check=1)
				chkdf<-rbind(chkdf,tmpdf)
			}
			## DEM_Rescaled
			for(dd in c("1000M","500M","250M")){
				tmpdf<-checkDirContents(pth=pth,varn=paste("DEM_Rescaled",dd,sep="/"),filt=NA,check=1)
				chkdf<-rbind(chkdf,tmpdf)
			}
			## DHI_MODIS
			for(dd in c("1000M","500M","250M")){
				tmpdf<-checkDirContents(pth=pth,varn=paste("DHI_MODIS",dd,sep="/"),filt=NA,check=7)
				chkdf<-rbind(chkdf,tmpdf)
			}
			## Stream_Distance
			for(dd in c("1000M","500M","250M")){
				tmpdf<-checkDirContents(pth=pth,varn=paste("Stream_Distance",dd,sep="/"),filt=".tif.",check=1)
				chkdf<-rbind(chkdf,tmpdf)
			}
			
			## Street_Distance
			for(dd in c("1000M","500M","250M")){
				tmpdf<-checkDirContents(pth=pth,varn=paste("Street_Distance",dd,sep="/"),filt=".tif.",check=1)
				chkdf<-rbind(chkdf,tmpdf)
			}
			write.table(chkdf, row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
			cat("\n","\n",file = zz)
			
			# test loading the bird data
			## HERE!!!
			
			#then test reading a yaml...
			ydpth<-paste(pth,"/requests/readme.yaml",sep="")
			yfl<-yaml.load_file(ydpth)
			if(identical(names(yfl),c("species","resolution","noise","scale","GEDIyears"))){
				cat("Able to read and understand the example readme.yaml file", file = zz, sep = "\n")
			}else{
				cat("Error: failed to read or understand the example readme.yaml file. Has it been deleted or altered?", file = zz, sep = "\n")
			}
			cat("\n","\n",file = zz)
			#Done
			
		}
		
		cat("\n","End of log.","\n","\n",file=zz)
		if(opt$s){
			w<-unlist(sessionInfo())
			tdf<-data.frame(param=names(w),value=w);row.names(tdf)<-NULL
			cat("SessionInfo:",file=zz,sep="\n")
			write.table(tdf, row.names = FALSE, col.names = FALSE, file=zz, append=TRUE)
		}
		
		close(zz)
		
		print(paste("Tests completed. Check file",logfile,"for results"), quote=FALSE)
	}
}