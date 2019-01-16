# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("rminer","raster","dismo","plyr","data.table","doParallel","xgboost","caret","kernlab","psych","compiler");
sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)

###
source("c:/users/lsalas/git/Soundscapes2Landscapes/sdmTool/scripts/fitSDMmodels_optimized.R")
fitCaseModelCmp <- try(cmpfun(fitCaseModel,options=list(suppressAll=TRUE)),silent=T)
cmpflag<-1
if(inherits(fitCaseModelCmp,"try-error")){
	#cat("Could not compile function: fitCaseModel. Please review the function.", file = zz, sep = "\n")
	print("Could not compile function: fitCaseModel. Please review the function.")
	cmpflag<-0
}else{
	#cat("Successfuly compiled function: fitCaseModel", file = zz, sep = "\n")
	print("Successfuly compiled function: fitCaseModel")
}
###


addGEDI<-c(TRUE)
gitpath<-"/home/ubuntu/Soundscapes2Landscapes/"
svpath<-paste0(gitpath,"results/")
logdir<-paste0(gitpath,"logs/")

X<-list(gitpath=gitpath,svpath=svpath,rez="500M",spp="ACWO",yrsp="3yr",gedi=addGEDI)
filen<-paste("FitoptimSDMBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
logfile<-paste(logdir,filen,".step",sep="")
zz <- try(file(logfile, "w"),silent=T)
if(inherits(zz,"try-error")){
	stop("Could not open log file")
}

if(cmpflag==1){
	res<-fitCaseModelCmp(X=X,logf=logf,ncores=NULL,percent.train=0.8,noise="noised")
}else{
	res<-fitCaseModel(X=X,logf=logf,ncores=NULL,percent.train=0.8,noise="noised")
}
close(zz)
print(res)

## step-in debug...
logf=zz;ncores=NULL;percent.train=0.8;noise="noised"
##


