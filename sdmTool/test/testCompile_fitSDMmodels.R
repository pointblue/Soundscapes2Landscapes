# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file tests the pre-compilation and run of fitSDMmodels::fitCaseModel

libs<-c("rminer","raster","dismo","plyr","data.table","xgboost","doParallel","caret","kernlab","psych","compiler");
sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)
####

### Add this to fitSDMbatch.R
source("c:/users/lsalas/git/Soundscapes2Landscapes/sdmTool/scripts/fitSDMmodels.R")
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

X<-list(gitpath="c:/users/lsalas/git/Soundscapes2Landscapes/",svpath="c:/temp/s2l/",rez="500M",spp="WESJ",yrsp="3yr",gedi=TRUE)
logf<-file("c:/temp/s2l/logsdm.log", "w")

### Add this to fitSDMbatch.R
percent.train=0.8
noise="noised"

if(cmpflag==1){
	res<-fitCaseModelCmp(X=X,logf=logf,ncores=NULL,percent.train=0.8,noise="noised")
}else{
	res<-fitCaseModel(X=X,logf=logf,ncores=NULL,percent.train=0.8,noise="noised")
}
###
print(res)

close(logf)
