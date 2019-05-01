# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file is a function that takes as input the path to a set of files with predicted values of competing models
## Each file has a table (named "predsSave") with as many rows as there are cells in the landscape being predicted
## It includes cellId and 14 columns, each representing the prediction under a different model in normal and logit spaces
## We use the logit spaces. The first column is cellId. (Total 15 columns)

## The function also requires a metric of model performance for each model + iteration (henceforth the model support)
## So, the function also requires a species code indicating the species to process, and it assumes that the file
## with the suport data will be named for example, ACWO_250M_2yr_withGEDI_VIF_Bal_i1_AUCsuppadj.RData. The table it 
## contains is called "suppadjSave". We assume that the model results data file name and the support file name both
## have the name of the species, spatial resolution, and a number for the boot, as in the example above.

## The support table has as many columns as the data file 
## (bootId instead of cellId, and the support for each of the 7 models)

## The function returns the weighted (by support) mean across all models and bootstraps, and 
## the Standard Error of the mean value, in the form of two rasters
## It also has the optional output of a weighted average raster for every boot - to create a video animation for it
## This option requires that you provide an empty raster of the right resolution, and 
## the folder "RasterSequence" must exist inside the datapth. See line 94 below for details, and alter as needed.

## First here is the function to back-transorm logits
# x is the value to logit-transform from
fromLogit<-function(x){
	bt<-exp(x)/(1+exp(x))
	return(bt)
}

# datapth is the path to a folder where the result RData files to process reside
# spcd is the species code - this is part of the filename of result files in datapth (per Pat's example)
# rez is the spatial resolution of results: 250M, 500M, or 1000M
# addViz: should the function output weighted average rasters for each bootstrap?
# emptyRast is an empty raster of the appropriate resolution, to be used if addViz = TRUE
summarizeOutputs<-function(datapth,spcd,rez,addViz=F,emptyRast=NA){
	# list the files in the directory for the species
	dfiles<-list.files(datapth,pattern=spcd)
	dfiles<-subset(dfiles,grepl(".RData",dfiles) & grepl(rez,dfiles))
	
	# find and load the support files
	sfiles<-subset(dfiles,grepl("suppadj",dfiles))
	
	# include only model results files in the loop below
	dfiles<-subset(dfiles,!grepl("suppadj",dfiles))
	
	# verify that the number of data files matches the number of support files
	numBoot<-NROW(sfiles)
	numBoot==NROW(dfiles)
	
	# populate first layer (first column) of stack with the cellId
	load(paste0(datapth,dfiles[1]))	#loads the table "predsSave" with results from the first bootstrap
	numCells<-nrow(predsSave)
	numModels<-(ncol(predsSave)-1)/2
	
	# dimensioning the stack: a matrix with ncol = numModels * numBoots, nrow = ncells
	stk<-matrix(rep(NA,times=(numCells*((numBoot*numModels)+1))),nrow=(numCells))
	
	# dimensioning the vector of supports too
	supvect<-rep(NA,times=numBoot*numModels)
		
	# make the first column be the cellIds
	stk[,1]<-predsSave$cellId
	
	for(z in 1:NROW(dfiles)){	
		# for each boot, this produces the weighted value for each model and cell
		# calculates the weighted mean and saves to raster (tiff)
		# and sticks the weighted values into stk
		dfn<-subset(dfiles,grepl(paste0("_i",z,"_"),dfiles))
		load(paste0(datapth,dfn))
		
		# If column 1 has the cellId, make a matrix out of the columns with predictions in logit space
		predsSave<-predsSave[,which(grepl("_lg",names(predsSave)))]
		dmx<-as.matrix(predsSave)
		dmx[is.na(dmx)]<-0	#make 0 those columns with NA's
		
		# Get vector of support for this bootstrap number 
		sfn<-subset(sfiles,grepl(paste0("_i",z,"_"),sfiles))
		load(paste0(datapth,sfn))
		
		# BIG assumption: the columns in predsSave are in the same order as the values in suppadjSave
		# checking:
		stval<-as.numeric(regexpr("_lg",names(predsSave[1])))+3
		pnm<-substr(names(predsSave),stval,stval+3); pnm<-gsub("knn_","pknn",pnm) #small inconsistent naming adjustment
		pnm<-paste0(spcd,"2yr_",pnm,"_i",z)
		if(!identical(pnm,names(suppadjSave))){stop("Order of supports does not match order of predicted value columns")}
		
		supp<-as.numeric(suppadjSave);supp[is.na(supp)]<-0
		
		#replace the values of stk by dmx, in the right places - same with supvect
		for(k in 1:numModels){
			q<-k+(numModels*(z-1))+1
			stk[,q]<-dmx[,k]
			supvect[q-1]<-supp[k]
		}
		#no need to free RAM here... 
		
		if(addViz==TRUE && class(emptyRast)=="RasterLayer"){
			require(raster)
			bootrst<-emptyRast
			bootrst[]<-(dmx %*% supp)/sum(supp)
			brname<-paste0(datapth,"RasterSequence/",spcd,"_",rez,"_i",z,".tif")
			writeRaster(bootrst,filename=brname,format="GTiff",overwrite=TRUE)
		}
	}
	
	
	#global mean is the sum of weighted values for each cell divided by the sum of weights
	sumWeights<-sum(supvect)
	glomean<-(stk[,-1] %*% supvect)/sumWeights
	gdf<-data.frame(cellId=stk[,1],lgmeanVal=fromLogit(glomean))
	
	#Calculate the global SE per cell
	denoVal<-sumWeights*((numBoot-1)/numBoot)
	#For each cell, SE is the square root of...
	#SUM(supvect x ((each value minus cell global mean)^2)) / denoVal 
	diffm<-apply(stk[,-1],2,FUN=function(x,glomean){
				y<-(x-glomean)^2
				return(y)
			},glomean=glomean)
	semx<-sqrt((diffm %*% supvect)/denoVal)
	
	gdf$stErr<-fromLogit(semx[,1])
	gdf$resolution<-rez
	
	return(gdf)
	
}

