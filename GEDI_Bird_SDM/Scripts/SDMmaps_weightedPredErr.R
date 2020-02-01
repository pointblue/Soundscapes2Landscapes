# Title: SDMmaps_weightedPredErr.R
# Author: Leo Salas [lsalas@pointblue.org] & Patrick Burns [pb463@nau.edu]
# Purpose: takes all bootstrap results and combines them together into a single prediction map based on weighted average AUC. Error of all bootstraps is also output in a separate map. See Detailed Description section below
# TODO: 
###############################################################################


####
#### Libraries 
####

library(raster)
library(sp)
library(rasterVis)
library(tidyverse)



####
#### 0. Functions
####

## Detailed description of summarizeOutputs function (below)
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
  dfiles<-list.files(datapth,pattern=paste0(spcd,".*\\.RData"))
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
      colmean<-
        stk[,q]<-dmx[,k]
      supvect[q-1]<-supp[k]
    }
    #no need to free RAM here... 
    
    if(addViz==TRUE && class(emptyRast)=="RasterLayer"){
      bootrst<-emptyRast
      bootrst[]<-(dmx %*% supp)/sum(supp)
      brname<-paste0(datapth,"RasterSequence/",spcd,"_",rez,"_i",z,".tif")
      writeRaster(bootrst,filename=brname,format="GTiff",overwrite=TRUE)
    }
  }
  
  print(paste0("Supp. vector has ", length(supvect), " values"))
  print(paste0("Individual model columns: ", ncol(stk[,-1])))
  
  #global mean is the sum of weighted values for each cell divided by the sum of weights
  sumWeights<-sum(supvect)
  glomean<-(stk[,-1] %*% supvect)/sumWeights
  gdf<-data.frame(cellId=stk[,1], meanValLg = glomean, meanVal=fromLogit(glomean))
  
  #Calculate the global SE per cell
  #need number of non-zero weights
  numNZW<-sum(supvect>0)
  denoVal<-sumWeights*((numNZW-1)/numNZW)
  #For each cell, SE is the square root of...
  #SUM(supvect x ((each value minus cell global mean)^2)) / denoVal 
  # refs: https://www.itl.nist.gov/div898/software/dataplot/refman2/ch2/weightsd.pdf
  #       https://stats.stackexchange.com/questions/6534/how-do-i-calculate-a-weighted-standard-deviation-in-excel
  diffm<-apply(stk[,-1],2,FUN=function(x,glomean){
    y<-(x-glomean)^2
    return(y)
  },glomean=glomean)
  semx<-sqrt((diffm %*% supvect)/(denoVal))
  
  gdf$stErrLg<-semx[,1]
  gdf$stErr<-fromLogit(semx[,1])
  gdf$min2SE<-fromLogit(gdf$meanValLg - 2*gdf$stErrLg)
  gdf$min1SE<-fromLogit(gdf$meanValLg - gdf$stErrLg)
  gdf$plus1SE<-fromLogit(gdf$meanValLg + gdf$stErrLg)
  gdf$plus2SE<-fromLogit(gdf$meanValLg + 2*gdf$stErrLg)
  gdf$delta1SE<-gdf$plus1SE - gdf$min1SE
  gdf$delta2SE<-gdf$plus2SE - gdf$min2SE
  gdf$p95minp5<-gdf$plus2SE-gdf$min2SE
  
  
  gdf$studStErr<-abs(gdf$stErr-gdf$meanVal)/gdf$meanVal
  gdf$binStErr<-cut(x=gdf$studStErr,breaks=seq(0,2,0.2),labels=as.character(1:10),right=F)
  gdf$binStErr<-ifelse(is.na(gdf$binStErr),"11",gdf$binStErr)
  gdf$resolution<-rez
  
  return(gdf)
  
}



####
#### 1. Inputs
####

# Produce prediction and error maps. Loop through each resolution and species
# Specify the data paths
pathToGit = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/repos/Soundscapes2Landscapes/"

svPath = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/results/s20190809/plots/predRas2/"

datapthBase = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/results/s20190809/All_wGEDI/"

specRun = c("ACWO", "AMGO", "BEWR", "BHGR", "BLPH", "BRBL", "BUSH",
            "CALT", "CAQU", "CBCH", "DEJU", "HOFI", "LEGO", "MODO",
            "NOFL", "NOMO", "NUWO", "OATI", "RWBL", "SOSP", "SPTO",
            "STJA", "WCSP", "WEBL", "WESJ")

rezs = c("250M", "500M", "1000M")



####
#### 2. Processing
####

# Make sure directory structure exists
if (!dir.exists(svPath)){
  dir.create(svPath)
  print("Created directory for output plots")
} else {
  print("Directory for output plots already exists")
}

svPath_GTiff = paste0(svPath, 'GeoTIFF/')
if (!dir.exists(svPath_GTiff)){
  dir.create(svPath_GTiff)
  print("Created directory for output GeoTIFF plots")
} else {
  print("Directory for output GeoTIFF plots already exists")
}

svPath_PNG = paste0(svPath, 'PNG/')
if (!dir.exists(svPath_PNG)){
  dir.create(svPath_PNG)
  print("Created directory for output PNG plots")
} else {
  print("Directory for output PNG plots already exists")
}

# Load in bird observation data
load(paste0(pathToGit, "sdmTool/data/Birds/250M/deflated_250M.RData"))
PA_250 = deflatedcovardf
load(paste0(pathToGit, "sdmTool/data/Birds/500M/deflated_500M.RData"))
PA_500 = deflatedcovardf
load(paste0(pathToGit, "sdmTool/data/Birds/1000M/deflated_1000M.RData"))
PA_1000 = deflatedcovardf


# Calculate weighted prediction probability and uncertainty and save maps
for (spcd in specRun){
  
  for (rez in rezs){
    print(paste0("Working on ", spcd, " at ", rez, " resolution"))
    basegrid<-raster(paste0(pathToGit,"sdmTool/data/CoastDistance/",rez,"/CoastDistance_",tolower(rez),".tif"))
    basegrid[]<-NA
    
    WA = summarizeOutputs(datapth = paste0(datapthBase,"/",rez,"/"), spcd = spcd, rez = rez)
    
    meanRas<-basegrid
    SERas<-basegrid
    SERas_bin<-basegrid
    cid<-WA$cellId
    meanvals<-as.numeric(WA$meanVal)
    SEvals<-as.numeric(WA$delta1SE)
    #SEvals_bin<-as.numeric(WA$binStErr)
    meanRas[cid]<-meanvals
    SERas[cid]<-SEvals
    #SERas_bin[cid]<-SEvals_bin
    
    # Also bring in presence and absence by species
    if(rez == "250M"){
      PAdf = PA_250
    } else if(rez == "500M"){
      PAdf = PA_500
    } else if(rez == "1000M"){
      PAdf = PA_1000 
    } else{
      print("Error with rez")
    }
    
    colsToKeep = c(paste0("gId",rez), "x", "y", spcd)
    PAdf_filt = PAdf %>% dplyr::select(one_of(colsToKeep)) %>%
                dplyr::filter(UQ(as.symbol(spcd)) >= 0)
    PAdf_filt_sp = sp::SpatialPointsDataFrame(coords = PAdf_filt[,c("x", "y")], data = PAdf_filt, proj4string = crs(basegrid))
    xy <- data.frame(coordinates(PAdf_filt[,c("x","y")]), z=PAdf_filt[,4])
    coordinates(xy) = ~x+y
    crs(xy) <- projection(meanRas)
    
    # Save the predictions as PNG
    cat("Saving weighted mean prediction map \n")
    png(paste0(svPath, "PNG/", spcd,"_", rez, "_WAPredMap_wLegend.png"), width = 4, height = 4, units = "in", res = 300, bg = "transparent")
    
    colr <- colorRampPalette(rev(brewer.pal(9, 'YlGnBu')))
    
    p=levelplot(meanRas, 
                margin=FALSE,                       # suppress marginal graphics
                colorkey=FALSE, #list(space='right', labels=list(at=0:1, font=16)),   
                par.settings=list(
                  axis.line=list(col='transparent') # suppress axes and legend outline
                ),
                scales=list(draw=FALSE),            # suppress axis labels
                col.regions=colr,                   # colour ramp
                at=seq(0, 1, len=101))+            # colour ramp breaks
                latticeExtra::layer(sp.points(xy, cex=0.7, pch = ifelse(xy$z > 0, 3, 1), col = ifelse(xy$z >0, "#FFAD29", "black")))
    print(p)
    
    dev.off()
    
    # also as raster
    writeRaster(meanRas,filename=paste0(svPath, "GeoTIFF/", spcd,"_", rez, "_WAPredMap.tif"),format="GTiff",overwrite=T)
    
    # Plot the binned prediction SE as PNG
    cat("Saving Standard Error prediction map \n")
    png(paste0(svPath, "PNG/", spcd,"_", rez, "_SEPredMap.png"), width = 10, height = 10, units = "in", res = 300, bg = "transparent")
    
    colr <- colorRampPalette(rev(brewer.pal(11, 'PuOr')))
    
    q=levelplot(SERas, 
                margin=FALSE,                       # suppress marginal graphics
                colorkey=FALSE, #list(space='right', labels=list(at=0:1, font=16)),   
                par.settings=list(
                  axis.line=list(col='transparent') # suppress axes and legend outline
                ),
                scales=list(draw=FALSE),            # suppress axis labels
                col.regions=colr,                   # colour ramp
                at=seq(0, 1, len=101))            # colour ramp breaks
    print(q)
    
    dev.off()
    #also as raster
    writeRaster(SERas,filename=paste0(svPath, "GeoTIFF/", spcd,"_", rez, "_SEPredMap.tif"),format="GTiff",overwrite=T)
    
  }
  
}

