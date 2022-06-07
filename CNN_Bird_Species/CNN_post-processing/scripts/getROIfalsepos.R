# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This script collects the FP from "not present" ROI labels
## The follow up is to hurdle these FPs and add to the pretrain and nopretrain model results, standardizing the rates before calculating metrics

## libraries
libs<-c("ggplot2","plyr","data.table","dplyr")
lapply(libs, require, character.only = TRUE)

#pathToLocalGit<-"C:/Users/salasle/CNNpaper/S2Ldevel/"
pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"

## Need the utility matching functions
source(paste0(pathToLocalGit,"GVanalyses/3models2outputs/scripts/predMatching_utils.R"))


## need the label data
roidf<-read.csv(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_210510.csv"), stringsAsFactors=FALSE)
roidata<-subset(roidf,vote=="not present")
roidata$roicenter<-roidata$x1+((roidata$x2-roidata$x1)/2)
roidata$event<-sapply(roidata$filename,function(x){substr(x,1,32)})
roidata<-roidata[,c("event","birdcode","site","device","year","month","hour","songtype","vote","type","method","roicenter","x1","x2")]
names(roidata)<-gsub("type","assessType",names(roidata))
roidata$roiId<-1:nrow(roidata)
events<-unique(roidata$event)

## Load the predictions
load(file=paste0(pathToLocalGit,"GVanalyses/data/modelsDataAll/allROI_pretrainedPredictions.RData"))
roipreds<-rbindlist(labelPredsList)
roipreds<-subset(roipreds,SamplingEvent %in% events)

dataSource<-"database"

## For each model, find the FP matches
fpMatchesList<-getROIfalsepos(allnproidata=roidata,roipreds=roipreds,dataSource=dataSource,getROIfpMatch=getROIfpMatch,removeDoubleFP=removeDoubleFP)

## Save the results and later add to the roi analyses on the test set
save(fpMatchesList,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/FPmatches_list_preTrained_06142021.RData"))

#########################################################
## Now try the not-pretrained model data
## Load the predictions
load(file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/untrainedPredictions_ROI.RData"))
speciesdf<-data.frame(SpeciesNum=1:54,SpeciesCode=c('STJA', 'EUCD', 'ACWO', 'GHOW', 'BRCR', 'COYE', 'BTPI', 'OSFL', 'AMCR', 'WBNU', 'HETH', 'CASJ', 'WITU', 'PIWO', 'LAZB', 'CAVI', 
				'BUOR', 'CALT', 'WETA', 'WEME', 'PAWR', 'RWBL', 'BHGR', 'HUVI', 'ATFL', 'MODO', 'WREN', 'NOFL', 'MOUQ', 'OCWA', 'SOSP', 'OATI', 'NUWO', 'PSFL', 'WCSP', 'WIWA', 'HOWR', 
				'CBCH', 'CAQU', 'DEJU', 'CORA', 'BEWR', 'GRSP', 'HOFI', 'RSHA', 'ANHU', 'BGGN', 'BTYW', 'SWTH', 'MAWR', 'WAVI', 'SPTO', 'AMRO', 'SAVS'))

roipredlist<-llply(1:3,function(nn,roidflist,speciesdf){
			tdf<-roidflist[[nn]]
			tdf<-subset(tdf,SpeciesNum>0)
			names(tdf)<-gsub("Score","PredictionScore",names(tdf))
			tdf$SamplingEvent<-substr(tdf$RegistryName,1,32)
			tdf<-merge(tdf,speciesdf,by="SpeciesNum",all.x=TRUE)
			tdf$ModelName<-ifelse(grepl("mobnet",tdf$PredictionsDatasetId[1]),"MobileNet::sigmoid",ifelse(grepl("ResNet50",tdf$PredictionsDatasetId[1]),"Resnet50::sigmoid","Resnet101::sigmoid"))
			tdf$PredictionsDatasetId<-ifelse(tdf$ModelName=="MobileNet::sigmoid",6,ifelse(tdf$ModelName=="Resnet50::sigmoid",2,4))
			tdf$PredictionId<-1:nrow(tdf)
			return(tdf)
		},roidflist=roidflist,speciesdf=speciesdf)

roipreds<-rbindlist(roipredlist)
roipreds<-subset(roipreds,SamplingEvent %in% events)

dataSource<-"database"

## For each model, find the FP matches
fpMatchesList<-getROIfalsepos(allnproidata=roidata,roipreds=roipreds,dataSource=dataSource,getROIfpMatch=getROIfpMatch,removeDoubleFP=removeDoubleFP)

## Save the results and later add to the roi analyses on the test set
save(fpMatchesList,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/FPmatches_list_not_preTrained_06142021.RData"))


