# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This script loads the GV data and creates matches against the no-pre-train models - sigmoid responses

## Need to get the GV results from pickles
## This file reads the pickles of predictions to GV files into data.frames
library(plyr); library(reticulate);library(data.table); library(ggplot2); library(dplyr)

## Need the utility matching functions
source("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/scripts/predMatching_utils.R")

## We start by getting the GV data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/gvData_20210518.RData")

## Add species code to the pickle data and filter by the selected model
speciesdf<-data.frame(SpeciesNum=1:54,SpeciesCode=c('STJA', 'EUCD', 'ACWO', 'GHOW', 'BRCR', 'COYE', 'BTPI', 'OSFL', 'AMCR', 'WBNU', 'HETH', 'CASJ', 'WITU', 'PIWO', 'LAZB', 'CAVI', 
				'BUOR', 'CALT', 'WETA', 'WEME', 'PAWR', 'RWBL', 'BHGR', 'HUVI', 'ATFL', 'MODO', 'WREN', 'NOFL', 'MOUQ', 'OCWA', 'SOSP', 'OATI', 'NUWO', 'PSFL', 'WCSP', 'WIWA', 'HOWR', 
				'CBCH', 'CAQU', 'DEJU', 'CORA', 'BEWR', 'GRSP', 'HOFI', 'RSHA', 'ANHU', 'BGGN', 'BTYW', 'SWTH', 'MAWR', 'WAVI', 'SPTO', 'AMRO', 'SAVS'))

gvedf<-subset(gvedf,!is.na(call)); nrow(gvedf)  
gvedf$spcall<-paste0(toupper(gvedf$species),"::",toupper(gvedf$call))
## Now we need the species and calls selected for training:
roidf<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_210510.csv", stringsAsFactors=FALSE)
roicalls<-unique(roidf[,c("birdcode","songtype")])
roicalls$spcall<-paste0(toupper(roicalls$birdcode),"::",toupper(roicalls$songtype))
gvedf<-subset(gvedf,spcall %in% roicalls$spcall)

gvedf<-subset(gvedf,species %in% speciesdf$SpeciesCode)
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)

##############################################################################################################################
# tabulate the pickle predictions
### DONE - did not save the code, but same as not pre-trained models

## load the BN GV predictions
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/BirdNet/data/BirdNET_GV_preds.RData")
bngvdf$PredictionId<-1:nrow(bngvdf)
names(bngvdf)<-gsub("second","Second",names(bngvdf))
names(bngvdf)<-gsub("birdcode","SpeciesCode",names(bngvdf))
names(bngvdf)<-gsub("event","SamplingEvent",names(bngvdf))
bngvdf$ModelName<-"BirdNET"
bngvdf$PredictionScore<-bngvdf$Probability*(10^7)
bngvdf$PredictionsDatasetId<-8

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)
dataSource="database"  ##ATTENTION!!! When reading pickles I convert to using the middle of the 2-second period, just like with the db
labelSource<-"GV"

checkNames(predsdf=bngvdf,labsdf=gvedf,labelSource=labelSource)

tm<-Sys.time()
bngvmatches<-getModelHurdledGVMatches(data=bngvdf,gvedf=gvedf,hurdvals=hurdvals,dataSource=dataSource,gvGetMatches=gvGetMatches,removeDoubleGVMatches=removeDoubleGVMatches)
Sys.time()-tm

bngvmatches$match<-as.character(bngvmatches$match)
unique(bngvmatches$match)
#all match NAs are FALSEPOS, matchDelta 9, so...
bngvmatches$match<-ifelse(is.na(bngvmatches$match),"FALSEPOS",bngvmatches$match)
bngvmatches$SpeciesCode<-as.character(bngvmatches$SpeciesCode)
bngvmatches$GVspeciesCode<-as.character(bngvmatches$GVspeciesCode)

## check
plotmatches<-as.data.frame(subset(bngvmatches,hurdle==0.65) %>% group_by(SpeciesCode) %>% 
				dplyr::summarize(numTP=sum(match=="TRUEPOS"),numFP=sum(match=="FALSEPOS"),numFN=sum(match=="FALSENEG")))

pdata<-reshape(plotmatches, idvar = "SpeciesCode", varying = list(2:4),
		v.names = "Count", direction = "long")
pdata$matchType<-ifelse(pdata$time==1,"True positive",ifelse(pdata$time==2,"False positive","False negative"))
row.names(pdata)<-NULL
dev.new()
ggplot(pdata,aes(x=SpeciesCode,y=Count)) + geom_bar(position="fill",stat="identity",aes(fill=matchType)) + coord_flip() + labs(x="Species",y="Precentage of predictions",title=" New method - Threshold: 0.65")

save(bngvmatches, file="c:/users/lsalas/git/S2L_devel/GVanalyses/BirdNet/data/BirdNET_GV_matches_06102021.RData")

