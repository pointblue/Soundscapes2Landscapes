# PURPOSE:
# Organizes data and provide analysis functions for the paper:
# "Classification of Bird Vocalizations in Soundscapes from an Urban-wildland Gradient for Applied Biodiversity Monitoring with Citizen Scientists"

# AUTHOR:
# Dr. Leo Salas
# EcoInformatics and Climate Solution
# Point Blue Conservation Science
# lsalas@pointblue.org

# Version: January 28, 2023; R 4.2.2

# Point to your local cloned repository
pathToLocalGit<-"[insert local path here]/" 

suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

#########################################################################################
####    READ ME !!!!

#### Code to process the data for result visualizations

## Below is the code that generates all the data used in the script generatePlots.R

###########################
# Here we provide the code and data resources to reproduce the input data file used for our results. Specifically...
# The outputs of this code file (i.e., the output of every "save" statemebt below) are provided and used with the generatePlots.R code file, which reproduces all the graphics and tables in the paper.
# The code is included here for completeness but the resulting data file is provided in this repository. So, it is not necessary to re-run it. (Takes time!)

# We start with the AI models predictions, for the sake of completeness
# However, the database of predictions and some of the code files to summarize the predictions are not included in this repository 
# We mention where each data file comes from, and if we do not provide the code file, we name it and can provide a copy upon request.

# We need the original pretrained and not pretrained predictions from each model to test against the soundscape data 
# The pre-trained predictions are in "predictions_sigmoid_20221216.csv" - Please ask for a copy of this ~2Gb data file until we are ready to post a copy in an open repository 
# The not-XC-pretrained predictions are obtained with the code file noXCmodels_results.R - we are happy to share a copy of this file upon request, along with the pickle files of predictions it uses
# We need the ROI performance data, the latest ROI centers file - available through this repository
# We also need the BirdNet predictions - available through this repository

#############
# Load the ROI data - need this first to filter the GV data by training call...
roidf<-read.csv(paste0(pathToLocalGit,"S2L_devel/GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_230109.csv"), stringsAsFactors=FALSE)
roidf$SamplingEvent<-substr(roidf$filename,1,32)
roidf<-roidf[,c("site","birdcode","songtype","vote","type","method","roicenterround","SamplingEvent")]
names(roidf)<-c("SiteName","SpeciesCode","songtype","vote","type","method","second","SamplingEvent")
# For now assuming we used all presents, since this is a test of accuracy
roidf<-subset(roidf,vote=="present")

#############
# Prepare the GV data
load(paste0(pathToLocalGit,"S2L_devel/GVanalyses/data/gvData.RData"))
# First, for testing BirdNET, keep an unfiltered copy
gvedf_bn<-gvedf
# Then filter correctly, knowing which call is the correct for each species...
gvedf<-subset(gvedf,!is.na(call)) 
gvedf$spcall<-paste0(toupper(gvedf$species),"::",toupper(gvedf$call))
# Now we need the species and calls selected for training:
roicalls<-unique(roidf[,c("SpeciesCode","songtype")])
roicalls$spcall<-paste0(toupper(roicalls$SpeciesCode),"::",toupper(roicalls$songtype))
gvedf<-subset(gvedf,spcall %in% roicalls$spcall)
# Add SamplingEvent to gvedf
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)
gvedf_bn$SamplingEvent<-substr(gvedf_bn$RegistryName,1,32)

# Then get the species with 40 or more GV records
gvsp<-ldply(unique(gvedf$species),function(ss,gvedf){
			spc<-nrow(subset(gvedf,species==ss))
			tdf<-data.frame(species=ss,GVcount=spc)
			return(tdf)
		},gvedf=gvedf)
gvsp<-subset(gvsp, GVcount>=40)
# finally subset the GV data for the 37 testable species
gvedf<-subset(gvedf, species %in% gvsp$species)
gvedf_bn<-subset(gvedf_bn, species %in% gvsp$species)

## need the AudiofileId data
afiles<-read.csv(paste0(pathToLocalGit,"data/audiofiles_20221216.csv"),stringsAsFactors=F)
afiles<-afiles[,-1]
afiles$chrdt<-as.character(as.POSIXct(paste0("20",afiles$Year,"-",afiles$Month,"-",afiles$Day," ",afiles$Hour,":",afiles$Minute),format="%Y-%m-%d %H:%M"))
afiles$SamplingEvent<-paste0(afiles$SiteName,"_",afiles$chrdt)
afiles$SamplingEvent<-substr(afiles$SamplingEvent,1,32)
afiles$SamplingEvent<-gsub(" ","_",afiles$SamplingEvent)
afiles$SamplingEvent<-gsub(":","-",afiles$SamplingEvent)
afiles<-afiles[,c("AudiofileId","SiteName","Year","Month","Day","Hour","Minute","SamplingEvent")]
#
## adding AudiofileId to both the GV and ROI datasets
gvedf<-merge(gvedf,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
gvedf<-subset(gvedf,!is.na(AudiofileId))
gvedf_bn<-merge(gvedf_bn,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
gvedf_bn<-subset(gvedf_bn,!is.na(AudiofileId))

# roiTestdf is needed to find optimal thresholds
roiTestdf<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_201109_testing.csv"), stringsAsFactors=FALSE)
roiTestdf$SamplingEvent<-substr(roiTestdf$filename,1,32)
roiTestdf<-merge(roiTestdf,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
roiTestdf<-subset(roiTestdf,!is.na(AudiofileId))
# Need the absent data for porper model evaluation to find FP
roiAbs_nptdf<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_230109_absent_testing.csv"), stringsAsFactors=FALSE)
roiAbs_nptdf$SamplingEvent<-substr(roiAbs_nptdf$filename,1,32)
roiAbs_nptdf<-merge(roiAbs_nptdf,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
roiAbs_nptdf<-subset(roiAbs_nptdf,!is.na(AudiofileId))
roiTestdf<-rbind(roiTestdf,roiAbs_nptdf)

################
# Preparing the pre-trained model predictions
# Run this once

roiAF<-unique(roiTestdf$AudiofileId)
gveAF<-unique(gvedf$AudiofileId)

preds<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/predictions_sigmoid_20221216.csv",stringsAsFactors=F)
preds<-merge(preds,afiles[,c("AudiofileId","SamplingEvent")],by="AudiofileId",all.x=T)
preds<-preds[,c("PredictionId","AudiofileId","PredictionsDatasetId","Second","SpeciesCode","PredictionScore","SamplingEvent")]
# We subset for the ROI and GV events, and the proper AudiofileId

predROI_PTdf<-subset(preds,AudiofileId %in% roiAF)
predROI_PTdf$ModelName<-ifelse(predROI_PTdf$PredictionsDatasetId %in% c(2,7),"ResNet50v2",
		ifelse(predROI_PTdf$PredictionsDatasetId %in% c(4,8),"ResNet101v2","MobileNetv2"))

predGV_PTdf<-subset(preds,AudiofileId %in% gveAF)
predGV_PTdf$ModelName<-ifelse(predGV_PTdf$PredictionsDatasetId %in% c(2,7),"ResNet50v2",
		ifelse(predGV_PTdf$PredictionsDatasetId %in% c(4,8),"ResNet101v2","MobileNetv2"))

save(predROI_PTdf,predGV_PTdf,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/pretrained_ROI_GV_predictions_20221225.RData") 

# Preparing the not-pretrained model predictions
# Run this once

speciesdf<-data.frame(SpeciesNum=1:54,SpeciesCode=c('STJA', 'EUCD', 'ACWO', 'GHOW', 'BRCR', 'COYE', 'BTPI', 'OSFL', 'AMCR', 'WBNU', 'HETH', 'CASJ', 'WITU', 'PIWO', 'LAZB', 'CAVI', 
				'BUOR', 'CALT', 'WETA', 'WEME', 'PAWR', 'RWBL', 'BHGR', 'HUVI', 'ATFL', 'MODO', 'WREN', 'NOFL', 'MOUQ', 'OCWA', 'SOSP', 'OATI', 'NUWO', 'PSFL', 'WCSP', 'WIWA', 'HOWR', 
				'CBCH', 'CAQU', 'DEJU', 'CORA', 'BEWR', 'GRSP', 'HOFI', 'RSHA', 'ANHU', 'BGGN', 'BTYW', 'SWTH', 'MAWR', 'WAVI', 'SPTO', 'AMRO', 'SAVS'))


load(paste0(pathToLocalGit,"S2L_devel/GVanalyses/3models2outputs/data/untrainedPredictions_gv_roi.RData"))
predGV_NPTdf<-rbindlist(gvdflist)
predGV_NPTdf<-merge(predGV_NPTdf,speciesdf,by="SpeciesNum")
predGV_NPTdf$PredictionId<-c(1:nrow(predGV_NPTdf)) #A fake predictionID because these records were never databased
predGV_NPTdf$SamplingEvent<-substr(predGV_NPTdf$RegistryName,1,32)
predGV_NPTdf$ModelName<-ifelse(grepl("ResNet50",predGV_NPTdf$PredictionsDatasetId),"ResNet50v2",
		ifelse(grepl("ResNet101",predGV_NPTdf$PredictionsDatasetId),"ResNet101v2","MobileNetv2"))
predGV_NPTdf<-merge(predGV_NPTdf[,c("PredictionId","PredictionsDatasetId","Second","Score","SpeciesCode","SamplingEvent","ModelName")],afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=TRUE)
names(predGV_NPTdf)<-gsub("Score","PredictionScore",names(predGV_NPTdf))
predGV_NPTdf<-predGV_NPTdf[,c("PredictionId","AudiofileId","PredictionsDatasetId","Second","SpeciesCode","PredictionScore","ModelName","SamplingEvent")]

predROI_NPTdf<-rbindlist(roidflist)
predROI_NPTdf<-merge(predROI_NPTdf,speciesdf,by="SpeciesNum")
predROI_NPTdf$PredictionId<-c(1:nrow(predROI_NPTdf)) #A fake predictionID because these records were never databased
predROI_NPTdf$SamplingEvent<-substr(predROI_NPTdf$RegistryName,1,32)
predROI_NPTdf$ModelName<-ifelse(grepl("ResNet50",predROI_NPTdf$PredictionsDatasetId),"ResNet50v2",
		ifelse(grepl("ResNet101",predROI_NPTdf$PredictionsDatasetId),"ResNet101v2","MobileNetv2"))
predROI_NPTdf<-merge(predROI_NPTdf[,c("PredictionId","PredictionsDatasetId","Second","Score","SpeciesCode","SamplingEvent","ModelName")],afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=TRUE)
names(predROI_NPTdf)<-gsub("Score","PredictionScore",names(predROI_NPTdf))
predROI_NPTdf<-predROI_NPTdf[,c("PredictionId","AudiofileId","PredictionsDatasetId","Second","SpeciesCode","PredictionScore","ModelName","SamplingEvent")]
predROI_NPTdf<-subset(predROI_NPTdf,AudiofileId %in% roiAF)

save(predROI_NPTdf,predGV_NPTdf,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/non_pretrained_ROI_GV_predictions_20221225.RData")
################

# Load the pretrained model predictions...
load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/pretrained_ROI_GV_predictions_20221225.RData")
# Load the non-pretrained model predictions...
load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/non_pretrained_ROI_GV_predictions_20221225.RData")

# Load the BirdNET data
bndf<-read.csv(paste0(pathToLocalGit,"Soundscapes2Landscapes/CNN_Bird_Species/CNN_post-processing/data/gv_birdnet_analyzer_audacity_2sec_overlap_geographic.csv"), stringsAsFactors=FALSE)
bndf<-subset(bndf,!is.na(birdcode))
bndf$SamplingEvent<-substr(bndf$File,1,32)
# BirdNET is trained on 3-second clips, we use 2 seconds. So, we expand the BirdNET predictions into two 2-second segments.
# For example, if BirdNET reports a detection for species X in the time interval starting on second 20 and ending on second 23, we will split the prediction into two: centered at second 21 and at second 22.
bngvdf<-ldply(1:nrow(bndf),function(rr,bndf){
			rdf<-bndf[rr,]
			ssec<-rdf$StartSecond
			if(ssec<58){
				rdf<-rbind(rdf,rdf)
				rdf$Second<-c(ssec+1,ssec+2)
			}else{
				rdf$Second<-ssec+1
			}
			return(rdf)
		}, bndf=bndf)
names(bngvdf)<-gsub("birdcode","SpeciesCode",names(bngvdf))
bngvdf$PredictionScore<-bngvdf$Probability*10^7
bngvdf$ModelName<-"BirdNET"
bngvdf$PredictionsDatasetId<-10
bngvdf$PredictionId<-c(1:nrow(bngvdf))
bngvdf<-merge(bngvdf[,c("PredictionId","PredictionsDatasetId","Second","PredictionScore","SpeciesCode","SamplingEvent","ModelName")],afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=TRUE)
bngvdf<-bngvdf[,c("PredictionId","AudiofileId","PredictionsDatasetId","Second","SpeciesCode","PredictionScore","ModelName","SamplingEvent")]

# Adding the non-geographic filtered data for BirdNET
ngbndf<-read.csv(paste0(pathToLocalGit,"Soundscapes2Landscapes/CNN_Bird_Species/CNN_post-processing/data/gv_birdnet_analyzer_audacity_2sec_overlap_no_geographic.csv"), stringsAsFactors=FALSE)
ngbndf$birdcode<-ifelse(is.na(ngbndf$birdcode),"XXSP",ngbndf$birdcode)
ngbndf$SamplingEvent<-substr(ngbndf$File,1,32)
ngbngvdf<-ldply(1:nrow(ngbndf),function(rr,ngbndf){
			rdf<-ngbndf[rr,]
			ssec<-rdf$StartSecond
			if(ssec<58){
				rdf<-rbind(rdf,rdf)
				rdf$Second<-c(ssec+1,ssec+2)
			}else{
				rdf$Second<-ssec+1
			}
			return(rdf)
		}, ngbndf=ngbndf)
names(ngbngvdf)<-gsub("birdcode","SpeciesCode",names(ngbngvdf))
ngbngvdf$PredictionScore<-ngbngvdf$Probability*10^7
ngbngvdf$ModelName<-"BirdNET"
ngbngvdf$PredictionsDatasetId<-10
ngbngvdf$PredictionId<-c(1:nrow(ngbngvdf))
ngbngvdf<-merge(ngbngvdf[,c("PredictionId","PredictionsDatasetId","Second","PredictionScore","SpeciesCode","SamplingEvent","ModelName")],afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=TRUE)
ngbngvdf<-ngbngvdf[,c("PredictionId","AudiofileId","PredictionsDatasetId","Second","SpeciesCode","PredictionScore","ModelName","SamplingEvent")]
####################
# Now we need the matching functions for GV data...

#####
# This function is a high-level cycling function seeking the matches for all hurdle levels for each model against the golden validations
# data is the predictions data.frame
# gvedf is the data frame of GV data, from the script makeGVdf.R. Must have: species, secs, SamplingEvent 
# hurdvals is the vactor of hurdles to be used to penalize the data
# gvGetMatches is a function called within this function
# removeDoubleGVMatches is a function called within this function
# REQUIREMENTS: data must have fields PredictionId, ModelName, SpeciesCode, SamplingEvent, and Second
# REQUIREMENTS: hurdle is a value between 0 and 1
# REQUIREMENTS: gvedf must have fields species, secs, SamplingEvent
getModelHurdledGVMatches<-function(data,gvedf,hurdvals,getGVMatches,removeDoubleGVMatches){
  
  library(doParallel)
  nodes<-detectCores()
  cl<-makeCluster(nodes)
  registerDoParallel(cl)
  
  ## CAREFUL - LOOP HERE for models...
  mdllist<-list()
  for(mm in unique(data$ModelName)){
    mdata<-subset(data,ModelName==mm)
    
    hurdleList<-list()
    for(hh in hurdvals){
      hv<-hh*(10^7)
      datah<-subset(mdata,PredictionScore>=hv)
      hbelow<-subset(mdata, PredictionScore<hv)
      ## We start by finding a match to all GV records...
      hurdledf<-ldply(.data=unique(gvedf$SamplingEvent), .parallel=TRUE, .fun=function(ee,datah,hbelow,gvedf,getGVMatches,removeDoubleGVMatches){
        gdf<-subset(gvedf,SamplingEvent==ee)
        gdf<-gdf[order(gdf$secs),]
        rdf<-subset(datah,SamplingEvent==ee)
        rdf<-rdf[order(rdf$Second),]
        
        match<-getGVMatches(rdf=rdf,gdf=gdf,removeDoubleGVMatches=removeDoubleGVMatches)
        if(nrow(match)>0){
          match$SamplingEvent<-ee
        }
        
        # Get TRUEPOS below the hurdle - these are FALSENEG, all others are TRUENEG
        hbdf<-subset(hbelow,SamplingEvent==ee)
        hbmatchp<-data.frame();hbmatchn<-data.frame()
        if(nrow(hbdf)>0){
          hbmatch<-getGVMatches(rdf=hbdf,gdf=gdf,removeDoubleGVMatches=removeDoubleGVMatches)
          hbmatch$SamplingEvent<-ee
          hbmatchp<-subset(hbmatch,match=="TRUEPOS")
          if(nrow(hbmatchp)>0){
            hbmatchp$match<-"FALSENEG";hbmatchp$matchDelta<-8
          }
          hbmatchn<-subset(hbmatch,match!="TRUEPOS")
          if(nrow(hbmatchn)>0){
            hbmatchn$match<-"TRUENEG";hbmatchn$matchDelta<-4
          }
        }
        
        match<-rbind(match,hbmatchp)
        match<-rbind(match,hbmatchn)
        return(match)
        
      },datah=datah,hbelow=hbelow,gvedf=gvedf,getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)
      if(nrow(hurdledf)>0){
        hurdledf$hurdle<-hh
      }
      
      hn<-paste0("Hurdle_",hh)
      hurdleList[[hn]]<-hurdledf
      
    }
    hurdres<-rbindlist(hurdleList)
    hurdres$ModelName<-mm
    
    mdllist[[mm]]<-hurdres
  }
  
  mdlres<-rbindlist(mdllist)
  
  return(mdlres)
}


######
# This is the workhorse matching function for GVs - this matching is minute-by-minute
# rdf is the predictions data.frame called from getModelHurdledGVMatches
# gdf is the GV detections single-event data.frame also called from getModelHurdledGVMatches
# removeDoubleGVMatches is a function called within this function
getGVMatches<-function(rdf,gdf,removeDoubleGVMatches){	#Processing one recording at the time
  
  evt<-gdf$SamplingEvent[1]
  
  ## This needs to happen each GV second by GV second, because the model should predict only where there is a GV record
  # For each GV label: is there a prediction?
  # A GV record reports the starting second of every second of a species call
  eventdf<-data.frame()
  eventfn<-data.frame()
  for(ss in unique(gdf$secs)){
    
    gvsec<-subset(gdf,secs==ss)
    
    ## There can be more than one label in a second
    for(kk in 1:nrow(gvsec)){
      
      # There is a gv record, then...
      spp<-as.character(gvsec[kk,"species"])
      
      secsearch<-seq(ss-1,ss+3)  #Search ss-1 to ss+3 for GV
      trdf<-subset(rdf,Second %in% secsearch)
      
      if(nrow(trdf)==0){	
        # no predictions overlap the GV record, so the GV record is a FN
        # careful here, because the same records will be repeated for species labeled in the same second
        efn<-data.frame(PredictionId=0, SamplingEvent=evt, Second=NA, GVsecond=ss, SpeciesCode=spp, 
                        GVspeciesCode=spp, PredictionScore=10000000, match="FALSENEG", matchDelta=8)
        eventfn<-rbind(eventfn,efn)
        madf<-data.frame()
      }else{
        # there are predictions overlapping the GV record
        # all predictions matching the species are given TRUEPOS
        strdf<-subset(trdf,SpeciesCode==spp)
        madftp<-data.frame(); madffp<-data.frame()
        if(nrow(strdf)>0){
          madftp<-data.frame(PredictionId=strdf$PredictionId, SamplingEvent=strdf$SamplingEvent, Second=strdf$Second, GVsecond=rep(ss,nrow(strdf)), SpeciesCode=strdf$SpeciesCode, 
                             GVspeciesCode=strdf$SpeciesCode, PredictionScore=strdf$PredictionScore, match=rep("TRUEPOS",nrow(strdf)), matchDelta=rep(1,nrow(strdf)))
        }else{
          #there are no predictions for the GV record, the GV record is a FN
          madftp<-data.frame(PredictionId=0, SamplingEvent=evt, Second=NA, GVsecond=ss, SpeciesCode=NA, 
                             GVspeciesCode=spp, PredictionScore=10000000, match="FALSENEG", matchDelta=8)
        }
        
        # all predictions not matching the species are given FALSEPOS (but filtered out later if they end up being TRUEPOS for another GV second)
        # since all predictions not assigned a match, are given a FALSEPOS, we could just omit this....
        nstrdf<-subset(trdf,SpeciesCode!=spp)
        if(nrow(nstrdf)>0){
          madffp<-data.frame(PredictionId=nstrdf$PredictionId, SamplingEvent=nstrdf$SamplingEvent, Second=nstrdf$Second, GVsecond=rep(ss,nrow(nstrdf)), SpeciesCode=nstrdf$SpeciesCode, 
                             GVspeciesCode=rep(spp,nrow(nstrdf)), PredictionScore=nstrdf$PredictionScore, match=rep("FALSEPOS",nrow(nstrdf)), matchDelta=rep(9,nrow(nstrdf)))
        }
        madf<-rbind(madftp,madffp)
      }
      eventdf<-rbind(eventdf,madf)
    }
    
  }
  
  ## We add the false negatives
  eventfn<-unique(eventfn)
  eventdf<-rbind(eventdf,eventfn)
  
  # Must now ensure all FALSEPOS are not TRUEPOS for a different GV record...
  eventTPR<-subset(eventdf,match=="TRUEPOS")$PredictionId##	 
  eventdf<-subset(eventdf, (match %in% c("TRUEPOS","FALSENEG")) | (match=="FALSEPOS" & !PredictionId %in% eventTPR)) #i.e., get all tp and fn,and only those fp that are not a record in eventTPR
  
  ## We then add all predictions not in eventdf at all (by PredictionId) and make them FP
  drdf<-subset(rdf,!PredictionId %in% eventdf$PredictionId)
  if(nrow(drdf)>0){
    dmadf<-data.frame(PredictionId=drdf$PredictionId, SamplingEvent=drdf$SamplingEvent, Second=drdf$Second, GVsecond=rep(NA,nrow(drdf)), SpeciesCode=drdf$SpeciesCode, 
                      GVspeciesCode=rep(NA,nrow(drdf)), PredictionScore=drdf$PredictionScore, match=rep("FALSEPOS",nrow(drdf)), matchDelta=rep(9,nrow(drdf)))
    eventdf<-rbind(eventdf,dmadf)
  }
  
  # Done
  return(eventdf)
}


#####
# Since getMatches can assign the same TRUEPOS or FALSEPOS prediction to two different and consecutive GV records (because a prediction spans 2 seconds and a GV label only 1)
# This function removes matches to ensure the same prediction does not match two consecutive GV records and thus is entered as a TRUEPOS twice
# This function is called by the getMatches function - NOT BEING USED (not needed) but left here so the code does not fail
# edf is the data frame sent from the getMatches function
removeDoubleGVMatches<-function(edf){
  edf$eventRecId<-1:nrow(edf)
  eventTPR<-subset(edf,match=="TRUEPOS")$PredictionId
  if(NROW(eventTPR)>NROW(unique(eventTPR))){
    remTPR<-numeric()
    for(pp in unique(eventTPR)){
      terem<-subset(edf,match=="TRUEPOS" & PredictionId==pp)$eventRecId
      if(NROW(terem)>1){remTPR<-c(remTPR,terem[-1])}
    }
    edf<-subset(edf,!eventRecId %in% c(remTPR))
  }
  
  eventFPR<-subset(edf,match=="FALSEPOS")$PredictionId
  if(NROW(eventFPR)>NROW(unique(eventFPR))){
    remFPR<-numeric()
    for(pp in unique(eventFPR)){
      ferem<-subset(edf,match=="FALSEPOS" & PredictionId==pp)$eventRecId
      if(NROW(ferem)>1){remFPR<-c(remFPR,ferem[-1])}
    }
    edf<-subset(edf,!eventRecId %in% c(remFPR))
  }
  
  edf<-edf[,which(names(edf)!="eventRecId")]
  return(edf)
}


## Now we need the same for the ROI predictions vs ROI data
getModelHurdledROIMatches<-function(data,roidf,hurdvals,getROIMatches){
  
  library(doParallel)
  nodes<-detectCores()
  cl<-makeCluster(nodes)
  registerDoParallel(cl)
  
  ## CAREFUL - LOOP HERE for models...
  mdllist<-list()
  for(mm in unique(data$ModelName)){
    mdata<-subset(data,ModelName==mm)
    
    hurdleList<-list()
    for(hh in hurdvals){
      hv<-hh*(10^7)
      datah<-subset(mdata,PredictionScore>=hv)
      hbelow<-subset(mdata, PredictionScore<hv)
      ## We start by finding a match to all GV records...
      hurdledf<-ldply(.data=unique(roidf$SamplingEvent), .parallel=TRUE, .fun=function(ee,datah,hbelow,roidf,getROIMatches){  #usually one, but could be more than one record per sampling event.
        gdf<-subset(roidf,SamplingEvent==ee)
        gdf<-gdf[order(gdf$secs),]
        rdf<-subset(datah,SamplingEvent==ee)
        rdf<-rdf[order(rdf$Second),]
        
        match<-getROIMatches(rdf=rdf,gdf=gdf)
        if(nrow(match)>0){
          match$SamplingEvent<-ee
        }
        
        # Get TRUEPOS below the hurdle - these are FALSENEG, all others are TRUENEG
        hbdf<-subset(hbelow,SamplingEvent==ee)
        hbmatchp<-data.frame();hbmatchn<-data.frame()
        if(nrow(hbdf)>0){
          hbmatch<-getROIMatches(rdf=hbdf,gdf=gdf)
          hbmatch$SamplingEvent<-ee
          hbmatchp<-subset(hbmatch,match=="TRUEPOS")
          if(nrow(hbmatchp)>0){
            hbmatchp$match<-"FALSENEG";hbmatchp$matchDelta<-8
          }
        }
        
        match<-rbind(match,hbmatchp)
        return(match)
        
      },datah=datah,hbelow=hbelow,roidf=roidf,getROIMatches=getROIMatches)
      if(nrow(hurdledf)>0){
        hurdledf$hurdle<-hh
      }
      
      #Here convert any TP to FP and any FN to TN for absence data
      hurdledf$match<-ifelse(hurdledf$match=="TRUEPOS" & hurdledf$vote=="not present","FALSEPOS",
                             ifelse(hurdledf$match=="FALSENEG" & hurdledf$vote=="not present","TRUENEG",hurdledf$match))
      
      hn<-paste0("Hurdle_",hh)
      hurdleList[[hn]]<-hurdledf
      
    }
    hurdres<-rbindlist(hurdleList)
    hurdres$ModelName<-mm
    
    mdllist[[mm]]<-hurdres
  }
  
  mdlres<-rbindlist(mdllist)
  
  return(mdlres)
}


######
# This is the workhorse matching function for GVs - this matching is minute-by-minute
# rdf is the predictions data.frame called from getModelHurdledROIMatches
# gdf is the ROI label single-event data.frame also called from getModelHurdledROIMatches
getROIMatches<-function(rdf,gdf){	#Processing one recording at the time
  
  evt<-gdf$SamplingEvent[1]
  
  ## This needs to happen each ROI second and not anywhere else, and we care only for the detection of the species in the label, not the others
  # For each ROI label: is there a prediction?
  # Note that there are no FALSEPOS in ROI predictions, only TRUEPOS and FALSENEG
  eventdf<-data.frame()
  for(rr in 1:nrow(gdf)){  #most likely there is only 1 sec (i.e., one label) per event, but...
    
    gvsec<-gdf[rr,]
    ss<-gvsec$secs[1]
    
    ## There cannot be more than one label in a second
    spp<-as.character(gvsec$species)
    vt<-as.character(gvsec$vote)
    
    secsearch<-seq(ss-1,ss+1)  #Search ss-1 to ss+1 among predictions for ROI labels
    trdf<-subset(rdf,Second %in% secsearch)
    
    # There is an ROI label but no prediction... 			
    if(nrow(trdf)==0){	
      # no predictions overlap the ROI record, so the ROI record is a FN
      efn<-data.frame(PredictionId=0, SamplingEvent=evt, Second=NA, GVsecond=ss, SpeciesCode=spp, 
                      GVspeciesCode=spp, PredictionScore=10000000, match="FALSENEG", matchDelta=8, vote=vt)
      eventdf<-rbind(eventdf,efn)
      
    }else{
      # there are predictions overlapping the ROI record
      # all predictions matching the species are given TRUEPOS
      strdf<-subset(trdf,SpeciesCode==spp)
      if(nrow(strdf)>0){
        efn<-data.frame(PredictionId=strdf$PredictionId, SamplingEvent=strdf$SamplingEvent, Second=strdf$Second, GVsecond=rep(ss,nrow(strdf)), SpeciesCode=strdf$SpeciesCode, 
                        GVspeciesCode=strdf$SpeciesCode, PredictionScore=strdf$PredictionScore, match=rep("TRUEPOS",nrow(strdf)), matchDelta=rep(1,nrow(strdf)),vote=rep(vt,(nrow(strdf))))
        eventdf<-rbind(eventdf,efn)
      }else{
        #The predictions are not of the same species as the label, thus the ROI record is a falseneg
        efn<-data.frame(PredictionId=0, SamplingEvent=evt, Second=ss, GVsecond=ss, SpeciesCode=spp, 
                        GVspeciesCode=spp, PredictionScore=10000000, match="FALSENEG", matchDelta=8, vote=vt)
        eventdf<-rbind(eventdf,efn)
      }
      
    }
    
  }
  
  # Done
  return(eventdf)
}

######
# The following are the summarization functions from match results
#####
# This function provides metrics by hurdle value, using summarizeToSample to estimate by species and model
# allmatches is the data frame of all the matches at all hurdle levels for all species and models
# bySpecies indicates if the summarization should be species specific, defaults to "no"
# summarizeToSample and summarizeToEvent are sub-functions called by this function, described below
# sumLevel indicates if the summary match is to be done clip by clip ("clip") or minute by minute ("minute" - do not use)
# beta is the F function modifier, with values >0. The smaller the value (e.g., 0.5) the higher the penalization for FP
# addEvent indicates...
# Metrics: 
# 	Sensitivity: TP/(TP+FN)
# 	Precision: TP/(TP+FP)
# 	F1: (2*Sensitivity*Precision)/(Sensitivity+Precision)
# 	Miss rate: FN/(FN+TP)
#	FPper: FP/(TP+FP)
#   Fbeta: (1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
summarizeByHurdle<-function(allmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE){
  hurdvals<-unique(allmatches$hurdle)
  sumByHurdle<-ldply(hurdvals,function(hh,allmatches,summarizeToSample,summarizeToEvent,bySpecies,sumLevel,beta,addEvent){
    hmatches<-subset(allmatches,hurdle==hh)
    if(bySpecies=="no"){
      if(sumLevel=="minute"){
        sumMatches<-summarizeToEvent(matches=hmatches,beta=beta,bySpecies=bySpecies,addEvent=addEvent)
      }else{
        sumMatches<-summarizeToSample(matches=hmatches,beta=beta)
      }
    }else{
      if(sumLevel=="minute"){
        sumMatches<-summarizeToEvent(matches=hmatches,beta=beta,bySpecies=bySpecies,addEvent=addEvent)
      }else{
        sumMatches<-ldply(unique(hmatches$SpeciesCode),function(ss,hmatches,summarizeToSample,beta){
          shmatches<-subset(hmatches,SpeciesCode==ss)
          ssmatches<-summarizeToSample(matches=shmatches,beta=beta)
          ssmatches$SpeciesCode<-ss
          return(ssmatches)
        },hmatches=hmatches,summarizeToSample=summarizeToSample,beta=beta)
      }
      
    }
    sumMatches$hurdle<-hh
    return(sumMatches)
  },allmatches=allmatches,summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,bySpecies=bySpecies,sumLevel=sumLevel,beta=beta,addEvent=addEvent)
  return(sumByHurdle)
}

####
# This function is called by summarizeByHurdle and provides a report of metric values by model for all records combined
# That is, it does not differentiate among species or events.
# matches is the subset of all matches (by hurdle, for example, but could be by hurdle+species) passed to the function
summarizeToSample<-function(matches,beta){
  sumToModel<-ldply(unique(matches$ModelName),function(mm,matches){
    mdf<-subset(matches,ModelName==mm)
    trval<-sum(mdf$match=="TRUEPOS")
    fnval<-sum(mdf$match=="FALSENEG")
    fpval<-sum(mdf$match=="FALSEPOS")
    sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
    prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
    f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
    miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
    fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
    fbeta<-ifelse(sens==0,0,(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens)))
    modeldf<-data.frame(count=nrow(mdf),truePos=trval,falseNeg=fnval,falsePos=fpval,
                        F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
    modeldf$ModelName<-mm
    return(modeldf)
  },matches=matches)
  return(sumToModel)
}

####
# This function is called by summarizeByHurdle and provides a report of metric values by model and possibly also by event
# matches is the subset of all matches (by hurdle, for example, but could be by hurdle+species) passed to the function
# bySpecies indicates if the summarization should be species specific, defaults to "no"
# addEvent defaults to FALSE. If set to TRUE this function attempts a summarization by event (not recommended)
summarizeToEvent<-function(matches,bySpecies,beta,addEvent){
  #split by model
  sumToModel<-ldply(unique(matches$ModelName),function(mm,matches,bySpecies,beta,addEvent){
    mdf<-subset(matches,ModelName==mm)
    #asign TP, FP, and FN by species+event, then add up by model or by species and model
    eventspeciesdf<-ldply(unique(mdf$SpeciesCode),function(ss,mdf,addEvent){
      smatches<-subset(mdf,SpeciesCode==ss)
      eventsdf<-ldply(unique(smatches$SamplingEvent),function(ee,smatches){
        edf<-subset(smatches,SamplingEvent==ee)
        if(TRUE %in% grepl("TRUEPOS",edf$match)){
          trv<-1;fnv<-0;fpv<-0
        }else{
          trv<-0
          fnv<-ifelse(TRUE %in% grepl("FALSENEG",edf$match),1,0)
          fpv<-ifelse(TRUE %in% grepl("FALSEPOS",edf$match),1,0)
        }
        redf<-data.frame(trv=trv,fnv=fnv,fpv=fpv)
        if(addEvent==TRUE){redf$event<-ee}
        return(redf)
      },smatches=smatches)
      eventsdf$SpeciesCode<-ss
      return(eventsdf)
    },mdf=mdf,addEvent=addEvent)
    
    if(addEvent==TRUE){ #we aggregate at the species level for each event
      mdldf<-eventspeciesdf %>% group_by(SpeciesCode,event) %>% dplyr::summarise(trval=sum(trv),fnval=sum(fnv),fpval=sum(fpv),count=NROW(trv))
      modeldf<-ldply(unique(mdldf$event),function(ee,mdldf,beta){
        evmdldf<-subset(mdldf,event==ee)
        if(bySpecies=="yes"){
          spmdldf<-ldply(unique(evmdldf$SpeciesCode),function(ss,evmdldf,beta){
            sdf<-subset(evmdldf,SpeciesCode==ss) #there should be just one value per species+model+hurdle
            sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
            prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
            f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
            miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
            fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
            fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
            resdf<-data.frame(SpeciesCode=ss,Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
                              F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
          },evmdldf=evmdldf,beta=beta)
          
        }else{
          #Before calculating the statistics, aggregate to the event-model level
          sdf<-evmdldf %>% dplyr::summarise(trval=sum(trval),fnval=sum(fnval),fpval=sum(fpval),count=sum(count))
          #now there should be just one row per model
          sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
          prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
          f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
          miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
          fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
          fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
          spmdldf<-data.frame(Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
                              F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
          
        }
        spmdldf$event<-ee
        return(spmdldf)
      },mdldf=mdldf,beta=beta)
      
    }else{# or we aggregate at the species level across events
      mdldf<-eventspeciesdf %>% group_by(SpeciesCode) %>% dplyr::summarise(trval=sum(trv),fnval=sum(fnv),fpval=sum(fpv),count=NROW(trv))
      
      if(bySpecies=="yes"){
        modeldf<-ldply(unique(mdldf$SpeciesCode),function(ss,mdldf,beta){
          sdf<-subset(mdldf,SpeciesCode==ss) #there should be just one value per species+model+hurdle
          sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
          prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
          f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
          miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
          fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
          fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
          resdf<-data.frame(SpeciesCode=ss,Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
                            F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
        },mdldf=mdldf,beta=beta)
        
      }else{
        #Before calculating the statistics, aggregate to the model level
        sdf<-mdldf %>% dplyr::summarise(trval=sum(trval),fnval=sum(fnval),fpval=sum(fpval),count=sum(count))
        #now there should be just one row per model
        sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
        prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
        f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
        miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
        fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
        fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
        modeldf<-data.frame(Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
                            F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
      }
      
    }
    
    modeldf$ModelName<-mm
    return(modeldf)
    
  },matches=matches,bySpecies=bySpecies,beta=beta,addEvent=addEvent)
  
  return(sumToModel)
}

####################
# For the pre-trained predictions vs GV
ptGVmatches<-getModelHurdledGVMatches(data=predGV_PTdf,gvedf=gvedf,hurdvals=seq(0.65,0.99,by=0.01),getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)

# For the not-pre-trained predictions vs GV
nptGVmatches<-getModelHurdledGVMatches(data=predGV_NPTdf,gvedf=gvedf,hurdvals=seq(0.65,0.99,by=0.01),getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)

## CAREFUL - now need the code to discern between presences and absences - use the absences for potential FP (only for ROI data, because otherwise we would only have TP and FN)
roimdf<-roiTestdf[,c("birdcode","x1","x2","SamplingEvent","vote")]
roimdf$secs<-round(roimdf$x2-roimdf$x1)
roimdf<-roimdf[,c("birdcode","secs","SamplingEvent","vote")]
names(roimdf)<-c("species","secs","SamplingEvent","vote")
# there are dupes, so...
roimdf<-unique(roimdf)
# For the pre-trained predictions vs ROI data
ptROImatches<-getModelHurdledROIMatches(data=predROI_PTdf,roidf=roimdf,hurdvals=seq(0.65,0.99,by=0.01),getROIMatches=getROIMatches)

# For the notpre-trained predictions vs ROI data
nptROImatches<-getModelHurdledROIMatches(data=predROI_NPTdf,roidf=roimdf,hurdvals=seq(0.65,0.99,by=0.01),getROIMatches=getROIMatches)

# Need the same for the BirdNET data vs GV
bnGVmatches<-getModelHurdledGVMatches(data=bngvdf,gvedf=gvedf_bn,hurdvals=seq(0.65,0.99,by=0.01),getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)
ngbnGVmatches<-getModelHurdledGVMatches(data=ngbngvdf,gvedf=gvedf_bn,hurdvals=seq(0.65,0.99,by=0.01),getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)

save(ptGVmatches,nptGVmatches,ptROImatches,nptROImatches,bnGVmatches,ngbnGVmatches,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/database/ROI_GV_allmatches_011523.RData")
#############################################

## END OF DATA PROCESSING
