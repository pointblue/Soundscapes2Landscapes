# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("ggplot2","plyr","RMySQL","data.table","reticulate","dplyr","rminer")
lapply(libs, require, character.only = TRUE)

pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"

## We start by getting the GV data
# These were read from the AirTable and saved into the R object below with the code file makeGVdf.R
load(file=paste0(pathToLocalGit,"GVanalyses/data/gvData_20210518.RData"))
sounddf<-aggregate(secs~species,gvedf,NROW); names(sounddf)<-c("SpeciesCode","DetectionCount")

# ...determine the number during dwn chorus (6-9 AM)
gvedf$hour<-substr(gvedf$RegistryName,28,29)
dcgvedf<-subset(gvedf,hour %in% c("06","07","08"))
dcdf<-aggregate(secs~species,dcgvedf,NROW); names(dcdf)<-c("SpeciesCode","DawnCount")
sounddf<-merge(sounddf,dcdf,by="SpeciesCode",all.x=TRUE)
sounddf$DawnCount<-ifelse(is.na(sounddf$DawnCount),0,sounddf$DawnCount)
sounddf$PCTdawnchorus<-round(100*sounddf$DawnCount/sounddf$DetectionCount)

# ...but then we need to filter for the species of interest, and further only the training calls
speciesdf<-data.frame(SpeciesNum=1:54,SpeciesCode=c('STJA', 'EUCD', 'ACWO', 'GHOW', 'BRCR', 'COYE', 'BTPI', 'OSFL', 
				'AMCR', 'WBNU', 'HETH', 'CASJ', 'WITU', 'PIWO', 'LAZB', 'CAVI', 
				'BUOR', 'CALT', 'WETA', 'WEME', 'PAWR', 'RWBL', 'BHGR', 'HUVI', 
				'ATFL', 'MODO', 'WREN', 'NOFL', 'MOUQ', 'OCWA', 'SOSP', 'OATI', 
				'NUWO', 'PSFL', 'WCSP', 'WIWA', 'HOWR', 'CBCH', 'CAQU', 'DEJU', 
				'CORA', 'BEWR', 'GRSP', 'HOFI', 'RSHA', 'ANHU', 'BGGN', 'BTYW', 
				'SWTH', 'MAWR', 'WAVI', 'SPTO', 'AMRO', 'SAVS'))
speciesmodeled<-speciesdf$SpeciesCode

gvedf<-subset(gvedf,!is.na(call)); nrow(gvedf)  
gvedf$spcall<-paste0(toupper(gvedf$species),"::",toupper(gvedf$call))

## Now we need the species and calls selected for training:
roidf<-read.csv(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_210510.csv"), 
		stringsAsFactors=FALSE)
roicalls<-unique(roidf[,c("birdcode","songtype")])
roicalls$spcall<-paste0(toupper(roicalls$birdcode),"::",toupper(roicalls$songtype))
gvedf<-subset(gvedf,spcall %in% roicalls$spcall)
tcgvdf<-aggregate(secs~species,gvedf,NROW); names(tcgvdf)<-c("SpeciesCode","TrainCallDetections")
sounddf<-merge(sounddf,tcgvdf,by="SpeciesCode",all.x=TRUE)
sounddf$TrainCallDetections<-ifelse(is.na(sounddf$TrainCallDetections),0,sounddf$TrainCallDetections)
sounddf<-subset(sounddf,SpeciesCode %in% speciesdf$SpeciesCode)
head(sounddf)

#################### 
## Not all species have sufficient data for evaluation of performance.
## Here we filter for those with >40 detections
sounddf$Include<-ifelse(sounddf$TrainCallDetections>40,"Yes","No")

# write to csv to include in ms.
write.csv(sounddf,file="c:/users/lsalas/downloads/gvspeciesTable.csv")

