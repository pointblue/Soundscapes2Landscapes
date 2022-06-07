# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## In this file we start with the newest csv of inspected clips for a set of species
## We query the predictions for these, looking for FP and TP predictions (in code files getROItruepos.R and getROIfalsepos.R)
## Those found are consequently attributed with the covariate data used in the logistic regression
## We save these with a versioning name, to be used in a new fit of the logistic regression models (see matchGVtoPreds_part4.R)

## We start by retrieving the latest list of true- and falsepositives 
libs<-c("plyr","RMySQL","data.table","dplyr")
lapply(libs, require, character.only = TRUE)

#pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"
pathToLocalGit<-"/home/ubuntu/S2L_devel/"

source(paste0(pathToLocalGit,"GVanalyses/3models2outputs/scripts/logisticCorr/logisticCorr_dataPrep_utils.R"))

load(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/FPmatches_list_preTrained_06142021.RData"))
load(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/TPmatches_list_preTrained_06142021.RData"))


## Then determine for each model and species how to create a balanced sample
# first listing the number of TP and FP per species and model
models<- names(fpMatchesList)
species<-unique(fpMatchesList[[1]]$SpeciesCode)  #species for which we have FP data
tpspecies<-unique(tpMatchesList[[1]]$SpeciesCode)  #all species modeled
tpspecies[which(!tpspecies %in% species)]  #species with no FP data

tpfpdf<-ldply(models,function(mm,species,fpMatchesList,tpMatchesList){
				tpdf<-tpMatchesList[[mm]]
				fpdf<-fpMatchesList[[mm]]
				spdf<-ldply(species,function(ss,tpdf,fpdf){
							tps<-nrow(subset(tpdf,SpeciesCode==ss))
							fps<-nrow(subset(fpdf,SpeciesCode==ss))
							tdf<-data.frame(SpeciesCode=ss,TPcount=tps,FPcount=fps)
							return(tdf)
						},tpdf=tpdf,fpdf=fpdf)
				spdf$ModelName<-mm
				return(spdf)
		},species=species,fpMatchesList=fpMatchesList,tpMatchesList=tpMatchesList)

## We can only work with a sufficient number of FP, and setting a filter of species+models to have at least 10 FP is the same as setting it to have 32 FP, so:
tpfpdf<-subset(tpfpdf, FPcount>=30)
## list the species we cannot model
tpspecies[which(!tpspecies %in% unique(tpfpdf$SpeciesCode))]

## Now for each model and species we take a sample of all TP that matches the number of FP (if TP > 1.2FP)
fpsellst<-llply(models,function(mm,tpfpdf,fpMatchesList,tpMatchesList){
			tpm<-subset(tpfpdf,ModelName==mm)
			mdldf<-ldply(1:nrow(tpm),function(ii,mm,tpm,fpMatchesList,tpMatchesList){
						sp<-tpm[ii,"SpeciesCode"]; tpc<-tpm[ii,"TPcount"]; fpc<-tpm[ii,"FPcount"]
						tpdf<-subset(tpMatchesList[[mm]],SpeciesCode==sp); tpdf$matchval<-1
						fpdf<-subset(fpMatchesList[[mm]],SpeciesCode==sp); fpdf$matchval<-0
						if(tpc>1.2*fpc){
							tpdf<-tpdf[sample(nrow(tpdf), 1.2*fpc), ]
						}
						resdf<-rbind(tpdf,fpdf)
						return(resdf)
					},mm=mm,tpm=tpm,fpMatchesList=fpMatchesList,tpMatchesList=tpMatchesList)
			return(mdldf)
		},tpfpdf=tpfpdf,fpMatchesList=fpMatchesList,tpMatchesList=tpMatchesList)
names(fpsellst)<-models

## Next, collect all the covariate data for the tp and fp data for each species and model
## Starting with a table of all AudiofileIds we will need
lgevents<-unique(c(unique(fpsellst[[1]]$SamplingEvent),unique(fpsellst[[2]]$SamplingEvent),unique(fpsellst[[3]]$SamplingEvent)))
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
afidsdf<-getAudiofileIds(lgevents,con)
dbDisconnect(con)
Sys.time()-tm

## To merge back to the fpsellst of data.frames, we first subset afidsdf for isevent==1
afidIsEvent<-subset(afidsdf,isevent==1)
for(ii in 1:3){
	tdf<-fpsellst[[ii]]
	tdf<-merge(tdf,afidIsEvent[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
	tdf$PredictionsDatasetId<-ifelse(tdf$ModelName=="Resnet50::sigmoid",2,ifelse(tdf$ModelName=="Resnet101::sigmoid",4,6))
	fpsellst[[ii]]<-tdf
}

save(afidsdf,fpsellst,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logCorr_AfIds_06142021.RData"))

## Now we get the covariate data we need:
sppdf<-data.frame(SpeciesCode=tpspecies)
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
tm<-Sys.time()
mdlspcovlst<-list()
for(ii in 1:3){
	targetdf<-fpsellst[[ii]]
	mdlspcovars<-getLogCorrVars(targetdf=targetdf,con=con,eventIddf=afidsdf,sppdf=sppdf,hurdval=0.65)
	mdlspcovlst[[ii]]<-mdlspcovars
}
dbDisconnect(con)
Sys.time()-tm

names(mdlspcovlst)<-names(fpsellst)

save(mdlspcovlst,fpsellst,afidsdf,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logCorr_modelCovarData_06142021.RData"))
## Finally, we are ready to fit the models, one per species and cnn model type.
