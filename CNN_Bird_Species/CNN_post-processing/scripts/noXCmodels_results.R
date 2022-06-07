# TODO: Add comment
# 
# Author: lsalas
###############################################################################

########
## machGVtoPreds.R reads and tabulates the Airtable data on golden validations
## matchGVtoPreds2.R reads the predictions from the database and compares against the GV at an array of hurdles
## matchGVtoPreds3.R finds the best model for each species and generates teh data for the logistic correction
## matchGVtoPreds4.R fits the correcting logistic model

## Chosen model for each species here: c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData - from matchGVtoPreds_part3.R

## Model performance data are summarized in plotPrecisionRecall.R
## Performance data saved here: c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/performanceData.RData
##   We have performance of the new XC pretrained models vs labeled ROIs:pretrainedLabeled
##   We have performance of the new XC pretrained models vs Shree ROIs:pretrainedShree
##   We have performance of the new XC pretrained models vs GV:pretrainedGV
## We have BirdNET performance against labeled ROIs: birdnetLabeled
## We have BirdNet performance against GV: birdnetGV
##### NEED to filter these against the model selected for each species before plotting, to be compatible with corrected data

## We have the performance of the new models after logistic correction too
## Saved here: c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrectionData.RData


## We need the no-XC-pretrained model performance vs ROIs. This file does that.
## Get the list of GV events
## Download and tabulate the pickles for these
## Match against the GV data
## Summarize by hurdle level
## Make PR plot

## This file reads the pickles of predictions to GV files into data.frames
library(plyr); library(reticulate);library(data.table); library(ggplot2)

roidf<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/data/pattern_matching_ROIs_201109.csv", stringsAsFactors=FALSE)
load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/gvData.RData")

##############################################################################################################################
### DONE - skip

## ATTENTION: two pickles not found - s2lam050_190411_2019-04-12_19-20.pkl and s2lam050_190427_2019-04-28_06-10.pkl
downpath<-"c:/users/lsalas/downloads/"
source_python("c:/users/lsalas/git/S2L_devel/GVanalyses/PreTrainEval/pickle_joblib_reader.py")

# filename is the name of the pickle file
# model is one of four: cols_54cls_mobnet_full_finetune, cols_54cls_Resnet101, cols_54cls_Resnet50, sp40_54cls_mobnet_full_finetune
returnFileVector<-function(filename,datapath,model,outtype="sigmoid",minhurd=0.65){
	filepath<-paste0(datapath,filename)
	if(file.exists(filepath)){
		pdat<-read_pickle_file(filepath)
		filedf<-ldply(1:NROW(pdat),function(ss,pdat,filename,model,outtype,minhurd){
					filenm<-gsub("pkl","wav",filename)
					score<-as.numeric(pdat[[ss]])*(10^7)
					smdf<-data.frame(PredictionId=0,AudiofileId=0,PredictionsDatasetId=model,Second=ss,SpeciesNum=1:54,Score=score,RegistryName=filenm,outtype=outtype,note="")
					smdf<-smdf[order(smdf$Score,decreasing=T),]
					if(outtype=="sigmoid"){
						retdf<-subset(smdf,Score>=(minhurd*(10^7)))
					}else{
						retdf<-smdf[1,]
					}
					return(retdf)
				},pdat=pdat,filename=filename,model=model,outtype=outtype,minhurd=minhurd)
	}else{
		filenm<-gsub("pkl","wav",filename)
		filedf<-data.frame(PredictionId=0,AudiofileId=0,PredictionsDatasetId=model,Second=0,SpeciesNum=0,Score=0,RegistryName=filenm,outtype=outtype,note="Pickle file not found")
	}
	
	return(filedf)
}

## Get the list of GV events to process
gvevents<-unique(gvedf$RegistryName)
gvpkls<-gsub(".wav",".pkl",gvevents)

gvdflist<-llply(c("IMGNET_54cls_mobnet_full_finetune/","IMGNET_54cls_ResNet50/","IMGNET_54cls_ResNet101/"),function(mm,downpath,gvpkls,returnFileVector){
			datapath<-paste0(downpath,"GVs/",mm)
			modeldata<-ldply(gvpkls,function(pp,datapath,model,returnFileVector){
						filevect<-returnFileVector(filename=pp,datapath=datapath,model=model)
						return(filevect)
					},datapath=datapath,model=mm,returnFileVector=returnFileVector)
			return(modeldata)
		},downpath=downpath,gvpkls=gvpkls,returnFileVector=returnFileVector)

########################

## Get the list of ROI events to process
roievents<-unique(roidf$filename)
roipkls<-gsub(".wav",".pkl",roievents)

roidflist<-llply(c("IMGNET_54cls_mobnet_full_finetune/","IMGNET_54cls_ResNet50/","IMGNET_54cls_ResNet101/"),function(mm,downpath,roipkls,returnFileVector){
			datapath<-paste0(downpath,"ROIs/",mm)
			modeldata<-ldply(roipkls,function(pp,datapath,model,returnFileVector){
						filevect<-returnFileVector(filename=pp,datapath=datapath,model=model)
						return(filevect)
					},datapath=datapath,model=mm,returnFileVector=returnFileVector)
			return(modeldata)
		},downpath=downpath,roipkls=roipkls,returnFileVector=returnFileVector)

save(gvdflist,roidflist,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/untraiedPredictions_gv_roi.RData")

####################################################################################################################################
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/untraiedPredictions_gv_roi.RData")

## Add species code to the pickle data and filter by the selected model
speciesdf<-data.frame(SpeciesNum=1:54,SpeciesCode=c('STJA', 'EUCD', 'ACWO', 'GHOW', 'BRCR', 'COYE', 'BTPI', 'OSFL', 'AMCR', 'WBNU', 'HETH', 'CASJ', 'WITU', 'PIWO', 'LAZB', 'CAVI', 
				'BUOR', 'CALT', 'WETA', 'WEME', 'PAWR', 'RWBL', 'BHGR', 'HUVI', 'ATFL', 'MODO', 'WREN', 'NOFL', 'MOUQ', 'OCWA', 'SOSP', 'OATI', 'NUWO', 'PSFL', 'WCSP', 'WIWA', 'HOWR', 
				'CBCH', 'CAQU', 'DEJU', 'CORA', 'BEWR', 'GRSP', 'HOFI', 'RSHA', 'ANHU', 'BGGN', 'BTYW', 'SWTH', 'MAWR', 'WAVI', 'SPTO', 'AMRO', 'SAVS'))

load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData")
names(spmodel)<-c("ModelName","SpeciesCode","F05v","Prec05")

gvdflst<-llply(1:3,function(nn,gvdflist,spmodel,speciesdf){
	tdf<-gvdflist[[nn]]
	tdf<-subset(tdf,SpeciesNum>0)
	names(tdf)<-gsub("Score","PredictionScore",names(tdf))
	tdf$SamplingEvent<-substr(tdf$RegistryName,1,32)
	tdf<-merge(tdf,speciesdf,by="SpeciesNum",all.x=TRUE)
	tdf$ModelName<-ifelse(grepl("mobnet",tdf$PredictionsDatasetId[1]),"MobileNet::sigmoid",ifelse(grepl("ResNet50",tdf$PredictionsDatasetId[1]),"Resnet50::sigmoid","Resnet101::sigmoid"))
	tdf$PredictionsDatasetId<-ifelse(tdf$ModelName=="MobileNet::sigmoid",6,ifelse(tdf$ModelName=="Resnet50::sigmoid",2,4))
	#tdf<-merge(tdf,spmodel,by=c("ModelName","SpeciesCode"),all.x=TRUE)
	#tdf<-subset(tdf,!is.na(F05v))
	return(tdf)
},gvdflist=gvdflist,spmodel=spmodel,speciesdf=speciesdf)

roidflst<-llply(1:3,function(nn,roidflist,spmodel,speciesdf){
	tdf<-roidflist[[nn]]
	tdf<-subset(tdf,SpeciesNum>0)
	names(tdf)<-gsub("Score","PredictionScore",names(tdf))
	tdf$SamplingEvent<-substr(tdf$RegistryName,1,32)
	tdf<-merge(tdf,speciesdf,by="SpeciesNum",all.x=TRUE)
	tdf$ModelName<-ifelse(grepl("mobnet",tdf$PredictionsDatasetId[1]),"MobileNet::sigmoid",ifelse(grepl("ResNet50",tdf$PredictionsDatasetId[1]),"Resnet50::sigmoid","Resnet101::sigmoid"))
	tdf$PredictionsDatasetId<-ifelse(tdf$ModelName=="MobileNet::sigmoid",6,ifelse(tdf$ModelName=="Resnet50::sigmoid",2,4))
	#tdf<-merge(tdf,spmodel,by=c("ModelName","SpeciesCode"),all.x=TRUE)
	#tdf<-subset(tdf,!is.na(F05v))
	return(tdf)
},roidflist=roidflist,spmodel=spmodel,speciesdf=speciesdf)

## Need these functions
## This function does the matches for all hurdle levels to be used for test and prod data
getModelHurdledMatches<-function(data,gvedf,hurdvals){
	res<-ldply(unique(data$ModelName),function(mm,data,gvedf,hurdvals){
				modelMatches<-data.frame()
				matches<-data.frame()
				datm<-subset(data,ModelName==mm)
				for(ss in unique(datm$SpeciesCode)){
					datms<-subset(datm,SpeciesCode==ss)
					for(hh in hurdvals){
						hv<-hh*(10^7)
						datmsh<-subset(datms,PredictionScore>=hv)
						for(ee in unique(gvedf$SamplingEvent)){  
							rdf<-subset(datmsh,SamplingEvent==ee)
							if(nrow(rdf)>0){rdf<-rdf[order(rdf$Second),]}
							gdf<-subset(gvedf,species==ss & SamplingEvent==ee)
							if(nrow(gdf)>0){gdf<-gdf[order(gdf$secs),]}
							match<-getMatches(rdf=rdf,gdf=gdf)
							if(nrow(match)>0){
								match$SamplingEvent<-ee
								match$hurdle<-hh
								matches<-rbind(matches,match)
							}
						}
						
					}
				}
				if(nrow(matches)>0){
					matches$ModelName<-mm
					matches$PredictionScoreType<-"sigmoid"
					matches$modelVersion<-unique(datm$modelVersion)
					modelMatches<-rbind(modelMatches,matches)
				}
					
				
				return(modelMatches)
				
			},data=data,gvedf=gvedf,hurdvals=hurdvals)
	return(res)
}

## Match against the GV for each output and threshold, generate table of 0/1, save
## Need these functions:
# This matches each file's cnn detections to the GVs and returns a data frame with second, cnnSpecies. gvspecies - this matching is minute-by-minute
# rdf is the cnn predictions
# gdf is the GV detections
getMatches<-function(rdf,gdf){	#Processing one recording at the time
	
	## ADD call type to the match
	## MAKE SURE all predictions are reported on
	
	#both dfs contain data for the same file, so it must be a match second by second first, then explore +/-1, 2 and 3. Keep the first species found and report that species if no match
	#consider first the special cases of 0 predictions or 0 gv detections
	if(nrow(rdf)==0){	#this should never happen, but...
		if(nrow(gdf)==0){  #No data in either, nothing to match
			madf<-data.frame()
		}else{
			# All records in gdf are false negatives
			madf<-data.frame(PredictionId=rep(NA,nrow(gdf)), Second=gdf$secs, GVsecond=gdf$secs, SpeciesCode=gdf$species, GVspeciesCode=gdf$species, PredictionScore=rep(0,nrow(gdf)), match="FALSENEG", matchDelta=rep(8,nrow(gdf)))
		}
	}else if(nrow(gdf)==0){	#It may be the case that no species' calls were detected in this recording
		# All records in rdg are false positives
		madf<-data.frame(PredictionId=rdf$PredictionId, Second=rdf$Second, GVsecond=rep(-9,nrow(rdf)), SpeciesCode=rdf$SpeciesCode, GVspeciesCode=rep("NA",nrow(rdf)), PredictionScore=rdf$PredictionScore, match="FALSEPOS", matchDelta=rep(9,nrow(rdf)))
	}else{
		# First add all the false negatives for species found in gdf and not in rdf
		spg<-unique(gdf$species); spr<-unique(rdf$SpeciesCode)
		fnsp<-spg[which(!spg %in% spr)]
		if(NROW(fnsp)>0){
			gfn<-subset(gdf,species %in% fnsp)
			mafn<-data.frame(PredictionId=rep(NA,nrow(gfn)), Second=gfn$secs, GVsecond=gfn$secs, SpeciesCode=gfn$species, GVspeciesCode=gfn$species, PredictionScore=rep(0,nrow(gfn)), match="FALSENEG", matchDelta=rep(8,nrow(gfn)))
		}
		
		# Now do the matches by clip
		madf<-ldply(rdf$Second,function(ss,rdf,gdf){
					srdf<-subset(rdf,Second==ss); spcd<-srdf$SpeciesCode
					# Symmetric match for first second, NOT symmetric for 2 or 3 seconds off, so that the expert might have marked the starting second of the bird call, but the machine predicted for a section 1-3 seconds later
					# symmetric for the first second in case gv timing of model and expert are a bit off.
					ssq1u<-c(ss-1); ssq1u<-subset(ssq1u,ssq1u >= 0 & ssq1u <= 60) #breaking into up and down so we can track the matching gv second to avoid double-dipping: two predictions on the same gv detection
					ssq1d<-c(ss+1); ssq1d<-subset(ssq1d,ssq1d >= 0 & ssq1d <= 60)
					ssq2<-c(ss-2); ssq2<-subset(ssq2,ssq2 >= 0 & ssq2 <= 60) #,ss+2 
					ssq3<-c(ss-3); ssq3<-subset(ssq3,ssq3 >= 0 & ssq3 <= 60) #,ss+3
					sgdf<-subset(gdf,secs==ss); flg<-0
					if(nrow(sgdf)>0){	#match on the second
						gspcd<-sgdf$species; tempmatch<-gspcd[1]
						if(spcd %in% gspcd){
							tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=0)
							flg<-1
						}
					}
					if(flg==0 & NROW(ssq1u)>0){	#No match at 0, try 1u
						sgdf<-subset(gdf,secs %in% ssq1u)  ##OR.. should we only use the case where gdf secs < rdf secs??
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-1, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=1)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq1d)>0){	#No match at 0, try 1d
						sgdf<-subset(gdf,secs %in% ssq1d)  ##OR.. should we only use the case where gdf secs < rdf secs??
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss+1, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=1)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq2)>0){	#No match at 1, try 2
						sgdf<-subset(gdf,secs %in% ssq2)
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-2, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=2)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq3)>0){	#No match at 2, try 3
						sgdf<-subset(gdf,secs %in% ssq3)
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-3, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=3)
								flg<-1
							}
						}
					}
					if(flg==0){	#no match at 3, no match so rdf value is a false positive
						tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=srdf$Second, GVsecond=-9, SpeciesCode=spcd, GVspeciesCode="NA", PredictionScore=srdf$PredictionScore, match="FALSEPOS", matchDelta=9)
					}
					return(tmdf)
				}, rdf=rdf, gdf=gdf)
		if(NROW(fnsp)>0){
			madf<-rbind(madf,mafn)
		}
	}
	#Before returning madf, need to scoop out and delete the double-dips
	if(sum(madf$match=="TRUEPOS")>0){madf<-removeDoubleMatches(madf)}
	
	return(madf)
}

# This function relables matches to ensure two model predictions do not match the same GV detection
# So it applies only to TRUEPOS matches, and it is called by the getMatches function
# madf is madf from the getMatches function
removeDoubleMatches<-function(madf){
	dfp<-subset(madf,match=="TRUEPOS")
	dfp<-dfp[order(dfp$Second),]
	fixrows<-integer()
	for(rr in 1:nrow(dfp)){
		##########################
		## Allowing for max 2 detections per GV detection, so two consecutive predictions can be for the same GV, due to imprecision in the timing of GV detections
		#if(rr+1 <= nrow(dfp)){
		#	if(dfp[rr,"GVsecond"]==dfp[rr+1,"GVsecond"]){
		#		remrows<-c(remrows,rr+2)
		#	}
		#}
		
		if(rr+2 <= nrow(dfp)){
			if(dfp[rr,"GVsecond"]==dfp[rr+2,"GVsecond"]){
				fixrows<-c(fixrows,rr+1)
			}
		}
		
		if(rr+3 <= nrow(dfp)){
			if(dfp[rr,"GVsecond"]==dfp[rr+3,"GVsecond"]){
				fixrows<-c(fixrows,rr+1)
			}
		}
		
	}
	if(NROW(fixrows)>0){
		madf[fixrows,"GVsecond"]<- -9
		madf[fixrows,"match"]<-"FALSEPOS"
		madf[fixrows,"matchDelta"]<- 9
	}
	
	return(madf)
}

gvedf<-subset(gvedf,!is.na(call)); nrow(gvedf)  

gvedf$spcall<-paste0(toupper(gvedf$species),"::",toupper(gvedf$call))
## Now we need the species and calls selected for training:
roicalls<-unique(roidf[,c("birdcode","songtype")])
roicalls$spcall<-paste0(toupper(roicalls$birdcode),"::",toupper(roicalls$songtype))
gvedf<-subset(gvedf,spcall %in% roicalls$spcall)

## do we have enough GV data to assess the models?
enoughLim<-40
numdf<-aggregate(secs~species,gvedf,NROW); names(numdf)<-c("species","count")
limspdf<-subset(numdf,count >= enoughLim)$species  
## For now not doing this...
#speciesmodeled<-limspdf$species
##instead
speciesmodeled<-numdf$species
NROW(speciesmodeled)
gvedf<-subset(gvedf,species %in% speciesmodeled)
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)
events<-unique(gvedf$SamplingEvent)

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)

tm<-Sys.time()
gvmatchlist<-llply(c(1:3),function(nn,gvdflst){
			tst<-gvdflst[[nn]]
			gvmatch<-getModelHurdledMatches(data=tst,gvedf=gvedf,hurdvals=hurdvals)
			return(gvmatch)
		},gvdflst=gvdflst)
Sys.time()-tm
gvmatches<-as.data.frame(rbindlist(gvmatchlist))
gvmatches$match<-as.character(gvmatches$match)
unique(gvmatches$match)
#all match NAs are FALSEPOS, matchDelta 9, so...
gvmatches$match<-ifelse(is.na(gvmatches$match),"FALSEPOS",gvmatches$match)

save(gvmatches, file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/noPretrainPerformance.RData")

########################################
## Now the roi matches
roidata<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_201109.csv", stringsAsFactors=FALSE)

roidata<-roidata[,c("filename","site","device","year","month","day","hour","min","birdcode","songtype","vote","type","method","temporaldup","roicenter","roicenterround")]
roidata$event<-sapply(roidata$filename,function(x){substr(x,1,32)})
roidata<-roidata[,c("event","birdcode","roicenterround","site","device","year","month","hour","songtype","vote","type","method","temporaldup","roicenter")]
names(roidata)<-gsub("type","assessType",names(roidata))
#names(roidata)<-gsub("event","SamplingEvent",names(roidata))
#names(roidata)<-gsub("birdcode","SpeciesCode",names(roidata))
roidata$roiId<-1:nrow(roidata)

## Need a function to retrieve for each label, positive and negative:
getBestMatch<-function(labeldf, preddf, models){  
	## Given the species, event, and rounded second, find the best matching second in the predictions for each model
	##	1) if there is a prediction match at that second or the previous second or the next second --> TP
	##  2) Otherwise, take the prediction with the highest score and generate FP and FN reports
	##  3) If no model prediction -> just FN
	## We take [event, birdcode, roicenterround], site, device, year, month, hour, songtype, vote, type(assessType), method, temporaldup, roicenter
	## We add: [SamplingEvent, SpeciesCode, Second], PredictionId, PredictionScore, Modelmatch
	evt<-labeldf$event
	sec<-labeldf$roicenterround; secp1<-sec+1; secm1<-sec-1
	if(secp1>59){secp1<-NA}; if(secm1<1){secm1<-NA}
	secs<-c(secm1,sec,secp1);secs<-subset(secs,!is.na(secs))
	vot<-labeldf$vote
	bcd<-labeldf$birdcode
	
	## this sec
	tdf<-subset(preddf,SamplingEvent==evt & Second %in% secs)
	## tdf can be nrow=0 because no model predicted for the set of secs in secs
	if(nrow(tdf)==0){ #if no predictions....
		rdf<-data.frame()
		for(mm in models){
			ldf<-labeldf
			if(vot=="present"){  #No predictions for a presence is a false neg
				ldf$match<-"FALSENEG"
				ldf$PredictionId<-0
				ldf$PredictionScore<-10^7
				ldf$Model<-mm
				ldf$matchDelta<-8
			}else{ #otherwise no predictions for a not present = true negative
				ldf$match<-"TRUENEG"
				ldf$PredictionId<-0
				ldf$PredictionScore<-10^7
				ldf$Model<-mm
				ldf$matchDelta<-5
			}
			rdf<-rbind(rdf,ldf)
		}
	}else{## else... there are predictions
		rdf<-data.frame()
		for(mm in models){
			ldf<-labeldf
			mdf<-subset(tdf,ModelName==mm)
			if(nrow(mdf)==0){
				if(vot=="present"){ #The ROI was not predicted by the model mm
					ldf$match<-"FALSENEG"
					ldf$PredictionId<-0
					ldf$PredictionScore<-10^7
					ldf$Model<-mm 
					ldf$matchDelta<-8
				}else if(nrow(mdf)==0 & vot=="not present"){ #An absent was not predicted by model mm, true negative
					ldf$match<-"TRUENEG"
					ldf$PredictionId<-0
					ldf$PredictionScore<-10^7
					ldf$Model<-mm
					ldf$matchDelta<-5
				}
			}else{ #Model mm has predictions. Is any of the predictions a match for bcd?
				sdf<-subset(mdf,SpeciesCode==bcd)
				if(nrow(sdf)==0){
					if(vot=="present"){  #there are predictions for model mm but none is a match for a presence
						## The max of these should thus be a false positive, and the roi should be a false negative
						#Take the highest score from mdf and create FP for the highest score, and FN for theROI
						mmdf<-subset(mdf,PredictionScore==max(PredictionScore))
						if(nrow(mmdf)>1){
							if(sec %in% mmdf$Second){
								mmdf<-subset(mmdf,Second==sec)
							}else{mmdf<-mmdf[1,]}
						}
						ldffp<-ldf; ldffn<-ldf
						
						ldffp$match<-"FALSEPOS"
						ldffp$PredictionId<-mmdf$PredictionId
						ldffp$PredictionScore<-mmdf$PredictionScore
						ldffp$birdcode<-mmdf$SpeciesCode
						ldffp$Model<-mm
						ldffp$matchDelta<-9
						
						ldffn$match<-"FALSENEG"
						ldffn$PredictionId<-0
						ldffn$PredictionScore<-10^7
						ldffn$Model<-mm
						ldffn$matchDelta<-8
						
						ldf<-rbind(ldffp,ldffn)
					}else if(nrow(sdf)==0 & vot=="not present"){  #there are predictions for model mm but none is a match for a non-presence
						ldf$match<-"TRUENEG"
						ldf$PredictionId<-0
						ldf$PredictionScore<-10^7
						ldf$Model<-mm
						ldf$matchDelta<-5
					}
				}else{ #there is a prediction for model mm and of the right species code, but for present or not present?
					sdf<-subset(sdf,PredictionScore==max(PredictionScore))
					if(nrow(sdf)>1){
						if(sec %in% sdf$Second){
							sdf<-subset(sdf,Second==sec)
						}else{sdf<-sdf[1,]}
					}
					if(vot=="present"){
						ldf$match<-"TRUEPOS"
						ldf$PredictionId<-sdf$PredictionId
						ldf$PredictionScore<-sdf$PredictionScore
						ldf$Model<-mm
						ldf$matchDelta<-1
					}else{  #vot=="not present"
						#a matching prediction for a not-present is a FP... or is it? Set to matchDelta==7
						ldf$match<-"FALSEPOS"
						ldf$PredictionId<-sdf$PredictionId
						ldf$PredictionScore<-sdf$PredictionScore
						ldf$Model<-mm
						ldf$matchDelta<-7	
					}
				}
			}
			rdf<-rbind(rdf,ldf)
		}
	}
	return(rdf)
	
}

## Loop through roidata into a new data.frame...
library(doParallel)
nodes<-detectCores()
cl<-makeCluster(nodes)
registerDoParallel(cl)

tm<-Sys.time()
resdf<-ldply(.data=1:3,.fun=function(dd,roidata,spmodel,roidflst,getBestMatch){
			labelPreds<-roidflst[[dd]]
			model<-unique(labelPreds$ModelName)[1]
			species<-subset(spmodel,ModelName==model)$SpeciesCode
			subroi<-subset(roidata,birdcode %in% species)
			nrd<-nrow(subroi)
			mdldf<-ldply(.data=1:nrd,.parallel=TRUE,.fun=function(rr,subroi,labelPreds,model,getBestMatch){ #
						if((rr/1000)==round(rr/1000)){print(rr)}
						labeldf<-subroi[rr,]
						rtdf<-getBestMatch(labeldf=labeldf,preddf=labelPreds,models=model)
						return(rtdf)
					},subroi=subroi,labelPreds=labelPreds,model=model,getBestMatch=getBestMatch)
			
			return(mdldf)
		},roidata=roidata,spmodel=spmodel,roidflst=roidflst,getBestMatch=getBestMatch)
Sys.time()-tm
names(resdf)<-gsub("Model","ModelName",names(resdf))

roimatchesNoPretrain<-resdf
gvmatchesNoPretrain<-gvmatches
save(roimatchesNoPretrain,gvmatchesNoPretrain,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/noPretrainPerformance.RData")  #roi_label_matches.RData is the old file







