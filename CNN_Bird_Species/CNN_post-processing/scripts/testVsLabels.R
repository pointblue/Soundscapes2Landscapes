# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## We want to obtain all the true positives, false positives and false negatives from the labeled data
## For each present/not present label, take the 2-second window that most closely matches the center of the ROI (i.e., the rounded second)
## Then collect the true presences when the prediction matches the label, or the false positive and false negative when there is a mismatch
## Finally, save the data for logistic analyses

## We read the labeled data
## Then we download the prediction from all six models from the s2l_devel database

#libs<-c("ggplot2","plyr","RMySQL","data.table")
#lapply(libs, require, character.only = TRUE)
library(plyr)

roidata<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/pattern_matching_ROIs_centers_201109.csv", stringsAsFactors=FALSE)
#roidata<-read.csv("/home/ubuntu/pattern_matching_ROIs_centers_201109.csv", stringsAsFactors=FALSE)

## Load the labelPreds data. It is the entire events where a label was found
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/labeledDataPredictions.RData")
#load(file="/home/ubuntu/labeledDataPredictions.RData")


roidata<-roidata[,c("filename","site","device","year","month","day","hour","min","birdcode","songtype","vote","type","method","temporaldup","roicenter","roicenterround")]
roidata$event<-sapply(roidata$filename,function(x){substr(x,1,32)})
roidata<-roidata[,c("event","birdcode","roicenterround","site","device","year","month","hour","songtype","vote","type","method","temporaldup","roicenter")]
names(roidata)<-gsub("type","assessType",names(roidata))
#names(roidata)<-gsub("event","SamplingEvent",names(roidata))
#names(roidata)<-gsub("birdcode","SpeciesCode",names(roidata))
roidata$roiId<-1:nrow(roidata)


########################################
## Need these functions:
# This function retrieves all the data from all models and outputs for each wav file in the GV table
## Query sintax builder, but only for NEW set of models. If using old database, then PR.PredictionScore becomes PR.SoftMaxScore
makeQuery<-function(event, dbver){
	predscore<-ifelse(dbver=="s2l_test","PR.PredictionScore","PR.SoftMaxScore")
	event<-as.character(event)
	site<-substr(event,1,15); yearv<-as.integer(substr(event,19,20)); monthv<-as.integer(substr(event,22,23)); dayv<-as.integer(substr(event,25,26))
	hourv<-as.integer(substr(event,28,29)); minutev<-as.integer(substr(event,31,32))
	sqltxt<-paste0("SELECT PR.PredictionId, PR.Second, PR.SpeciesCode, PR.PredictionsDatasetId, ",predscore," AS PredictionScore, AF.AudiofileId, ",
			"CONCAT(AF.SiteName,'_20',AF.Year, '-', LPAD(AF.Month, 2, '0'), '-', LPAD(AF.Day, 2, '0'), '_', LPAD(AF.Hour, 2, '0'), '-', LPAD(AF.Minute, 2, '0')) AS SamplingEvent ",
			"FROM predictions AS PR INNER JOIN audiofiles AS AF ON (PR.audiofileid = AF.audiofileid) WHERE ",
			"EXISTS (SELECT 1 FROM audiofiles AS audiofiles1 WHERE (audiofiles1.SiteName = '",site,"') AND ",
			"(audiofiles1.Year = '",yearv,"') AND (audiofiles1.Month = '",monthv,"') AND (audiofiles1.Day = '",dayv,"') AND ",
			"(audiofiles1.Hour = '",hourv,"') AND (audiofiles1.Minute = '",minutev,"') AND (AF.audiofileid = audiofiles1.AudiofileId))")
	return(sqltxt)
}

# This function lists all the unique files in the GV dataset and queries the data for all of them
getROIpredictions<-function(events,speciesmodeled,dbver="s2l_test"){
	
	#dimension output
	tdf<-data.frame(PredictionId=rep(999999,59),Second=rep(99,59),SpeciesCode=rep("XXXX",59),PredictionsDatasetId=rep(9,59),PredictionScore=rep(9999999,59),AudiofileId=rep(123456,59),SamplingEvent=rep("s2lamXXX_100101_0000-00-00_00-00",59))
	reslst<-list();for(rr in 1:NROW(events)){reslst[[rr]]<-tdf}
	
	# make the connection and query
	con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = dbver)
	
	for(rr in events){
		sqltxt<-makeQuery(rr, dbver)
		preddf<-dbGetQuery(con,sqltxt)
		if(nrow(preddf)>0){
			reslst[[rr]]<-preddf
		}
	}
	
	## Add information about the model...
	if(dbver=="s2l_test"){
		predinfo<-dbGetQuery(con,"select MD.ModelName, PD.PredictionsDatasetId, PD.PredictionScoreType from models as MD inner join predictions_datasets as PD on (MD.ModelId = PD.ModelId)")
		predinfo$modelVersion<-"Pre-trained 3 models"
	}else{
		predinfo<-dbGetQuery(con,"select CONCAT(MD.ModelName,'_',MD.Epochs,'_',MD.PreTrainData) as ModelName, PD.PredictionsDatasetId from models as MD inner join predictions_datasets as PD on (MD.ModelId = PD.ModelId)")	
		predinfo$PredictionScoreType<-"softmax"
		predinfo$modelVersion<-"No pre-train 6 models"
	}
	
	dbDisconnect(con)
	dbdf<-rbindlist(reslst)
	dbdf<-subset(dbdf,SpeciesCode != "XXXX")
	dbdf<-subset(dbdf,SpeciesCode %in% speciesmodeled)
	dbdf<-merge(dbdf,predinfo,by="PredictionsDatasetId",all.x=TRUE)
	
	return(dbdf)
}

#######################################
## Get the list of unique events from the roidata list: "s2lamXXX_100101_0000-00-00_00-00"
## Get the list of species modeled from the roidata iist
#speciesmodeled<-unique(roidata$birdcode)
#events<-unique(roidata$event)
#labelPreds<-getROIpredictions(events=events,speciesmodeled=speciesmodeled)
#save(labelPreds,file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/labeledDataPredictions.RData")
#######################################


labelPreds$ModelName<-sapply(labelPreds$ModelName,function(x){
			if(x=="cols_54cls_mobnet_full_finetune"){
				mn<-"MobileNet"
			}else{
				mn<-substr(x,12,nchar(x))
			}
			return(mn)
		})
labelPreds$Model<-paste0(labelPreds$ModelName,"::",labelPreds$PredictionScoreType)
labelPreds<-labelPreds[,c("SamplingEvent","SpeciesCode","Second","PredictionId","PredictionScore","Model")]


## List of models:
models<-unique(labelPreds$Model)

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
			mdf<-subset(tdf,Model==mm)
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

nrd<-nrow(roidata)
tm<-Sys.time()
resdf<-ldply(.data=1:nrd,.parallel=TRUE,.fun=function(rr,roidata,labelPreds,models,getBestMatch){
			if((rr/1000)==round(rr/1000)){print(rr)}
			labeldf<-roidata[rr,]
			rtdf<-getBestMatch(labeldf=labeldf,preddf=labelPreds,models=models)
			if(ncol(rtdf)>20){stop(paste("in row",rr,"column 21 appeared"))}
			if(is.na(rtdf$event) | is.na(rtdf$roiId)){stop(paste("in row",rr,"NAs appear"))}
			return(rtdf)
		},roidata=roidata,labelPreds=labelPreds,models=models,getBestMatch=getBestMatch)
Sys.time()-tm

#save(resdf,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/roilabelmatch.RData")  #roi_label_matches.RData is the old file
save(resdf,file="/home/ubuntu/roilabelmatch.RData")
