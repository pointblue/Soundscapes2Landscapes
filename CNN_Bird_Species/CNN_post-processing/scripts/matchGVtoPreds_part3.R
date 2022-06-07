# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file generates the dataset for logistic regressions to find more optimal solutions to hurdling that maximizes TP (ie, fewer FN), and minimizes FP
## For the sake of efficiency, since the ROI data provides lots of TP, we shall plot FP to FN for each hurdle for each model

## Each species is different: one or more models may work well, so the first question, based on the GV analyses, is to determine which model to use for each species
## The second step is to fit the logistic regression using a stepwise model, and then decide for which species we use the logistic correction, and for which we stick to a high hurdle
## In matchGVtoPreds_part4.R we construct a function based on what is discovered here to correct all the data

#Ignore this for now...
## There are 55 species with ROI data, but one has only 95 rois, so 54
## There are 47 species with GV data, but only 37 with at least 40 detections in the GV set
## Species for which there is not enough GV data (<40 detections):
## ATFL, BTPI, BRCR, BUOR, LAZB, NOFL, NUWO, PIWO, RSHA, SWTH
## Species for which there is no GV data
## ANHU BUSH GHOW GRSP HETH HOWR HUVI OSFL
## Species for which there is not enough ROI data
## BUSH

## An important output of this script is the GV results after logistic to plot for CNN paper
library(plyr); library(ggplot2); library(RMySQL)

## Load and prep the data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/roilabelmatch.RData")
nrow(resdf)
resdf$PredictionScore<-ifelse(resdf$PredictionScore==0,10000000,resdf$PredictionScore)
#resdf<-subset(resdf,vote=="present")

#remove dupes
listRoiIDdupes<-function(df){
	duprois<-df$roiId
	## Work with the highest prediction score
	mdf<-subset(df,PredictionScore==max(PredictionScore))
	if(NROW(unique(mdf$birdcode))==1){ ## If only one bird code among max predscores take the lower roiId
		keeproi<-min(mdf$roiId)
	}else{ 
		#more than one bird code, matching prediction score = max
		#take the first correct match if available
		mmdf<-subset(mdf,match=="TRUEPOS")
		if(nrow(mmdf)>0){
			keeproi<-mmdf$roiId; keeproi<-keeproi[1]
		}else{
			#no TRUEPOS - just take the first record
			keeproi<-min(mdf$roiId)
		}
	}
	remrois<-duprois[which(duprois!=keeproi)]
	return(remrois)
}
resdupes<-subset(resdf,temporaldup=="Y")
resdupes$dupId<-paste0(resdupes$event,"::",resdupes$roicenterround,"::",resdupes$Model)
remroivals<-sapply(unique(resdupes$dupId),function(dd,resdupes,listRoiIDdupes){
			df<-subset(resdupes,dupId==dd)
			rrv<-listRoiIDdupes(df)
			return(rrv)
		},resdupes=resdupes,listRoiIDdupes=listRoiIDdupes)
remroivals<-as.numeric(unlist(remroivals))
resdf<-subset(resdf,!roiId %in% remroivals)

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)


#Now let's use precision at maxF0.5 to select the best model for each species
sigdf<-subset(resdf,!grepl("softmax",Model))

summarizeLabeledToSampleBySpecies<-function(matches,hurdles){
	hurdsample<-ldply(unique(hurdles),function(hh,matches){
				hv<-hh*(10^7)
				hmatches<-subset(matches, PredictionScore>=hv)
				sumToSample<-data.frame()
				for(mm in unique(hmatches$Model)){
					modeldf<-subset(hmatches,Model==mm)
					sumToSpecies<-ldply(unique(modeldf$birdcode),function(ss,modeldf){
								speciesdf<-subset(modeldf,birdcode==ss)
								trval<-sum(speciesdf$match=="TRUEPOS")
								fnval<-sum(speciesdf$match=="FALSENEG")
								fpval<-sum(speciesdf$match=="FALSEPOS")
								f1val<-trval/((trval*2)+fnval+fpval)
								sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
								prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
								miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
								fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
								specdf<-data.frame(count=nrow(speciesdf),truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
								specdf$birdcode<-ss
								return(specdf)
							},modeldf=modeldf)
					sumToSpecies$Model<-mm
					sumToSample<-rbind(sumToSample,sumToSpecies)
				}
				sumToSample$Threshold<-hh
				return(sumToSample)
		},matches=matches)
	return(hurdsample)
}
sproidf<-summarizeLabeledToSampleBySpecies(matches=sigdf,hurdles=hurdvals)

getFval<-function(df,beta){
	fv<-(1+(beta^2))*(df$Prec*df$Sens)/(((beta^2)*df$Prec) + df$Sens)
	return(fv)
}

sproidf$F05v<-sapply(1:nrow(sproidf),function(rr,sproidf,getFval){
			df<-sproidf[rr,]
			fval<-getFval(df=df,beta=0.5)
			return(fval)
		},sproidf=sproidf,getFval=getFval)

f05maxdf<-aggregate(F05v~Model+birdcode,sproidf,max)
f05maxdf$Prec05<-sapply(1:nrow(f05maxdf),function(rr,f05maxdf,sproidf){
			mdl<-f05maxdf[rr,"Model"];spp<-f05maxdf[rr,"birdcode"];f05val<-f05maxdf[rr,"F05v"]
			tdf<-subset(sproidf,Model==mdl & birdcode==spp & F05v==f05val)
			prv<-max(tdf$Prec)
			return(prv)
		},f05maxdf=f05maxdf,sproidf=sproidf)

## spmodel is the table of species and best model at threshold 0.9 and maxF0.5 accuracy
spmodel<-ldply(unique(f05maxdf$birdcode),function(ss,f05maxdf){
			tdf<-subset(f05maxdf,birdcode==ss); mp<-max(tdf$Prec05)
			tdf<-subset(tdf,Prec05==mp)
			return(tdf[1,])
		},f05maxdf=f05maxdf)

save(spmodel,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData")
##########################################################################################################################################
## Choosing threshold 0.65 - SKIP
## We may want to use a general threshold for all species to apply the logistic model
## First, let's use the GV data to evalaute the miss rate vs threshold, see if all species agree on some value
## Load the data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/allmaches_65to99999.RData")
allmatches<-subset(allmatches,modelVersion=="Pre-trained 3 models")
allmatches$Model<-sapply(allmatches$ModelName,function(mm){
			ncm<-nchar(mm);nmn<-substr(mm,12,ncm)
			nmn<-ifelse(grepl("mobnet",nmn),"MobileNet",nmn)
		})
allmatches$Model<-paste0(allmatches$Model,"::",allmatches$PredictionScoreType)


## Need a function for summary by species
summarizeToSampleAllSpecies<-function(matches,spmodel){
	hurdsample<-ldply(unique(matches$hurdle),function(hh,matches,spmodel){
				hv<-as.character(hh)
				hmatches<-subset(matches, hurdle==hv)
				sumToSpecies<-ldply(unique(hmatches$SpeciesCode),function(ss,hmatches,spmodel){
							smatches<-subset(hmatches,SpeciesCode==ss)
							spmdl<-subset(spmodel,birdcode==ss)$Model
							modeldf<-subset(smatches,Model==spmdl)
							d0val<-sum(modeldf$matchDelta==0); d1val<-sum(modeldf$matchDelta==1); d2val<-sum(modeldf$matchDelta==2); d3val<-sum(modeldf$matchDelta==3)
							trval<-sum(d0val,d1val,d2val,d3val)
							fnval<-sum(modeldf$matchDelta==8)
							fpval<-sum(modeldf$matchDelta==9)
							f1val<-trval/((trval*2)+fnval+fpval)
							sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
							prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
							miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
							fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
							specdf<-data.frame(count=nrow(modeldf),d0count=d0val, d1count=d1val, d2count=d2val, d3count=d3val,
									truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
							specdf$Model<-spmdl	
							specdf$birdcode<-ss
							return(specdf)
						},hmatches=hmatches,spmodel=spmodel)
				sumToSpecies$Threshold<-hh
				return(sumToSpecies)
			},matches=matches,spmodel=spmodel)
	return(hurdsample)
}

########################
## Make table of metrics per species: kappa, total acc, AUC
metrics<-summarizeToSampleAllSpecies(matches=allmatches,spmodel=spmodel)
bplotdf<-subset(metrics,Threshold<=0.99)
bplotdf$highlight<-ifelse(bplotdf$Threshold==0.9,"No","Yes")
p1<-ggplot(data=bplotdf,aes(x=as.character(Threshold),y=Miss, fill=highlight, alpha=highlight)) + geom_hline(yintercept=0.5,size=2,color="red") + geom_boxplot() +
		scale_fill_manual(values=c("#69b3a2", "grey")) + scale_alpha_manual(values=c(0.5,0.8)) +  theme(legend.position = "none") +
		labs(x="", y="Miss rate")
p2<-ggplot(data=bplotdf,aes(x=as.character(Threshold), y=FPper, fill=highlight, alpha=highlight)) + geom_hline(yintercept=0.2,size=2,color="red") + geom_boxplot() +
		scale_fill_manual(values=c("#69b3a2", "grey")) + scale_alpha_manual(values=c(0.5,0.8)) +  theme(legend.position = "none") +
		labs(x="", y="Percent False Positive")

p1;dev.new();p2

### CHOICE: we use the 0.9 threshold to build the data for the logistic models
##########################################################################################################################################

## Now we generate the logistic set for each species
library(dplyr)
thr<-0.65
resdf<-subset(resdf,birdcode!="BUSH")	#No data for bushtit

## lgset is a list of tables, one per species, with the TP and FP records
lgset<-llply(unique(resdf$birdcode),function(ss,resdf,spmodel,thr){
			mdl<-subset(spmodel,birdcode==ss)$Model
			spdf<-subset(resdf,birdcode==ss & Model==mdl)
			spdf<-subset(spdf,PredictionScore>=(thr*10^7))
			spdf<-spdf[which(!names(spdf) %in% c("site","device","year","month","hour","songassessType","vote","assessType","method","temporaldup","roicenter"))]
			
			## If number of TP is within 0.8-1.2 number of FP, take as is
			ntp<-sum(spdf$match=="TRUEPOS"); nfp<-sum(spdf$match=="FALSEPOS")
			if(nfp>0 & ntp>0){
				if((ntp/nfp)<1.2 & (ntp/nfp)>0.8){
					rsp<-subset(spdf,match %in% c("TRUEPOS","FALSEPOS"))
				}else{
					if(nfp<ntp){	#fewer falsepos, sample truepos
						rsp<-subset(spdf,match=="FALSEPOS")
						stp<-sample_n(subset(spdf,match=="TRUEPOS"),size=nfp)
						rsp<-rbind(rsp,stp)
					}else{		#fewer truepos, sample falsepos
						rsp<-subset(spdf,match=="TRUEPOS")
						sfp<-sample_n(subset(spdf,match=="FALSEPOS"),size=ntp)
						rsp<-rbind(rsp,sfp)
					}
				}
			}else{
				rsp<-data.frame()
			}
			
			nrec<-nrow(rsp)
			print(paste(ss,nrec,ntp))
			return(rsp)
		},resdf=resdf,spmodel=spmodel,thr=thr)
names(lgset)<-unique(resdf$birdcode)

getMinCount<-function(lgset,mincount){
	minc<-character()
	for(ii in 1:NROW(lgset)){
		if(nrow(lgset[[ii]])<mincount){
			minc<-c(minc,names(lgset)[ii])
		}
	}
	return(minc)
}

getMinCount(lgset=lgset,mincount=50)


##################################
## Now we gather the covariate data for all the records in each of the species df...
## We need to know: 
## 1 - the number of times the species was detected in the event
## 2 - the number of times all other species were detected in the event
## 3 - the total number of species detected in the event
## 4 - the total number of detections of the species in the 3 prior and 3 posterior minutes

## So we need the AudiofileId for the event, and for the 3 prior and 3 posterior, in one query, by: SiteName Year Month Day Hour Minute
## We create a table with these values
## We retrieve all predictions for the event to answer 1-3
## We query specifically for the total number of predictions of species X in AudiofileIds a-g for question 4 (select count(*) from thistable where species=S and audiofileid in (...))

## Remember this
#modelIddf<-data.frame(Model=c("Resnet50::sigmoid","Resnet101::sigmoid","MobileNet::sigmoid"),PredictionsDatasetId=c(2,4,6))

## Gather all the unique events in the lgset
lgevents<-sapply(1:NROW(lgset),function(ss,lgset){
			df<-lgset[[ss]]; ev<-unique(df$event)
			return(ev)
		},lgset=lgset)
lgevents<-unlist(lgevents)
lgevents<-unique(lgevents)

## Get the AudiofileIds for these unique events - takes 1.5 hrs
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
eventIddf<-ldply(lgevents,function(ee,con){
			site<-substr(ee,1,15)
			yr<-substr(ee,19,20)
			mn<-substr(ee,22,23)
			dy<-substr(ee,25,26)
			hr<-substr(ee,28,29)
			mt<-substr(ee,31,32)
			dt<-paste0("20",yr,"-",mn,"-",dy," ",hr,":",mt,":00")
			afq<-paste0("select AudiofileId from audiofiles where SiteName='",site,"' AND ",
					"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') <= DATE_ADD('",dt,"', INTERVAL 30 MINUTE) AND ",
					"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') >= DATE_SUB('",dt,"', INTERVAL 30 MINUTE)")
			afid<-dbGetQuery(con,afq)
			afe<-paste0("select AudiofileId from audiofiles where SiteName='",site,"' and Year=",yr," and Month=",mn," and Day=",dy," and Hour=",hr," and Minute=",mt)
			afeid<-dbGetQuery(con,afe)
			#covert into data.frame
			afid$event<-ee;afid$isevent<-ifelse(afid$AudiofileId==afeid$AudiofileId,1,0);afid$eventAfid<-afeid$AudiofileId
			return(afid)
		},con=con)
dbDisconnect(con)
Sys.time()-tm

### Now retrieve the number of individuals per species, number of species, and count of target species in isevent==1
### For this we need to know which are the target species for the event. 
### Let's start getting all the target species for an event
targetdf<-ldply(1:NROW(lgset),function(ss,lgset){
			df<-lgset[[ss]]
			if(nrow(df)>0){
				df<-unique(df[,c("event","birdcode")])
			}
			return(df)
		}, lgset=lgset)

## tragetdf is the list of unique species-events
## Then for each event get the species and counts detected for that species
eventIsEvent<-subset(eventIddf,isevent==1)
## add the AudiofileId to the list of events in targetdf using eventIsEvent, then species and PredictionsDatasetlId to use to retrieve predictions
targetdf<-merge(targetdf,eventIsEvent,all.x=TRUE)
targetdf<-merge(targetdf,spmodel[,c("birdcode","Model")],by="birdcode",all.x=TRUE)
targetdf$PredictionsDatasetId<-ifelse(targetdf$Model=="Resnet50::sigmoid",2,ifelse(targetdf$Model=="Resnet101::sigmoid",4,6))
sppdf<-data.frame(SpeciesCode=names(lgset))
hurdval<-0.9*10^7

#tm<-Sys.time()			#About 19 min   COLLAPSING INTO ONE FUNCTION BELOW
#con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
#countsdf<-ldply(1:nrow(targetdf),function(dd,targetdf,sppdf,con,hurdval){ 
#			afid<-as.numeric(targetdf[dd,"AudiofileId"])
#			pdid<-as.numeric(targetdf[dd,"PredictionsDatasetId"])
#			spcd<-as.character(targetdf[dd,"birdcode"])
#			rdf<-targetdf[dd,c("birdcode","event","AudiofileId","Model","PredictionsDatasetId")]
#			qdf<-dbGetQuery(con,paste0("select count(*) as count from predictions where AudiofileId = ",afid," and PredictionScore >= ",hurdval," and PredictionsDatasetId = ",pdid," and SpeciesCode = '",spcd,"'"))
#			rdf$count<-as.numeric(qdf$count[1])
#			return(rdf)
#		},targetdf=targetdf,sppdf=sppdf,con=con,hurdval=hurdval)
#dbDisconnect(con)
#Sys.time()-tm

### Finally, do a select count for the target species and others in all the audiofileIds for the event. About 40 minutes
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
logistdf<-ldply(1:nrow(targetdf),function(dd,targetdf,sppdf,con,hurdval,eventIddf){
			afv<-as.numeric(targetdf[dd,"AudiofileId"])
			pdid<-as.numeric(targetdf[dd,"PredictionsDatasetId"])
			spcd<-as.character(targetdf[dd,"birdcode"])
			rdf<-targetdf[dd,c("birdcode","event","AudiofileId","Model","PredictionsDatasetId")]
			
			afid<-subset(eventIddf,eventAfid==afv)$AudiofileId;afid<-paste(afid,collapse=",")
			cdf<-dbGetQuery(con,paste0("select count(*) as count from predictions where AudiofileId = ",afv," and PredictionScore >= ",hurdval," and PredictionsDatasetId = ",pdid," and SpeciesCode = '",spcd,"'"))
			rdf$count<-as.numeric(cdf$count[1])
			
			qdf<-dbGetQuery(con,paste0("select SpeciesCode,count(*) as Count from predictions where AudiofileId in (",afid,") and PredictionScore >= ",hurdval," and PredictionsDatasetId = ",pdid," GROUP BY SpeciesCode"))
			sdf<-merge(sppdf,qdf,by="SpeciesCode",all.x=TRUE)
			sdf$Count<-ifelse(is.na(sdf$Count),0,sdf$Count)
			tsdf<-as.data.frame(t(sdf$Count));names(tsdf)<-sdf$SpeciesCode
			tsdf$rich<-sum(sdf$Count>0)
			tsdf$nearCount<-tsdf[1,spcd]
			resdf<-cbind(rdf,tsdf)
			return(resdf)
		},targetdf=targetdf,sppdf=sppdf,con=con,hurdval=hurdval,eventIddf=eventIddf)
dbDisconnect(con)
Sys.time()-tm

## We have everything. Now we just need to add the response.
respdf<-ldply(1:NROW(lgset),function(nn,lgset){
			ndf<-lgset[[nn]]
			if(nrow(ndf)>0){
				ndf<-ndf[,c("event","birdcode","match")]
				ndf$binval<-ifelse(ndf$match=="TRUEPOS",1,0)
			}
			return(ndf)
		},lgset=lgset)

## respdf IS the data.frame we need
respdf<-merge(respdf,logistdf,by=c("event","birdcode"),all.x=TRUE)

save(eventIddf,targetdf,logistdf,respdf,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrData_fullHour065.RData")


################################
## Finally, we fit the logistic regression and evaluate the models
## We do that in _part4.R





#######################################################################################################################################################################

for(ss in unique(sproidf$birdcode)){
	spplot<-subset(sproidf,birdcode==ss)
	p1<-ggplot(spplot,aes(x=Miss, y=FPper)) + facet_wrap(~Model,ncol=3) + 
			geom_line(aes(color=Threshold),size=2) +
			scale_fill_brewer() + labs(x="Miss rate", y="False positive percent",color="Threshold",title=paste0("Species: ",ss)) +
			theme_bw()
	print(p1)
	readline(prompt="Press [enter] to continue")
}


## This variation is for the labeled data
summarizeLabeledToSampleAllSpecies<-function(matches,hurdles){
	hurdsample<-ldply(unique(hurdles),function(hh,matches){
				hv<-hh*(10^7)
				hmatches<-subset(matches, PredictionScore>=hv)
				sumToSample<-ldply(unique(hmatches$Model),function(mm,hmatches){
							modeldf<-subset(hmatches,Model==mm)
							trval<-sum(modeldf$match=="TRUEPOS")
							fnval<-sum(modeldf$match=="FALSENEG")
							fpval<-sum(modeldf$match=="FALSEPOS")
							f1val<-trval/((trval*2)+fnval+fpval)
							sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
							prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
							miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
							fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
							specdf<-data.frame(count=nrow(modeldf),truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
							specdf$Model<-mm
							return(specdf)
						},hmatches=hmatches)
				sumToSample$Threshold<-hh
				return(sumToSample)
			},matches=matches)
	return(hurdsample)
}

lmetsdf<-summarizeLabeledToSampleAllSpecies(matches=resdf,hurdles=hurdvals)
lmetsdf$Model<-ifelse(lmetsdf$Model=="MobileNet::softmax","Mobilenet::softmax",ifelse(lmetsdf$Model=="MobileNet::sigmoid","Mobilenet::sigmoid",lmetsdf$Model))

modelorder<-data.frame(Model=unique(lmetsdf$Model),morder=c(3,6,2,5,1,4))
labelroisp<-merge(lmetsdf,modelorder,by="Model", all.x=T)
labelroisp$Model<-as.factor(labelroisp$Model)
labelroisp$Model<-reorder(labelroisp$Model,labelroisp$morder)

## Plot each model's data FP vs FN by hurdle
p1<-ggplot(labelroisp,aes(x=Miss, y=FPper)) + facet_wrap(~Model,ncol=3) + 
		geom_line(aes(color=Threshold),size=2) +
		scale_fill_brewer() + labs(x="Miss rate", y="False positive percent",color="Threshold") +
		theme_bw()


		