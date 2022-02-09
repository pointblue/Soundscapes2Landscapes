# TODO: Add comment
# 
# Author: lsalas
###############################################################################

#### This file produces all the tables and results shown in the CNN paper - we will include in a notebook

## Data files:
# logisticCorrModels_fullHour065_predAdj658095_10262021.RData
# This file contains the GV results, and adjusted results after logistic correction using
# 65% threshold for the covariate data of the logistic model, and a 50% penalization of the logistic prediction

# BirdNET_GV_matches_06102021.RData
# This is the evaluation of the BirdNET predictions against the GV data.

# noPretrainGVPerformance_06072021.RData
# this is the evaluation of model predictions against GV (without logistic correction)

# Correction_FPdifference_minuteLevel.RData
# this compares the rate of FP between the logistic-corrected predictions vs not corrected at the minute level

## IN EACH CASE EXPLAIN WHY NOT USING ROI data???
## INDICATE FIGURE NUMBER

##########################

libs<-c("ggplot2","plyr","dplyr")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))
pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"

## Need the utility matching functions
source(paste0(pathToLocalGit,"GVanalyses/3models2outputs/scripts/predMatching_utils.R"))

## If using ROI data, use the files:
# c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/noPretrainROIPerformance_06102021.RData
# c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/WithPretrainROIPerformance_06102021.RData
## DO NOT use the no/With..PretrainROIPerformance_corrected_06222021.RData because these use all the RI data, not just the test ROIs

##########################
## PR curves

## Looping through hurdle value filters, then by model
load(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_predAdj658095_10262021.RData"))
hfilts<-names(gvpreadadjlst)
summdf<-ldply(hfilts,function(hnm,gvpreadadjlst,summarizeToSampleAllSpecies){
			hfdf<-gvpreadadjlst[[hnm]]
			mdldf<-ldply(unique(hfdf$Model),function(mdl,hfdf,summarizeToSampleAllSpecies){
						matches<-subset(hfdf,Model==mdl)
						adjmatches<-matches[,which(names(matches)!="match")]
						names(adjmatches)<-gsub("adjMatch","match",names(adjmatches))
						metricsraw<-summarizeByHurdle(allmatches=matches,bySpecies="no",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
						metricsraw$Treatment<-"Uncorrected"
						metricsadj<-summarizeByHurdle(allmatches=adjmatches,bySpecies="no",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
						metricsadj$Treatment<-"Corrected"
						mtdf<-rbind(metricsraw,metricsadj)
						mtdf$Model<-mdl
						return(mtdf)
					},hfdf=hfdf,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
			mdldf$PredictionFilter<-hnm
			return(mdldf)
		},gvpreadadjlst=gvpreadadjlst,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)

## Use only PredictionFilter==h65
prdf<-subset(summdf,PredictionFilter=="h65")

# need to add the BirdNET data
load(file=paste0(pathToLocalGit,"GVanalyses/BirdNet/data/BirdNET_GV_matches_06102021.RData"))
bndf<-summarizeByHurdle(allmatches=bngvmatches,bySpecies="no",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
bndf$Treatment<-"Uncorrected"; bndf$Model<-"BirdNET"
prplot<-rbind(prdf[,names(bndf)],bndf)

#need to make pretty... ONLY GV results included - not all species, not all records
# why not use the ROI data??
p1<-ggplot(prplot,aes(x=hurdle, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw() +
		facet_wrap(~Model, ncol=2)
p2<-ggplot(prplot,aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw() +
		facet_wrap(~Model, ncol=2)
print(p1)
dev.new();print(p2)

###########################
## F05 curves plot
### Construct the PR curve for 3 species: facet by species, curves are pretrained 3 models, dot shows the max
hfdf<-gvpreadadjlst[["h65"]]
mdldf<-ldply(unique(hfdf$Model),function(mdl,hfdf,summarizeToSampleAllSpecies){
			matches<-subset(hfdf,Model==mdl)
			adjmatches<-matches[,which(names(matches)!="match")]
			names(adjmatches)<-gsub("adjMatch","match",names(adjmatches))
			metricsraw<-summarizeByHurdle(allmatches=matches,bySpecies="yes",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
			metricsraw$Treatment<-"Uncorrected"
			metricsadj<-summarizeByHurdle(allmatches=adjmatches,bySpecies="yes",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
			metricsadj$Treatment<-"Corrected"
			mtdf<-rbind(metricsraw,metricsadj)
			mtdf$Model<-mdl
			return(mtdf)
		},hfdf=hfdf,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
mdlcorrdf<-subset(mdldf,Treatment=="Corrected")
mFbsp<-aggregate(Fbeta~SpeciesCode+Model,data=mdlcorrdf,max)
wFbsp<-reshape(mFbsp,idvar="SpeciesCode",timevar="Model",direction="wide")
names(wFbsp)<-c("SpeciesCode","MobileNet","Resnet101","Resnet50")
## CANNOT DO SOSP...
wFbsp<-subset(wFbsp,SpeciesCode != "SOSP")

#Now loop through each species and identify the top model and the hurdle value for it
mxFbsp<-ldply(wFbsp$SpeciesCode,function(ss,wFbsp,plotdataBySpecies){
			tdf<-subset(wFbsp,SpeciesCode==ss)
			m1<-tdf$MobileNet;m2<-tdf$Resnet101;m3<-tdf$Resnet50
			if(m1>m2 && m1>m3){
				pdbs<-subset(mdlcorrdf,Model=="MobileNet::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="MobileNet",Fbeta=m1,Threshold=hurdval)
			}else if(m2>m1 && m2>m3){
				pdbs<-subset(mdlcorrdf,Model=="Resnet101::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="Resnet101",Fbeta=m2,Threshold=hurdval)
			}else{
				pdbs<-subset(mdlcorrdf,Model=="Resnet50::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="Resnet50",Fbeta=m3,Threshold=hurdval)
			}
			if(nrow(rdf)>1){rdf<-subset(rdf,Threshold==max(rdf$Threshold))}
			return(rdf)
		},wFbsp=wFbsp,plotdataBySpecies=plotdataBySpecies)

for(ss in mxFbsp$SpeciesCode){ ## For the appendix!
	spMxFb<-subset(mdlcorrdf,SpeciesCode==ss)
	spMxFb$ModelName<-ifelse(spMxFb$Model=="MobileNet::sigmoid","MobileNet",ifelse(spMxFb$Model=="Resnet101::sigmoid","Resnet101","Resnet50"))
	modelN<-subset(mxFbsp,SpeciesCode==ss)$ModelName
	threshV<-subset(mxFbsp,SpeciesCode==ss)$Threshold
	## Need the mdf of the maxBetas...
	mdf<-subset(spMxFb,grepl(modelN,Model)) 
	mdf<-subset(mdf,hurdle==threshV)
	
	pF<-ggplot(spMxFb,aes(x=hurdle,y=Fbeta)) + geom_line(aes(color=ModelName),size=1.2) +
			geom_point(data = mdf,aes(x=hurdle,y=Fbeta),size=3) +
			theme_bw() + labs(title=ss,x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model")
	dev.new();print(pF)
}

## Inspecting the results, I selected:
# MobileNet: BTYW, MODO
# Resnet101: OATI
# Resnet50: WREN
mxBetabySp<-subset(mdlcorrdf, SpeciesCode %in% c("BTYW","OATI","WREN"))
mxBetabySp$ModelName<-ifelse(mxBetabySp$Model=="MobileNet::sigmoid","MobileNet",ifelse(mxBetabySp$Model=="Resnet101::sigmoid","Resnet101","Resnet50"))
## Need the mdf of the maxBetas...
mdf<-subset(mxFbsp,((SpeciesCode=="BTYW" & ModelName=="MobileNet") | (SpeciesCode=="OATI" & ModelName=="Resnet101") | (SpeciesCode=="WREN" & ModelName=="Resnet50")))
names(mdf)<-gsub("Threshold","hurdle",names(mdf))
pF<-ggplot(mxBetabySp,aes(x=hurdle,y=Fbeta)) + geom_line(aes(color=ModelName),size=1.2) + facet_wrap(~SpeciesCode,ncol=3) +
		geom_point(data = mdf,aes(x=hurdle,y=Fbeta),size=3) +
		theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model")
dev.new();print(pF)

###########################
## Matt's dot-plot
mFbsp$ModelName<-ifelse(mFbsp$Model=="MobileNet::sigmoid","MobileNet",ifelse(mFbsp$Model=="Resnet101::sigmoid","Resnet101","Resnet50"))
mdp<-ggplot(subset(mFbsp,SpeciesCode!="SOSP"),aes(x=SpeciesCode,y=Fbeta)) + geom_point(aes(color=ModelName),size=3) + coord_flip() +
		theme_bw() + labs(x="",y=paste0("F(","\u03b2","=0.5)"),color="Model")
dev.new();print(mdp)

###########################
## Table of performance metrics
load(file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/noPretrainGVPerformance_06072021.RData"))
noPretrainGV<-summarizeByHurdle(allmatches=gvmatches,bySpecies="no",summarizeToSample=summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5)
noPretrainGV$ModelType<-"Not Pre-trained"
noPretrainGV$Treatment<-"Uncorrected"
noPretrainGV$Model<-ifelse(noPretrainGV$ModelName=="MobileNet::sigmoid","MobileNet",ifelse(noPretrainGV$ModelName=="Resnet50::sigmoid","Resnet50","Resnet101"))
preTrainedGV<-prdf
preTrainedGV$Model<-ifelse(preTrainedGV$Model=="MobileNet::sigmoid","MobileNet",ifelse(preTrainedGV$Model=="Resnet50::sigmoid","Resnet50","Resnet101"))
preTrainedGV$ModelType<-"Pre-trained"

flds<-c("Fbeta","Prec","Sens","F1val","FPper","Model","ModelType","Treatment")
inddata<-rbind(noPretrainGV[,flds],preTrainedGV[,flds])
models<-unique(inddata$Model)

getClosestRecall<-function(dd,sensval){
	svu<-sensval+0.001; svd<-sensval-0.001
	dd<-dd<-dd[order(dd$Sens),]
	dda<-subset(dd,Sens<svu);ddarec<-dda[nrow(dda),"Sens"]
	ddb<-subset(dd,Sens>svd);ddbrec<-ddb[1,"Sens"]
	if((0.2-ddarec)<(ddbrec-0.2)){ #use dda
		ddr<-dda[nrow(dda),]
	}else{
		ddr<-ddb[1,]
	}
	return(ddr)
}
# for each model need to get: 
# max(F05), precision and recall at that value, max(F1val) and precision and recall at that value; closest recall value to 0.2 and 0.5, and associated precision values
indtable<-ldply(models,function(mdl,inddata,getClosestRecall){
			mdldata<-subset(inddata,Model==mdl)
			types<-unique(mdldata$ModelType)
			mdltable<-ldply(types,function(typ,mdldata,getClosestRecall){
						mtypedata<-subset(mdldata,ModelType==typ)
						treats<-unique(mtypedata$Treatment)
						typedata<-ldply(treats,function(trt,mtypedata,getClosestRecall){
									ttmdata<-subset(mtypedata,Treatment==trt)
									maxF05df<-subset(ttmdata,Fbeta==max(ttmdata$Fbeta,na.rm=TRUE))
									maxF1df<-subset(ttmdata,F1val==max(ttmdata$F1val,na.rm=TRUE))
									rec20df<-getClosestRecall(dd=ttmdata,sensval=0.2)
									rec50df<-getClosestRecall(dd=ttmdata,sensval=0.5)
									corrV<-ifelse(trt=="Uncorrected","Unc","Corr")
									resdf<-data.frame(Corrected=corrV, 
											F05val=round(maxF05df$Fbeta,3), Prec_maxF05=round(maxF05df$Prec,3), Rec_maxF05=round(maxF05df$Sens,3), FPP_maxF05=round(maxF05df$FPper,3),
											F1val=round(maxF1df$Fbeta,3), Prec_maxF1=round(maxF1df$Prec,3), Rec_maxF1=round(maxF1df$Sens,3), FPP_maxF1=round(maxF1df$FPper,3),
											#R20val=round(rec20df$Sens,3), Prec_R20=round(rec20df$Prec,3), FPP_R20=round(rec20df$FPper,3), #nothing close to R20
											R50val=round(rec50df$Sens,3), Prec_R50=round(rec50df$Prec,3), FPP_R50=round(rec50df$FPper,3))
									return(resdf)
								},mtypedata=mtypedata,getClosestRecall=getClosestRecall)
						typedata$Pretrain<-ifelse(typ=="Not Pre-trained","No","Yes")
						return(typedata)
					},mdldata=mdldata,getClosestRecall=getClosestRecall)
			mdltable$Model<-mdl
			mdltable$sortMdl<-ifelse(mdltable$Model=="MobileNet",1,ifelse(mdltable$Model=="Resnet50",2,3))
			mdltable$sortTrt<-ifelse(mdltable$Corrected=="Unc",10,20)
			mdltable$sortPtn<-ifelse(mdltable$Pretrain=="No",100,200)
			mdltable$sortVal<-mdltable$sortMdl + mdltable$sortTrt + mdltable$sortPtn
			mdltable$Treatment<-ifelse(mdltable$sortVal==111,"No Pretraining Uncorrected",
					ifelse(mdltable$sortVal==112,"No Pretraining Uncorrected",
							ifelse(mdltable$sortVal==113,"No Pretraining Uncorrected",
									ifelse(mdltable$sortVal==211,"With Pretraining Uncorrected",
											ifelse(mdltable$sortVal==212,"With Pretraining Uncorrected",
													ifelse(mdltable$sortVal==213,"With Pretraining Uncorrected","With Pretraining Corrected"))))))
			return(mdltable)
		},inddata=inddata,getClosestRecall=getClosestRecall)
indtable<-indtable[order(indtable$sortVal),]
indtable<-indtable[,c("Pretrain","Corrected","Model",
				"F05val","Prec_maxF05","Rec_maxF05","FPP_maxF05",
				"F1val","Prec_maxF1","Rec_maxF1","FPP_maxF1",
				"Prec_R50","FPP_R50","Treatment")] #"R20val","Prec_R20","FPP_R20","R50val"
names(indtable)<-c("Pretrain","Corrected","Model",
		"max(F0.5)","Precision at max(F0.5)","Recall at max(F0.5)","%FP at max(F0.5)",
		"max(F1.0)","Precision at max(F1.0)","Recall at max(F1.0)","%FP at max(F1.0)",
		"Precision at 50% Recall","%FP at 50% Recall","Treatment")

nc<-ncol(indtable)-1
idvar<-c("Pretrain","Corrected","Model","Treatment")
lindtable<-reshape(indtable,idvar=idvar,varying=list(4:nc),direction="long",times=names(indtable)[4:nc],timevar="Index",v.names="Value")
row.names(lindtable)<-NULL

trtorddf<-data.frame(Treatment=unique(lindtable$Treatment),trtorder=c(1,2,3))
lindtable<-merge(lindtable,trtorddf,by="Treatment",all.x=T)
lindtable$Treatment<-as.factor(lindtable$Treatment)
lindtable$Treatment<-reorder(lindtable$Treatment,lindtable$trtorder)

indorddf<-data.frame(Index=unique(lindtable$Index),indorder=c(1,4,7,3,6,2,5,8,10,9))
lindtable<-merge(lindtable,indorddf,by="Index",all.x=T)
lindtable$Index<-as.factor(lindtable$Index)
lindtable$Index<-reorder(lindtable$Index,lindtable$indorder)

mdlorddf<-data.frame(Model=unique(lindtable$Model),mdlorder=c(2,1,3))
lindtable<-merge(lindtable,mdlorddf,by="Model",all.x=T)
lindtable$Model<-as.factor(lindtable$Model)
lindtable$Model<-reorder(lindtable$Model,lindtable$mdlorder)

perfPlot<-ggplot(lindtable,aes(x=Treatment,y=Value)) + geom_point(aes(color=Model),size=3) + 
		geom_point(aes(color=Model),size=3, data=subset(lindtable,Model=="MobileNet")) +
		facet_wrap(~Index,ncol=4) + coord_flip() + theme_bw() + labs(x="",y="Index value") 
dev.new(); print(perfPlot)

indtable<-indtable[,c("Model","Treatment","max(F0.5)","Precision at max(F0.5)","Recall at max(F0.5)","%FP at max(F0.5)",
				"max(F1.0)","Precision at max(F1.0)","Recall at max(F1.0)","%FP at max(F1.0)",
				"Precision at 50% Recall","%FP at 50% Recall")]
print(indtable)

##########################
## PR curves comparing the effect of pre-training
#need to make pretty...
flds<-c("Prec","Sens","hurdle","Model","ModelType","Treatment")
pretdata<-rbind(noPretrainGV[,flds],preTrainedGV[,flds])
pretdata<-subset(pretdata,Treatment=="Uncorrected")

mdlorddf<-data.frame(Model=unique(pretdata$Model),mdlorder=c(1,2,3))
pretdata<-merge(pretdata,mdlorddf,by="Model",all.x=T)
pretdata$Model<-as.factor(pretdata$Model)
pretdata$Model<-reorder(pretdata$Model,pretdata$mdlorder)

p1<-ggplot(pretdata,aes(x=hurdle, y=Prec)) + 
		geom_line(aes(color=ModelType),size=2) +
		scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
		scale_x_continuous(limits=c(0.6,1),breaks=c(0.6,0.7,0.8,0.9,1)) +
		labs(x="Threshold", y="Precision",color="Pretraining") + theme_bw() +
		facet_wrap(~Model,ncol=3)
p2<-ggplot(pretdata,aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=ModelType),size=2,) +
		scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
		scale_x_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
		labs(x="Recall", y="Precision",color="Pretraining") + theme_bw() +
		facet_wrap(~Model,ncol=3)
print(p1)
dev.new();print(p2)


##########################
## PR curve using each species' best model
prBestBySpecies<-ldply(mxFbsp$SpeciesCode,function(spp,mxFbsp,mdldf){
			mxF<-subset(mxFbsp,SpeciesCode==spp)
			hrdv<-mxF$Threshold; mdlv<-paste0(mxF$ModelName,"::sigmoid")
			tdf<-subset(mdldf,SpeciesCode==spp & Model==mdlv) #& hurdle==hrdv
			return(tdf)
		},mxFbsp=mxFbsp,mdldf=mdldf)
prBestBySpecies<-subset(prBestBySpecies,hurdle>0.70)

# dplyr summarize the resulting df
prBestBySppdf<-as.data.frame(prBestBySpecies %>% group_by(hurdle,Treatment) %>% 
				dplyr::summarize(Prec=mean(Prec,na.rm=TRUE),Sens=mean(Sens,na.rm=TRUE)))

#need to make pretty...
p1bbsp<-ggplot(subset(prBestBySppdf,hurdle<0.999),aes(x=hurdle, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw() 
p2bbsp<-ggplot(subset(prBestBySppdf,Sens<0.7),aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw() 
dev.new();print(p1bbsp)
dev.new();print(p2bbsp)

###########################
## Point graph of F05 vs threshold where each dot is the best model and threshold for each species, mapping color to taxonomy
taxdf<-mxFbsp
taxdf$Passerine<-ifelse(taxdf$SpeciesCode %in% c("ACWO","CAQU","MODO","MOUQ","WITU"),"No","Yes")
taxPlt<-ggplot(taxdf,aes(x=Threshold,y=Fbeta)) + geom_point(aes(color=Passerine),size=3) + theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Passerine?")
mdlPlt<-ggplot(taxdf,aes(x=Threshold,y=Fbeta)) + geom_point(aes(color=ModelName),size=3) + theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model")
dev.new();print(taxPlt)
dev.new();print(mdlPlt)

###########################
## Performance at the event, before and after logistic correction
# I.e., summarize the GV results at the event, with and without correction
#######
# This takes time - skip if file exists alredy
difffilen<-paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/Correction_FPdifference_minuteLevel.RData")
if(file.exists(difffilen)){
	load(difffilen)
}else{
	hfilts<-names(gvpreadadjlst)
	summdfev<-ldply(hfilts,function(hnm,gvpreadadjlst,summarizeToSampleAllSpecies){
				hfdf<-gvpreadadjlst[[hnm]]
				mdldf<-ldply(unique(hfdf$Model),function(mdl,hfdf,summarizeToSampleAllSpecies){
							matches<-subset(hfdf,Model==mdl)
							adjmatches<-matches[,which(names(matches)!="match")]
							names(adjmatches)<-gsub("adjMatch","match",names(adjmatches))
							metricsraw<-summarizeByHurdle(allmatches=matches,bySpecies="yes",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="minute",beta=0.5,addEvent=TRUE)
							metricsraw$Treatment<-"Uncorrected"
							metricsadj<-summarizeByHurdle(allmatches=adjmatches,bySpecies="yes",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="minute",beta=0.5,addEvent=TRUE)
							metricsadj$Treatment<-"Corrected"
							mtdf<-rbind(metricsraw,metricsadj)
							mtdf$Model<-mdl
							return(mtdf)
						},hfdf=hfdf,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
				mdldf$PredictionFilter<-hnm
				return(mdldf)
			},gvpreadadjlst=gvpreadadjlst,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
	
	## Use only PredictionFilter==h65
	prdf<-subset(summdfev,PredictionFilter=="h65")
	## Get the number of species per event (i.e., species with truePos>0) for corrected and uncorrected
	corrdiff<-as.data.frame(subset(prdf,truePos>0) %>% group_by(event,Treatment) %>% dplyr::summarize(nsppdiff=length(unique(SpeciesCode))))
	uncorrev<-subset(corrdiff,Treatment=="Uncorrected")$event
	correv<-subset(corrdiff,Treatment=="Corrected")$event
	gvev<-unique(prdf$event)
	zevuncorr<-subset(gvev, !gvev %in% uncorrev)
	zevuncorrdf<-data.frame(event=zevuncorr,Treatment=rep("Uncorrected",length(zevuncorr)),nsppdiff=rep(0,length(zevuncorr)))
	zevcorr<-subset(gvev, !gvev %in% correv)
	zevcorrdf<-data.frame(event=zevcorr,Treatment=rep("Corrected",length(zevcorr)),nsppdiff=rep(0,length(zevcorr)))
	corrdiff<-rbind(corrdiff,zevuncorrdf)
	corrdiff<-rbind(corrdiff,zevcorrdf)
	save(corrdiff,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/Correction_FPdifference_minuteLevel.RData"))
}
#######

spdiffplot<-ggplot(corrdiff,aes(x=nsppdiff)) + geom_bar(aes(fill=Treatment),position="dodge") + theme_bw() + labs(x="Number of species per event",y="Number of GV events")
dev.new(); print(spdiffplot)

###########################
## Consider comparing to simple filter used previously: compare vs GV at the 2-second, event, and site levels.
# Can't be done yet because the simple filters are summarized at the site level, not at the event level
# Once we correct every prediction we'll be able to compare.


