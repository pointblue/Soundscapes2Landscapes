# TODO: Add comment
# 
# Author: Leo Salas (lsalas@pointblue.org) & Matthew Clark (matthew.clark@sonoma.edu)
###############################################################################

#### This file produces all the tables and results shown in the CNN paper

## Using soundscape test data vs ROI test data
# Here we use soundcape test data throughout, because the AI models are trained to predict very well against the ROI test set
# yet they do not predict as well against a random set of data from our recordings (the soundscape data). We believe that this difference is due to our
# choice of training data, which resulted in recordings with very clear sounds and distinct calls, thus not generalizing as well to the reality
# of complex and noisy environments in most of our recordings.

## Data files:
# logisticCorrModels_fullHour065_predAdj658095_10262021.RData
# This file contains the results of predictions tested against the soundscape data, adjusting results after logistic correction using
# 65% threshold for the covariate data of the logistic model, and a 50% penalization of the logistic prediction
# Clearly, we can get better results by increasing either or both penalization thresholds, but this low-level 
# penalization illustrates well the effect of our approach.
# Source:

# BirdNET_GV_matches_06102021.RData
# This is the evaluation of the BirdNET predictions against the soundscape data.
# Source:

# noPretrainGVPerformance_06072021.RData
# this is the evaluation of model predictions against GV (without logistic correction)
# Source:

# Correction_FPdifference_minuteLevel.RData
# this compares the rate of FP between the logistic-corrected predictions vs not corrected at the minute level
# Source:


##########################

libs<-c("ggplot2","plyr","dplyr","RColorBrewer")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

pathToLocalGit<-"c:/users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/CNN_post-processing/"  #point to your local git clone

## Need the utility matching functions
source(paste0(pathToLocalGit,"/scripts/predMatching_utils.R"))


##################################################################
## FIGURE 5

## Looping through hurdle value filters, then by model
load(paste0(pathToLocalGit,"data/logisticCorrModels_fullHour065_predAdj65809599_lch5099_04022022.RData"))
hfilts<-names(gvpreadadjlst)
gvpreadadjlst<-llply(hfilts,function(hnm,gvpreadadjlst){
			     hfdf<-gvpreadadjlst[[hnm]]
			     hfdf<-subset(hfdf,lchurd==0.50)
			     return(hfdf)
		},gvpreadadjlst=gvpreadadjlst)
names(gvpreadadjlst)<-hfilts
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
load(file=paste0(pathToLocalGit,"data/BirdNET_GV_matches_06102021.RData"))
bndf<-summarizeByHurdle(allmatches=bngvmatches,bySpecies="no",summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,sumLevel="clip",beta=0.5)
bndf$Treatment<-"Uncorrected"; bndf$Model<-"BirdNET"
prplot<-rbind(prdf[,names(bndf)],bndf)

p1<-ggplot(subset(prplot,hurdle<=0.99),aes(x=hurdle, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw() +
		facet_wrap(~Model, ncol=2)
p2<-ggplot(subset(prplot,hurdle<=0.9999),aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=Treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw() +
		facet_wrap(~Model, ncol=2)
dev.new();print(p1)
dev.new();print(p2)

###########################
## FIGURE 6 - F05 curves plot
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
names(wFbsp)<-c("SpeciesCode","MobileNetv2","ResNet101v2","ResNet50v2")
## CANNOT DO SOSP...
wFbsp<-subset(wFbsp,SpeciesCode != "SOSP")

#Now loop through each species and identify the top model and the hurdle value for it
mxFbsp<-ldply(wFbsp$SpeciesCode,function(ss,wFbsp,plotdataBySpecies){
			tdf<-subset(wFbsp,SpeciesCode==ss)
			m1<-tdf$MobileNetv2;m2<-tdf$ResNet101v2;m3<-tdf$ResNet50v2
			m1<-ifelse(is.na(m1),0,m1);m2<-ifelse(is.na(m2),0,m2);m3<-ifelse(is.na(m3),0,m3)
			if(m1>=m2 && m1>=m3){
				pdbs<-subset(mdlcorrdf,Model=="MobileNet::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="MobileNetv2",Fbeta=m1,Threshold=hurdval)
			}else if(m2>m1 && m2>=m3){
				pdbs<-subset(mdlcorrdf,Model=="Resnet101::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet101v2",Fbeta=m2,Threshold=hurdval)
			}else{
				pdbs<-subset(mdlcorrdf,Model=="Resnet50::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet50v2",Fbeta=m3,Threshold=hurdval)
			}
			if(nrow(rdf)>1){rdf<-subset(rdf,Threshold==max(rdf$Threshold))}
			return(rdf)
		},wFbsp=wFbsp,plotdataBySpecies=plotdataBySpecies)

############### Parenthesis
## This to make Table 2 and Figure 8 - below  
bySpeciesMxFbsp<-ldply(mxFbsp$SpeciesCode,function(ss,mxFbsp,mdldf){
			tmxfb<-subset(mxFbsp,SpeciesCode==ss)
			mdlmxfb<-ifelse(tmxfb$ModelName=="MobileNetv2","MobileNet::sigmoid",ifelse(tmxfb$ModelName=="ResNet50v2","Resnet50::sigmoid","Resnet101::sigmoid"))
			hrdmxfb<-tmxfb$Threshold
			fbetamx<-tmxfb$Fbeta
			sppmxfb<-subset(mdldf,SpeciesCode==ss & Model==mdlmxfb & hurdle==hrdmxfb)
			ucspmxfb<-subset(sppmxfb,Treatment=="Uncorrected")
			crspmxfb<-subset(sppmxfb,Treatment=="Corrected")
			bspmxfb<-data.frame(SpeciesCode=ss,maxFbeta=fbetamx,Threshold=hrdmxfb,Model=tmxfb$ModelName,corrPrecision=crspmxfb$Prec,corrRecall=crspmxfb$Sens,uncorrPrecision=ucspmxfb$Prec,uncorRecall=ucspmxfb$Sens)
			return(bspmxfb)
		},mxFbsp=mxFbsp,mdldf=mdldf)

# repeat for the uncorrected
mdluncorrdf<-subset(mdldf,Treatment=="Uncorrected")
umFbsp<-aggregate(Fbeta~SpeciesCode+Model,data=mdluncorrdf,max)
uwFbsp<-reshape(umFbsp,idvar="SpeciesCode",timevar="Model",direction="wide")
names(uwFbsp)<-c("SpeciesCode","MobileNetv2","ResNet101v2","ResNet50v2")
## CANNOT DO SOSP...
uwFbsp<-subset(uwFbsp,SpeciesCode != "SOSP")

#Now loop through each species and identify the top model and the hurdle value for it
umxFbsp<-ldply(uwFbsp$SpeciesCode,function(ss,uwFbsp,plotdataBySpecies){
			tdf<-subset(uwFbsp,SpeciesCode==ss)
			m1<-tdf$MobileNetv2;m2<-tdf$ResNet101v2;m3<-tdf$ResNet50v2
			m1<-ifelse(is.na(m1),0,m1);m2<-ifelse(is.na(m2),0,m2);m3<-ifelse(is.na(m3),0,m3)
			if(m1>=m2 && m1>=m3){
				pdbs<-subset(mdlcorrdf,Model=="MobileNet::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="MobileNetv2",Fbeta=m1,Threshold=hurdval)
			}else if(m2>m1 && m2>=m3){
				pdbs<-subset(mdlcorrdf,Model=="Resnet101::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet101v2",Fbeta=m2,Threshold=hurdval)
			}else{
				pdbs<-subset(mdlcorrdf,Model=="Resnet50::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet50v2",Fbeta=m3,Threshold=hurdval)
			}
			if(nrow(rdf)>1){rdf<-subset(rdf,Threshold==max(rdf$Threshold))}
			return(rdf)
		},uwFbsp=uwFbsp,plotdataBySpecies=plotdataBySpecies)

bySpeciesUMxFbsp<-ldply(umxFbsp$SpeciesCode,function(ss,umxFbsp,mdldf){
			tmxfb<-subset(umxFbsp,SpeciesCode==ss)
			mdlmxfb<-ifelse(tmxfb$ModelName=="MobileNetv2","MobileNet::sigmoid",ifelse(tmxfb$ModelName=="ResNet50v2","Resnet50::sigmoid","Resnet101::sigmoid"))
			hrdmxfb<-tmxfb$Threshold
			fbetamx<-tmxfb$Fbeta
			sppmxfb<-subset(mdldf,SpeciesCode==ss & Model==mdlmxfb & hurdle==hrdmxfb)
			ucspmxfb<-subset(sppmxfb,Treatment=="Uncorrected")
			crspmxfb<-subset(sppmxfb,Treatment=="Corrected")
			bspmxfb<-data.frame(SpeciesCode=ss,maxFbeta=fbetamx,Threshold=hrdmxfb,Model=tmxfb$ModelName,corrPrecision=crspmxfb$Prec,corrRecall=crspmxfb$Sens,uncorrPrecision=ucspmxfb$Prec,uncorRecall=ucspmxfb$Sens)
			return(bspmxfb)
		},umxFbsp=umxFbsp,mdldf=mdldf)
########################

for(ss in mxFbsp$SpeciesCode){ ## For the appendix!
	spMxFb<-subset(mdlcorrdf,SpeciesCode==ss)
	spMxFb$ModelName<-ifelse(spMxFb$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(spMxFb$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
	modelN<-subset(mxFbsp,SpeciesCode==ss)$ModelName
	threshV<-subset(mxFbsp,SpeciesCode==ss)$Threshold
	## Need the mdf of the maxBetas...
	mdf<-subset(spMxFb,hurdle==threshV & ModelName==modelN)
	
	pF<-ggplot(spMxFb,aes(x=hurdle,y=Fbeta)) + geom_line(aes(color=ModelName),size=1.2) +
			geom_point(data = mdf,aes(x=hurdle,y=Fbeta),size=3) +
			theme_bw() + labs(title=ss,x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model")
	#dev.new();print(pF)
}

## Inspecting the results, I selected:
# MobileNetv2: BTYW, MODO
# ResNet101v2: OATI
# ResNet50v2: WREN
mxBetabySp<-subset(mdlcorrdf, SpeciesCode %in% c("BTYW","OATI","WREN"))
mxBetabySp$ModelName<-ifelse(mxBetabySp$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(mxBetabySp$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
## Need the mdf of the maxBetas...
mdf<-subset(mxFbsp,((SpeciesCode=="BTYW" & ModelName=="MobileNetv2") | (SpeciesCode=="OATI" & ModelName=="ResNet101v2") | (SpeciesCode=="WREN" & ModelName=="ResNet50v2")))
names(mdf)<-gsub("Threshold","hurdle",names(mdf))
pF<-ggplot(subset(mxBetabySp, hurdle <=0.99),aes(x=hurdle,y=Fbeta)) + geom_line(aes(color=ModelName),size=1.2) + facet_wrap(~SpeciesCode,ncol=3) +
		geom_point(data = mdf,aes(x=hurdle,y=Fbeta),size=3) +
		theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model") +
    scale_colour_brewer(palette = "Set2") 
dev.new();print(pF)

ggsave("C:/Users/salasle/git/Soundscapes2Landscapes/CNN_Bird_Species/three_species_optimal_threshold.png", width = 7, height = 5, units = "in", dpi=600)


################################################################
## FIGURE 7
## Optimal threshold dot-plot by species, 3 models
mFbsp$ModelName<-ifelse(mFbsp$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(mFbsp$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
mdpdf<-subset(mFbsp,SpeciesCode!="SOSP")

## Excluding SOSP because of poor performance   "SOSP",
gvspp<-c("ACWO","AMCR","AMRO","BEWR","BGGN","BHGR","BTYW","CALT","CAQU","CASJ","CAVI","CBCH","CORA","COYE","DEJU","EUCD","HOFI","MAWR","MODO","OATI","OCWA",
	 "PAWR","PSFL","RWBL","SAVS","SPTO","STJA","WAVI","WBNU","WCSP","WEME","WETA","WITU","WIWA","WREN")
mdpdf<-subset(mdpdf,SpeciesCode %in% gvspp)

#### All species corrected regardless of GV evaluation = 46
## With the above filter - all species with enough GV data to evaluate = 35

mdp<-ggplot(mdpdf,aes(x=SpeciesCode,y=Fbeta)) + geom_point(aes(color=ModelName),size=3) + coord_flip() +
  theme_bw() + labs(x="",y=paste0("F(","\u03b2","=0.5)"),color="Model") +
  scale_colour_brewer(palette = "Set2") +
  scale_x_discrete(limits = rev(unique(sort(mFbsp$SpeciesCode))))
dev.new();print(mdp)

ggsave("C:/Users/salasle/git/Soundscapes2Landscapes/CNN_Bird_Species/species_optimal_threshold.png", width = 7, height = 7, units = "in", dpi=600)

######################################
##### Table 2
bySpeciesMxFbsp<-subset(bySpeciesMxFbsp,SpeciesCode %in% gvspp)
bySpeciesUMxFbsp<-subset(bySpeciesUMxFbsp,SpeciesCode %in% gvspp)

tbl2<-as.data.frame(bySpeciesMxFbsp %>% group_by(Model) %>% dplyr::summarize(maxFbeta=mean(maxFbeta),Precision=mean(corrPrecision),Recall=mean(corrRecall)))
tbl2$maxFbeta<-round(tbl2$maxFbeta,2);tbl2$Precision<-round(tbl2$Precision*100,1);tbl2$Recall<-round(tbl2$Recall*100,1)
tbl2all<-data.frame(Model="All",maxFbeta=round(mean(bySpeciesMxFbsp$maxFbeta),2),Precision=round(mean(bySpeciesMxFbsp$corrPrecision)*100,1),Recall=round(mean(bySpeciesMxFbsp$corrRecall)*100,1))
tbl2<-rbind(tbl2,tbl2all)
tbl2unc<-as.data.frame(bySpeciesUMxFbsp %>% group_by(Model) %>% dplyr::summarize(maxFbeta=mean(maxFbeta),Precision=mean(uncorrPrecision),Recall=mean(uncorRecall)))
tbl2unc$maxFbeta<-round(tbl2unc$maxFbeta,2);tbl2unc$Precision<-round(tbl2unc$Precision*100,1);tbl2unc$Recall<-round(tbl2unc$Recall*100,1)
tbl2uncAll<-data.frame(Model="All",maxFbeta=round(mean(bySpeciesUMxFbsp$maxFbeta),2),Precision=round(mean(bySpeciesUMxFbsp$uncorrPrecision)*100,1),Recall=round(mean(bySpeciesUMxFbsp$uncorRecall)*100,1))
tbl2unc<-rbind(tbl2unc,tbl2uncAll)

################################



#################################################################################
## Anything below goes to Supplemental Materials

###########################
## Table of performance metrics
load(file=paste0(pathToLocalGit,"data/noPretrainGVPerformance_06072021.RData"))
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
# Soundscape-level plot
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

mdldf$Model<-ifelse(mdldf$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(mdldf$Model=="Resnet50::sigmoid","ResNet50v2","ResNet101v2"))

prBestBySpecies<-ldply(mxFbsp$SpeciesCode,function(spp,mxFbsp,mdldf){
			mxF<-subset(mxFbsp,SpeciesCode==spp)
			hrdv<-mxF$Threshold; mdlv<-mxF$Model
			tdf<-subset(mdldf,SpeciesCode==spp & Model==mdlv) #& hurdle==hrdv
			return(tdf)
		},mxFbsp=mxFbsp,mdldf=mdldf)
#prBestBySpecies<-subset(prBestBySpecies,hurdle>0.65)

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
## Point graph of F05 vs threshold where each dot is the best model and threshold for each species
mdlPlt<-ggplot(mxFbsp,aes(x=Threshold,y=Fbeta)) + geom_point(aes(color=ModelName),size=3) + theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model") + scale_colour_brewer(palette = "Set2")
dev.new();print(mdlPlt)

ggsave("C:/Users/salasle/git/Soundscapes2Landscapes/CNN_Bird_Species/f05_vs_threshold.png", width = 7, height = 5, units = "in", dpi=600)


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

###########################
## FIGURE 3
## Precision-recall plot of test ROIs, all species combined

## Performance data...
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/performanceData.RData")

## Logistic correction data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrectionData.RData")
pretrainedCorrGV$Model<-"Species specific"; pretrainedCorrGV$morder<-5;pretrainedCorrGV$Source<-"Corrected golden validations"
pretrainedUncorrGV$Model<-"Species specific"; pretrainedUncorrGV$morder<-5;pretrainedUncorrGV$Source<-"Uncorrected golden validations"

getFval<-function(df,beta){
  fv<-(1+(beta^2))*(df$Prec*df$Sens)/(((beta^2)*df$Prec) + df$Sens)
  return(fv)
}

pretrainedCorrGV$F10v<-sapply(1:nrow(pretrainedCorrGV),function(rr,pretrainedCorrGV,getFval){
  df<-pretrainedCorrGV[rr,]
  fval<-getFval(df=df,beta=1)
  return(fval)
},pretrainedCorrGV=pretrainedCorrGV,getFval=getFval)

pretrainedCorrGV$F05v<-sapply(1:nrow(pretrainedCorrGV),function(rr,pretrainedCorrGV,getFval){
  df<-pretrainedCorrGV[rr,]
  fval<-getFval(df=df,beta=0.5)
  return(fval)
},pretrainedCorrGV=pretrainedCorrGV,getFval=getFval)

pretrainedUncorrGV$F10v<-sapply(1:nrow(pretrainedUncorrGV),function(rr,pretrainedUncorrGV,getFval){
  df<-pretrainedUncorrGV[rr,]
  fval<-getFval(df=df,beta=1)
  return(fval)
},pretrainedUncorrGV=pretrainedUncorrGV,getFval=getFval)

pretrainedUncorrGV$F05v<-sapply(1:nrow(pretrainedUncorrGV),function(rr,pretrainedUncorrGV,getFval){
  df<-pretrainedUncorrGV[rr,]
  fval<-getFval(df=df,beta=0.5)
  return(fval)
},pretrainedUncorrGV=pretrainedUncorrGV,getFval=getFval)

pretrainedCorrGV<-pretrainedCorrGV[,names(alldata)]
pretrainedUncorrGV<-pretrainedUncorrGV[,names(alldata)]

plotdata<-rbind(alldata,pretrainedCorrGV);plotdata<-rbind(plotdata,pretrainedUncorrGV)
plotdata$Model<-as.character(plotdata$Model)
plotdata$Model<-gsub("::sigmoid","",plotdata$Model)

### We want to plot the pretrained corrected vs uncorrected separtely
## So...
alldata$Model<-gsub("::sigmoid","",alldata$Model)
alldata$Model<-as.factor(alldata$Model)
morder<-data.frame(Model=unique(alldata$Model),modelOrder=c(3,2,1,4))
alldata<-merge(alldata,morder, by="Model")
alldata$Model<-reorder(alldata$Model,alldata$modelOrder)

logistdata<-rbind(pretrainedCorrGV,pretrainedUncorrGV)

## ROI performance data
load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/roilabelmatch.RData")
pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"
roidata<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/pattern_matching_ROIs_201109_testing.csv", stringsAsFactors=FALSE)
roidata$roicenter<-roidata$x1+((roidata$x2-roidata$x1)/2)
roidata$event<-sapply(roidata$filename,function(x){substr(x,1,32)})
roidata<-roidata[,c("event","birdcode","site","device","year","month","hour","songtype","vote","type","method","roicenter","x1","x2")]
names(roidata)<-gsub("type","assessType",names(roidata))
roidata$roiId<-1:nrow(roidata)

resdf<-subset(resdf,event %in% unique(roidata$event))
resdf$PredictionScore<-ifelse(resdf$PredictionScore==0,10000000,resdf$PredictionScore)
resdf<-subset(resdf,vote=="present")

listRoiIDdupes<-function(df){
  duprois<-df$roiId
  mdf<-subset(df,PredictionScore==max(PredictionScore))
  if(NROW(unique(mdf$birdcode))==1){
    #either take the higher prediction score or just the lower roiId
    if(nrow(mdf)>1){
      #same prediction score
      keeproi<-min(df$roiId)
    }else{ #there is a max prediction score
      keeproi<-mdf$roiId
    }
  }else{
    #more than one bird code, matching prediction score = max
    #take the first correct match
    mmdf<-subset(mdf,match=="TRUEPOS")
    keeproi<-mmdf$roiId; keeproi<-keeproi[1]
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
resdf<-subset(resdf,!birdcode == "BUSH")

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)

## Need this function:
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

species<-unique(resdf$birdcode)
remove(sproi)
for (s in species){
  df<-resdf[resdf$birdcode == s,] 
  sproidf<-summarizeLabeledToSampleAllSpecies(matches=df,hurdles=hurdvals)
  sproidf$SpeciesCode<-s
  if(exists("sproi")) sproi<-rbind(sproi,sproidf) else sproi<-sproidf
}

modelorder<-data.frame(Model=unique(lmetsdf$Model),morder=c(4,1,5,2,6,3))
labelroisp<-merge(lmetsdf,modelorder,by="Model", all.x=T)
labelroisp$Model<-as.factor(labelroisp$Model)
labelroisp$Model<-reorder(labelroisp$Model,labelroisp$morder)
labelroisp<-subset(labelroisp,grepl("sigmoid",Model))
labelroisp$Source<-"Test labeled data"
labelroisp$Model<-gsub("::sigmoid","",labelroisp$Model)

## Add it to alldata...
newplotdata<-subset(alldata,Source %in% c("Golden validations","Shree's ROIs","Golden validations NPT"))
newplotdata$Source<-ifelse(newplotdata$Source=="Shree's ROIs","Test labeled data NPT",newplotdata$Source)
newplotdata<-subset(newplotdata,Model!="BirdNET")

npdata<-rbind(newplotdata[,names(labelroisp)],labelroisp)

npdata <- npdata %>% mutate(Source = recode(Source,
                          "Golden validations" = "Soundscape - XC Pre-training",
                          "Golden validations NPT" = "Soundscape - No XC Pre-training",
                          "Test labeled data" = "ROI - XC Pre-training",
                          "Test labeled data NPT" = "ROI - No XC Pre-training"
  ))


npdata$Model = recode_factor(npdata$Model,
              "Resnet50" = "ResNet50v2",
              "Resnet101" = "ResNet101v2",
              "MobileNet" = "MobileNetv2")



p1da<-ggplot(subset(npdata, Threshold<=0.99),aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
  geom_line(aes(color=Source),size=2) +
  labs(x="Recall", y="Precision",color="Test source") +
  theme_bw() +
  scale_colour_brewer(palette = "Set2") 
 
ggsave("C:/Users/salasle/git/Soundscapes2Landscapes/CNN_Bird_Species/Pre-training_vs_no pre-training.png", width = 7, height = 5, units = "in", dpi=600)


####################################
## FIGURE 4
# Precision vs. threshold - ROIs and soundscape
ptdata = npdata %>% filter(Source == "Soundscape - XC Pre-training" | Source == "ROI - XC Pre-training")

colors = RColorBrewer::brewer.pal(4, "Set2")[1:4]
colors_select = c(colors[2], colors[4])

p2da<-ggplot(subset(ptdata, Threshold<=0.99),aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
  geom_line(aes(color=Source),size=2) +
  labs(x="Threshold", y="Precision",color="Test source") +
  theme_bw() +
  scale_colour_manual(values=colors_select)

ggsave("C:/Users/salasle/git/Soundscapes2Landscapes/CNN_Bird_Species/Pre-training_precision_vs_threshold.png", width = 7, height = 5, units = "in", dpi=600)


### End of Matt's code


