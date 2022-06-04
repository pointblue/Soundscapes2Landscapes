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

## Using a subset of 36 species instead of all 54
# We use only those species with enough soundscape data to evaluate the performance of models. Only 36 species satisfy this requirement.

## Data files:
# logisticCorrModels_fullHour065_predAdj65809599_lch5099_04022022.RData
# This file contains the results of predictions tested against the soundscape data, adjusting results after logistic correction using
# 65% threshold for the covariate data of the logistic model, and a 50% penalization of the logistic prediction
# Clearly, better results may be achieved by increasing either or both penalization thresholds, but this low-level 
# penalization illustrates well the effect of our approach.
# Source:

# BirdNET_GV_matches_06102021.RData
# This is the evaluation of the BirdNET predictions against the soundscape data.
# Source:

# performanceData.RData
# This file contains the evaluations of predictions from uncorrected data and performance from not-pretrained models for ROI data
# Source:

# logisticCorrectionData.RData
# This file provides the evaluations of predictions from corrected data
# Source:

# roilabelmatch.RData
# This file provides the evaluations of predictions for pre-trained models predicting on ROI data
# Source:

# pattern_matching_ROIs_201109_testing.csv
# We use this to filter for only predictions near a known ROI tag
# Source: This is the raw output from Arbimon for the files and locations of each tag, but filtered to inlcude only the tags set aside for testing the models

##########################

libs<-c("ggplot2","plyr","dplyr","RColorBrewer","ggpubr")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))
pathToLocalGit<-"c:/users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/CNN_post-processing/"  #Point to your local cloned repository

## Need the utility matching functions
source(paste0(pathToLocalGit,"scripts/predMatching_utils.R"))

# species in soundscape data to use in statistics (>=40 events for a species for calls used in training). Only 36 spp satisfy this requirement.
gvspp<-c("ACWO","AMCR","AMRO","BEWR","BGGN","BHGR","BTYW","CALT","CAQU","CASJ","CAVI","CBCH","CORA","COYE","DEJU","EUCD","HOFI","MAWR","MODO","MOUQ","OATI","OCWA",
		"PAWR","PSFL","RWBL","SAVS","SPTO","STJA","WAVI","WBNU","WCSP","WEME","WETA","WITU","WIWA","WREN")

##########################
## FIGURE 5 - PR curves

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
prplot$ModelName<-ifelse(prplot$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(prplot$Model=="Resnet101::sigmoid","ResNet101v2",ifelse(prplot$Model=="Resnet50::sigmoid","ResNet50v2","BirdNET")))

# this with GV data, not selecting for species with sufficient data
p1<-ggplot(subset(prplot, hurdle<=0.99),aes(x=hurdle, y=Prec*100)) + 
		geom_line(aes(color=Treatment),size=2) +
		labs(x="Threshold", y="Precision") + theme_bw() +
		facet_wrap(~ModelName, ncol=2) +
		scale_colour_brewer(palette = "Set2") +
		theme(legend.title=element_blank())
p2<-ggplot(subset(prplot, hurdle<=0.99999),aes(x=Sens*100, y=Prec*100)) + 
		geom_line(aes(color=Treatment),size=2,) +
		labs(x="Recall", y="Precision") + theme_bw() +
		facet_wrap(~ModelName, ncol=2) +
		scale_colour_brewer(palette = "Set2") +
		theme(legend.title=element_blank())
p3 <- ggarrange(p1, p2,
		labels = c("A", "B"),
		common.legend = TRUE,
		legend = "bottom",
		ncol = 2, nrow = 1) 
print(p3)   # This is Figure 5

##ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/species_post-classification_correction.png", width = 7, height = 5, units = "in", dpi=600)
#write.csv(prplot,"C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/prplot_data.csv", row.names=F)

###########################
### This code leads to Table 2 and also...
### Construct the PR curve for 3 species: facet by species, curves are pretrained 3 models, dot shows the max (i.e., Figure 6)
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
mxFbsp<-ldply(wFbsp$SpeciesCode,function(ss,wFbsp,plotdataBySpecies,mdlcorrdf){
			tdf<-subset(wFbsp,SpeciesCode==ss)
			m1<-tdf$MobileNetv2;m1<-ifelse(is.na(m1),0,m1);m2<-tdf$ResNet101v2;m2<-ifelse(is.na(m2),0,m2);m3<-tdf$ResNet50v2;m3<-ifelse(is.na(m3),0,m3)
			if((m1>m2 && m1>m3) | (m1==m2 && m1>m3) | (m1>m2 && m1==m3)){
				pdbs<-subset(mdlcorrdf,Model=="MobileNet::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="MobileNetv2",Fbeta=m1,Threshold=hurdval)
			}else if((m2>m1 && m2>m3) | (m2>m1 && m2==m3) | (m2==m1 && m2>m3)){
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
		},wFbsp=wFbsp,plotdataBySpecies=plotdataBySpecies,mdlcorrdf=mdlcorrdf)

optimalStats <- data.frame()
for(ss in mxFbsp$SpeciesCode){ ## For the appendix!
	spMxFb<-subset(mdlcorrdf,SpeciesCode==ss)
	spMxFb$ModelName<-ifelse(spMxFb$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(spMxFb$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
	modelN<-subset(mxFbsp,SpeciesCode==ss)$ModelName
	threshV<-subset(mxFbsp,SpeciesCode==ss)$Threshold
	## Need the mdf of the maxBetas...
	mdf<-subset(spMxFb,hurdle==threshV & ModelName==modelN)
	optimalStats <- rbind(optimalStats,mdf)
	pF<-ggplot(spMxFb,aes(x=hurdle,y=Fbeta)) + geom_line(aes(color=ModelName),size=1.2) +
			geom_point(data = mdf,aes(x=hurdle,y=Fbeta),size=3) +
			theme_bw() + labs(title=ss,x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model")
	#dev.new();print(pF)
}

# species with enough GV data
optimalStats<-subset(optimalStats,SpeciesCode %in% gvspp)

# write out stats on GV data with optimal
#write.csv(optimalStats,"C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/soundscape_optimal_threshold_statistics_corrected.csv", row.names = F)

###################
## Table 2
tbl2<-as.data.frame(optimalStats %>% group_by(ModelName) %>% dplyr::summarize(maxFbeta=mean(Fbeta),Precision=mean(Prec),Recall=mean(Sens)))
tbl2$maxFbeta<-round(tbl2$maxFbeta,2);tbl2$Precision<-round(tbl2$Precision*100,1);tbl2$Recall<-round(tbl2$Recall*100,1)
tbl2all<-data.frame(ModelName="All",maxFbeta=round(mean(optimalStats$Fbeta),2),Precision=round(mean(optimalStats$Prec)*100,1),Recall=round(mean(optimalStats$Sens)*100,1))
tbl2<-rbind(tbl2,tbl2all) #This is the table for corrected data

## Uncorrected
mdluncorrdf<-subset(mdldf,Treatment=="Uncorrected")
umFbsp<-aggregate(Fbeta~SpeciesCode+Model,data=mdluncorrdf,max)
uwFbsp<-reshape(umFbsp,idvar="SpeciesCode",timevar="Model",direction="wide")
names(uwFbsp)<-c("SpeciesCode","MobileNetv2","ResNet101v2","ResNet50v2")
## CANNOT DO SOSP...
uwFbsp<-subset(uwFbsp,SpeciesCode != "SOSP")

#Now loop through each species and identify the top model and the hurdle value for it
umxFbsp<-ldply(uwFbsp$SpeciesCode,function(ss,uwFbsp,plotdataBySpecies,mdluncorrdf){
			tdf<-subset(uwFbsp,SpeciesCode==ss)
			m1<-tdf$MobileNetv2;m1<-ifelse(is.na(m1),0,m1);m2<-tdf$ResNet101v2;m2<-ifelse(is.na(m2),0,m2);m3<-tdf$ResNet50v2;m3<-ifelse(is.na(m3),0,m3)
			if((m1>m2 && m1>m3) | (m1==m2 && m1>m3) | (m1>m2 && m1==m3)){
				pdbs<-subset(mdluncorrdf,Model=="MobileNet::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="MobileNetv2",Fbeta=m1,Threshold=hurdval)
			}else if((m2>m1 && m2>m3) | (m2>m1 && m2==m3) | (m2==m1 && m2>m3)){
				pdbs<-subset(mdluncorrdf,Model=="Resnet101::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet101v2",Fbeta=m2,Threshold=hurdval)
			}else{
				pdbs<-subset(mdluncorrdf,Model=="Resnet50::sigmoid" & SpeciesCode==ss)
				hurdval<-subset(pdbs,Fbeta==max(pdbs$Fbeta,na.rm=T))$hurdle
				rdf<-data.frame(SpeciesCode=ss,ModelName="ResNet50v2",Fbeta=m3,Threshold=hurdval)
			}
			if(nrow(rdf)>1){rdf<-subset(rdf,Threshold==max(rdf$Threshold))}
			return(rdf)
		},uwFbsp=uwFbsp,plotdataBySpecies=plotdataBySpecies,mdluncorrdf=mdluncorrdf)

optimalUncStats <- data.frame()
for(ss in umxFbsp$SpeciesCode){ ## For the appendix!
	uspMxFb<-subset(mdluncorrdf,SpeciesCode==ss)
	uspMxFb$ModelName<-ifelse(uspMxFb$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(uspMxFb$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
	modelN<-subset(umxFbsp,SpeciesCode==ss)$ModelName
	threshV<-subset(umxFbsp,SpeciesCode==ss)$Threshold
	## Need the mdf of the maxBetas...
	mdf<-subset(uspMxFb,hurdle==threshV & ModelName==modelN)
	optimalUncStats <- rbind(optimalUncStats,mdf)
}

# species with enough GV data
optimalUncStats<-subset(optimalUncStats,SpeciesCode %in% gvspp)
tbl2unc<-as.data.frame(optimalUncStats %>% group_by(ModelName) %>% dplyr::summarize(maxFbeta=mean(Fbeta),Precision=mean(Prec),Recall=mean(Sens)))
tbl2unc$maxFbeta<-round(tbl2unc$maxFbeta,2);tbl2unc$Precision<-round(tbl2unc$Precision*100,1);tbl2unc$Recall<-round(tbl2unc$Recall*100,1)
tbl2uncAll<-data.frame(ModelName="All",maxFbeta=round(mean(optimalUncStats$Fbeta),2),Precision=round(mean(optimalUncStats$Prec)*100,1),Recall=round(mean(optimalUncStats$Sens)*100,1))
tbl2unc<-rbind(tbl2unc,tbl2uncAll)  #This is the table for uncorrected data
#################################################

## FIGURE 6
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

#ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/three_species_optimal_threshold.png", width = 7, height = 5, units = "in", dpi=600)


###########################
## FIGURE 7
## Optimal threshold dot-plot by species, 3 models
mFbsp$ModelName<-ifelse(mFbsp$Model=="MobileNet::sigmoid","MobileNetv2",ifelse(mFbsp$Model=="Resnet101::sigmoid","ResNet101v2","ResNet50v2"))
mdpdf<-subset(mFbsp,SpeciesCode!="SOSP")

## Only including the species with enough GV data ##
mdpdf<-subset(mdpdf,SpeciesCode %in% gvspp)

## All species corrected regardless of GV evaluation = 46
## With the above filter - all species with enough GV data to evaluate = 36

mdp1<-ggplot(mdpdf,aes(x=SpeciesCode,y=Fbeta)) + geom_point(aes(color=ModelName),size=3) + coord_flip() +
		theme_bw() + labs(x="",y=paste0("F(","\u03b2","=0.5)"),color="Model") +
		scale_colour_brewer(palette = "Set2") +
		scale_x_discrete(limits = rev(unique(sort(mdpdf$SpeciesCode)))) +
		scale_y_continuous(breaks=seq(0,1,.1))
dev.new();print(mdp1)

#ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/species_optimal_threshold.png", width = 7, height = 7, units = "in", dpi=600)

## Find the threshold for the maxF05 for each species   $
mdpdf$optimalthr<-sapply(1:nrow(mdpdf),function(ii,mdpdf,mdlcorrdf){
					sppc<-mdpdf[ii,"SpeciesCode"]
					mdln<-mdpdf[ii,"Model"]
					betv<-mdpdf[ii,"Fbeta"]
					thrv<-max(subset(mdlcorrdf,SpeciesCode==sppc & Model==mdln & Fbeta==betv)$hurdle)
					return(thrv)
				},mdpdf=mdpdf,mdlcorrdf=mdlcorrdf)
#save(mdpdf,file="C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/species_optimal_threshold.RData")

##########################
## FIGURE 8
mdp2<-ggplot(optimalStats,aes(SpeciesCode,Prec*100, fill=ModelName)) + geom_bar(stat="identity") + 
		geom_hline(yintercept = mean(optimalStats$Prec*100),color="black") + 
		coord_flip() +
		theme_bw() + labs(x="",y="Precision",fill = "Optimal Model") +
		scale_fill_brewer(palette = "Set2") +
		scale_x_discrete(limits = rev(unique(sort(mdpdf$SpeciesCode)))) + 
		scale_y_continuous(breaks=seq(0,100,10))
dev.new();print(mdp2)

mdp3<-ggplot(optimalStats,aes(SpeciesCode,Sens*100, fill=ModelName)) + geom_bar(stat="identity") + 
		geom_hline(yintercept = mean(optimalStats$Sens*100),color="black") + 
		coord_flip() +
		theme_bw() + labs(x="",y="Recall",fill = "Optimal Model") +
		scale_fill_brewer(palette = "Set2") +
		scale_x_discrete(limits = rev(unique(sort(mdpdf$SpeciesCode)))) + 
		scale_y_continuous(breaks=seq(0,100,10))
dev.new();print(mdp3)


mdp4 <- ggarrange(mdp2, mdp3,
		labels = c("A", "B"),
		common.legend = TRUE,
		legend = "bottom",
		ncol = 2, nrow = 1) 
dev.new();print(mdp4)

#ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/species_optimal_threshold_precision_recall.png", width = 7, height = 7, units = "in", dpi=600)

###########################
## FIGURE 3
## Precision-recall plot of test ROIs, all species combined
## Modified from plotPRcurves_V2.R

## Performance data...
load(paste0(pathToLocalGit,"data/performanceData.RData"))

## Logistic correction data
load(paste0(pathToLocalGit,"data/logisticCorrectionData.RData"))

## pretrainedCorrGV and pretrainedUncorrGV are kissing: Model (Species specific), morder (5), Source (=treatment), F10 and F05
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

## load and perpare roi performance data
load(paste0(pathToLocalGit,"data/roilabelmatch.RData"))
pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"
roidata<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_201109_testing.csv"), stringsAsFactors=FALSE)
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


## Add the roi data to alldata...
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

p1da<-ggplot(subset(npdata, Threshold<=0.99),aes(x=Sens*100, y=Prec*100)) + facet_wrap(~Model,ncol=3) + 
		geom_line(aes(color=Source),size=2) +
		labs(x="Recall", y="Precision",color="Test source") +
		theme_bw() +
		scale_colour_brewer(palette = "Set2") +
		scale_y_continuous(breaks=seq(0,100,10))
print(p1da)   #This is Figure 3

#ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/Pre-training_vs_no pre-training.png", width = 7, height = 5, units = "in", dpi=600)

####################################
## FIGURE 4
# Precision vs. threshold - ROIs and soundscape
ptdata = npdata %>% filter(Source == "Soundscape - XC Pre-training" | Source == "ROI - XC Pre-training")

colors = RColorBrewer::brewer.pal(4, "Set2")[1:4]
colors_select = c(colors[2], colors[4])

p2da<-ggplot(subset(ptdata, Threshold<=0.99),aes(x=Threshold, y=Prec*100)) + facet_wrap(~Model,ncol=3) + 
		geom_line(aes(color=Source),size=2) +
		labs(x="Threshold", y="Precision",color="Test source") +
		theme_bw() +
		scale_colour_manual(values=colors_select)
print(p2da)

#ggsave("C:/Users/lsalas/git/Soundscapes2Landscapes/CNN_Bird_Species/Pre-training_precision_vs_threshold.png", width = 7, height = 5, units = "in", dpi=600)
