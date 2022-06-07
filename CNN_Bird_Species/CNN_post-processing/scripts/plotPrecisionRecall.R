# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## We want Shree's ROIs, GV (all data), labeled (all data), BirdNET, labeled-selected, labeled-selected-corrected, and GV corrected

libs<-c("ggplot2","plyr","RMySQL","data.table","reticulate","dplyr")
lapply(libs, require, character.only = TRUE)

load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData")
spmodel$speciesmodel<-paste0(spmodel$birdcode,"::",spmodel$Model)

####################################################################################################################################################################
## Get class names for Shree's ois
source_python("c:/users/lsalas/git/S2L_devel/GVanalyses/PreTrainEval/pickle_joblib_reader.py")


datapath<-"c:/users/lsalas/downloads/pkl_for_Leo/"
modelsdf<-data.frame(models=c("mobilenet", "resnet_50","resnet_101"),Models=c("cols_54cls_mobnet_full_finetune", "cols_54cls_Resnet50","cols_54cls_Resnet101"))
outtypes<-c("sigmoid","softmax")

## These are 2695 x 54 matrices
## Use the CLASS_NAMES.pkl to retrieve the names of columns (i.e., spp)
filepath<-paste0(datapath,"CLASS_NAMES.pkl")
classnames<-as.character(read_pickle_file(filepath)) #column names (54)


########################################
## Need these functions:

## Need a function for overall summary
summarizeToSampleAllSpecies<-function(matches){
	hurdsample<-ldply(unique(matches$hurdle),function(hh,matches){
				hv<-as.character(hh)
				hmatches<-subset(matches, hurdle==hv)
				sumToSample<-ldply(unique(hmatches$Model),function(mm,hmatches){
							modeldf<-subset(hmatches,Model==mm)
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
							specdf$Model<-mm
							return(specdf)
						},hmatches=hmatches)
				sumToSample$Threshold<-hv
				return(sumToSample)
			},matches=matches)
	return(hurdsample)
}

## Function to convert Shree's matrices into something we can use

## Function to read a pickle file and return content
readPickle<-function(datapath,model,outtype,classnames){
	filepath<-paste0(datapath,model,"/",model,"_",outtype,".pkl")
	pdat<-as.data.frame(read_pickle_file(filepath))
	names(pdat)<-classnames
	return(pdat)
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

## This function subsets the data by the species and model
subsetBySpeciesModel<-function(df,spmodel,spfield="birdcode",modelfield="Model"){
	df$speciesmodel<-paste0(df[,spfield],"::",df[,modelfield])
	dff<-subset(df,speciesmodel %in% spmodel$speciesmodel)
	dff<-dff[,which(names(dff)!="speciesmodel")]
	return(dff)
}

############################################################################################################################
### Plot Shree's ROIs

## Use the truelabel pickle file to find out the true species in each of the 2695 ROIs being tested for each model
truelabels<-ldply(modelsdf$models,function(mm,datapath,outtypes,classnames){
			truem<-readPickle(datapath=datapath,model=mm,outtype="truelabel",classnames=classnames)
			truev<-apply(truem,1,function(x,classnames){
						tdf<-data.frame(val=as.numeric(x),name=names(x))
						spv<-subset(tdf,val==1)$name
						return(spv)
					},classnames=classnames)
			truedf<-data.frame(label=truev,model=mm)
			truedf$recId<-1:nrow(truedf)
			return(truedf)
		},datapath=datapath,outtypes=outtypes,classnames=classnames)

## Then read from each model and output to retrieve max score and corresponding species
predsroi<-ldply(modelsdf$models,function(mm,datapath,outtypes,classnames){
			predout<-data.frame()
			for(tt in outtypes){
				modelm<-readPickle(datapath=datapath,model=mm,outtype=tt,classnames=classnames)
				preddf<-ldply(1:nrow(modelm),function(rr,modelm){
							x<-modelm[rr,]
							tdf<-data.frame(score=as.numeric(x),species=names(x))
							spdf<-subset(tdf,score==max(score))
							return(spdf)
						},modelm=modelm)
				preddf$outtype<-tt
				preddf$recId<-1:nrow(preddf)
				predout<-rbind(predout,preddf)
			}
			predout$model<-mm
			return(predout)
		},datapath=datapath,outtypes=outtypes,classnames=classnames)

## Then create a table that has model, output, species, score, truelabel, match
predsroidf<-merge(predsroi,truelabels,by=c("recId","model"),all.x=TRUE)

## Finally take each row in predsroidf and produce a match type: TRUEPOS if species=label, else FALSEPOS for species and FALSENEG for label
roires<-ldply(1:nrow(predsroidf),function(rr,predsroidf){
			mod<-as.character(predsroidf[rr,"model"]);scor<-predsroidf[rr,"score"]
			spec<-as.character(predsroidf[rr,"species"]);otyp<-as.character(predsroidf[rr,"outtype"])
			labv<-as.character(predsroidf[rr,"label"]);recid<-predsroidf[rr,"recId"]
			if(spec==labv){
				tdf<-data.frame(model=mod,outtype=otyp,recId=recid,species=spec,score=scor,match="TRUEPOS")
			}else{
				tdf<-data.frame(model=c(mod,mod),outtype=c(otyp,otyp),recId=c(recid,recid),species=c(spec,labv),score=c(scor,1),match=c("FALSEPOS","FALSENEG"))
			}
			return(tdf)
		},predsroidf=predsroidf)
roires$Model<-as.character(paste0(roires$model,"::",roires$outtype))
roires$Model<-gsub("mobilenet","Mobilenet",roires$Model)
roires$Model<-gsub("resnet_","Resnet",roires$Model)
roires$Model<-ifelse(roires$Model=="Mobilenet::sigmoid","MobileNet::sigmoid",roires$Model)

## To plot, we need to increase the hurdle: any true positive not passing the hurdle becomes a false negative; any false positive not passing the hurdleis ignored
hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)
## This function calculates Precision, Sensitivity, Recall/Specificity, and the Miss rate for a particular thresholded set of results
sumrois<-ldply(hurdvals,function(hh,roires){
			sumhurdlst<-list()
			hv<-as.character(hh)
			hmatches<-subset(roires, score>=hh)
			hbelow<-subset(roires, score<hh)
			sumToSample<-ldply(unique(hmatches$Model),function(mm,hmatches,hbelow){
						modeldf<-subset(hmatches,Model==mm)
						belowdf<-subset(hbelow,Model==mm)
						trval<-sum(modeldf$match=="TRUEPOS"); 
						fnval<-sum(modeldf$match=="FALSENEG") + sum(belowdf$match=="TRUEPOS")
						fpval<-sum(modeldf$match=="FALSEPOS")
						f1val<-trval/((trval*2)+fnval+fpval)
						sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
						prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
						miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
						fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
						specdf<-data.frame(count=nrow(modeldf),truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
						specdf$Model<-mm
						return(specdf)
					},hmatches=hmatches,hbelow=hbelow)
			sumToSample$Threshold<-hh
			sumhurdlst[[hv]]<-sumToSample
			hurdsample<-rbindlist(sumhurdlst)
			return(hurdsample)
		},roires=roires)

modelorder<-data.frame(Model=unique(sumrois$Model),morder=c(3,6,2,5,1,4))
allsprois<-merge(sumrois,modelorder,by="Model", all.x=T)
allsprois$Model<-as.factor(allsprois$Model)
allsprois$Model<-reorder(allsprois$Model,allsprois$morder)

## let's bun the miss rate so it's easy to see in the plot
p1<-ggplot(allsprois,aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "white", mid="black", high = "#f04911") + 
		geom_line(aes(color=Miss),size=2) +
		scale_fill_brewer() + labs(x="Threshold", y="Precision",color="Miss rate")
p2<-ggplot(allsprois,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "white", mid="black", high = "#f04911") + 
		geom_line(aes(color=Threshold),size=2,) +
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Threshold")
print(p1)
dev.new();print(p2)

##### 
##### allsprois is the df of performance of pretrained models vs Shree's rois.
## Using only the new models:
allsprois<-subset(allsprois,grepl("sigmoid",Model))
allsprois$Source<-"Shree's ROIs"
pretrainedShree<-allsprois

alldata<-pretrainedShree

########################################################################################################################################################
## Plotting the GV data results

## Load the data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/allmaches_65to99999.RData")

metricsall$Threshold<-as.numeric(as.character(metricsall$Threshold))

allgvplot<-merge(metricsall,modelorder,by="Model", all.x=T)
allgvplot$Model<-as.factor(allgvplot$Model)
allgvplot$Model<-reorder(allgvplot$Model,allgvplot$morder)

## let's add the miss rate so it's easy to see in the plot
p1<-ggplot(subset(allgvplot,hurdle<0.999),aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "white", mid="black", high = "#f04911") + 
		geom_line(aes(color=Miss),size=2) +
		scale_fill_brewer() + labs(x="Threshold", y="Precision",color="Miss rate")
p2<-ggplot(allgvplot,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		#scale_color_gradient2(low = "white", mid="black", high = "#f04911") + 
		geom_line(aes(color=Threshold),size=2) +
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Threshold")
print(p1)
dev.new();print(p2)


####
#### allsp is the pretrained model performance against GV
allgvplot$Source<-"Golden validations"
pretrainedGV<-allgvplot[,names(pretrainedShree)]

alldata<-rbind(alldata,pretrainedGV)

#########################################################################################################################################
## Plotting the labeled data results...
load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/roilabelmatch.RData")

## CAREFUL: need to use only positive matches  DONE
## CAREFUL: need to remove dupes  DONE

nrow(resdf)
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

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)

lmetsdf<-summarizeLabeledToSampleAllSpecies(matches=resdf,hurdles=hurdvals)

modelorder<-data.frame(Model=unique(lmetsdf$Model),morder=c(4,1,5,2,6,3))
labelroisp<-merge(lmetsdf,modelorder,by="Model", all.x=T)
labelroisp$Model<-as.factor(labelroisp$Model)
labelroisp$Model<-reorder(labelroisp$Model,labelroisp$morder)

p2<-ggplot(labelroisp,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "white", mid="black", high = "#f04911") + 
		geom_line(aes(color=Threshold),size=2,) +
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Threshold")

####
#### labelroisp is the performance data of pretraind models against the labeled ROIs
labelroisp<-subset(labelroisp,grepl("sigmoid",Model))
labelroisp$Source<-"Labeled data"
pretrainedLabeled<-labelroisp[,names(pretrainedShree)]

alldata<-rbind(alldata,pretrainedLabeled)

#####################################
## Add the BirdNet predictions to the plot
load("c:/users/lsalas/git/S2L_devel/GVanalyses/BirdNet/BirdNET_PR_curve.RData")
load("c:/users/lsalas/git/S2L_devel/GVanalyses/BirdNet/BirdNET_roi_plot.RData")
bnplot$morder<-4; bnplot$Model<-"BirdNET"; bnplot$Source<-"Golden validations"
bnroiplot$morder<-4; bnroiplot$Model<-"BirdNET"; bnroiplot$Source<-"Labeled data"

bndata<-rbind(bnplot,bnroiplot)

p1<-ggplot(bndata,aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "black", mid="blue", high = "red") + 
		geom_point(aes(color=Miss,shape=Source),size=2) +
		scale_fill_brewer() + labs(x="Threshold", y="Precision",color="Miss rate",size="Test source") +
		theme_bw()
p2<-ggplot(bndata,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		#scale_color_gradient2(low = "black", mid="blue", high = "gray") + 
		geom_line(aes(color=Source),size=2) + #Threshold,shape=
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Test source") + #"Threshold",shape=
		theme_bw()
print(p1)
dev.new();print(p2)


####
#### bnplot is the performance of BirdNet vs GV
#### bnroiplot is the performance of BirdNET vs labeled rois
birdnetGV<-bnplot[,names(pretrainedShree)]
birdnetLabeled<-bnroiplot[,names(pretrainedShree)]

alldata<-rbind(alldata,birdnetLabeled)
alldata<-rbind(alldata,birdnetGV)


#####################################   
## Add the performance of the uncorrected - already corrected for species-model
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/noPretrainPerformance.RData")

summarizeLabeledToSampleAllSpeciesUC<-function(matches,hurdles){
	hurdsample<-ldply(unique(hurdles),function(hh,matches){
				hv<-hh*(10^7)
				hmatches<-subset(matches, PredictionScore>=hv)
				hbelow<-subset(matches, PredictionScore<hv)
				sumToSample<-ldply(unique(hmatches$ModelName),function(mm,hmatches){
							modeldf<-subset(hmatches,ModelName==mm)
							belowdf<-subset(hbelow,ModelName==mm)
							trval<-sum(modeldf$match=="TRUEPOS")
							fnval<-sum(modeldf$match=="FALSENEG") + sum(belowdf$match=="TRUEPOS")
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

gvNoPretrainPerformance<-summarizeLabeledToSampleAllSpeciesUC(matches=gvmatchesNoPretrain,hurdles=hurdvals)
gvNoPretrainPerformance$Source<-"Golden validations NPT"

roiNoPretrainPerformance<-summarizeLabeledToSampleAllSpeciesUC(matches=roimatchesNoPretrain,hurdles=hurdvals)
roiNoPretrainPerformance$Source<-"Labeled data NPT"

npplot<-rbind(gvNoPretrainPerformance,roiNoPretrainPerformance)
npplot<-merge(npplot,modelorder,by="Model", all.x=T)
npplot$Model<-as.factor(npplot$Model)
npplot$Model<-reorder(npplot$Model,npplot$morder)


p1<-ggplot(npplot,aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "black", mid="blue", high = "red") + 
		geom_point(aes(color=Miss,shape=Source),size=2) +
		scale_fill_brewer() + labs(x="Threshold", y="Precision",color="Miss rate",size="Test source") +
		theme_bw()
p2<-ggplot(npplot,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		#scale_color_gradient2(low = "black", mid="blue", high = "gray") + 
		geom_line(aes(color=Source),size=2) + #Threshold,shape=
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Test source") + #"Threshold",shape=
		theme_bw()
print(p1)
dev.new();print(p2)

noPretrainPerformance<-npplot[,names(pretrainedShree)]
alldata<-rbind(alldata,noPretrainPerformance)


#########################################

## Plot all results combined
p1<-ggplot(alldata,aes(x=Threshold, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "black", mid="blue", high = "red") + 
		geom_point(aes(color=Miss,shape=Source),size=2) +
		scale_fill_brewer() + labs(x="Threshold", y="Precision",color="Miss rate",size="Test source") +
		theme_bw()
p2<-ggplot(alldata,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		geom_line(aes(color=Source),size=2) + #Threshold,shape=
		scale_fill_brewer() + labs(x="Recall", y="Precision",color="Test source") + #"Threshold",shape=
		theme_bw()
print(p1)
dev.new();print(p2)


#########################################
## Create table of indices for paper
getFval<-function(df,beta){
	fv<-(1+(beta^2))*(df$Prec*df$Sens)/(((beta^2)*df$Prec) + df$Sens)
	return(fv)
}

alldata$F10v<-sapply(1:nrow(alldata),function(rr,alldata,getFval){
			df<-alldata[rr,]
			fval<-getFval(df=df,beta=1)
			return(fval)
		},alldata=alldata,getFval=getFval)

alldata$F05v<-sapply(1:nrow(alldata),function(rr,alldata,getFval){
			df<-alldata[rr,]
			fval<-getFval(df=df,beta=0.5)
			return(fval)
		},alldata=alldata,getFval=getFval)

f05maxdf<-aggregate(F05v~Model+Source,alldata,max)
f05maxdf$Prec05<-sapply(1:nrow(f05maxdf),function(rr,f05maxdf,alldata){
			mdl<-f05maxdf[rr,"Model"];src<-f05maxdf[rr,"Source"];f05val<-f05maxdf[rr,"F05v"]
			tdf<-subset(alldata,Model==mdl & Source==src & F05v==f05val)
			prv<-max(tdf$Prec)
			return(prv)
		},f05maxdf=f05maxdf,alldata=alldata)

f10maxdf<-aggregate(F10v~Model+Source,alldata,max)
f10maxdf$Prec10<-sapply(1:nrow(f10maxdf),function(rr,f10maxdf,alldata){
			mdl<-f10maxdf[rr,"Model"];src<-f10maxdf[rr,"Source"];f10val<-f10maxdf[rr,"F10v"]
			tdf<-subset(alldata,Model==mdl & Source==src & F10v==f10val)
			prv<-max(tdf$Prec)
			return(prv)
		},f10maxdf=f10maxdf,alldata=alldata)

rec20df<-ldply(unique(alldata$Model),function(mm,alldata){
			mdf<-subset(alldata,Model==mm)
			sdf<-data.frame()
			for(ss in unique(mdf$Source)){
				tdf<-subset(mdf,Source==ss)
				tdf<-subset(tdf,Sens>=0.2)
				tdf<-tdf[order(tdf$Sens),]
				prv<-tdf[1,"Prec"]; rcv<-tdf[1,"Sens"]
				rdf<-data.frame(Model=mm,Source=ss,Rec20=rcv,Prec20Rec=prv)
				sdf<-rbind(sdf,rdf)
			}
			return(sdf)
		},alldata=alldata)
rec20df<-subset(rec20df,Source!="Labeled data" | Model!="BirdNET")

rec50df<-ldply(unique(alldata$Model),function(mm,alldata){
			mdf<-subset(alldata,Model==mm)
			sdf<-data.frame()
			for(ss in unique(mdf$Source)){
				tdf<-subset(mdf,Source==ss)
				tdf<-subset(tdf,Sens>=0.5)
				tdf<-tdf[order(tdf$Sens),]
				prv<-tdf[1,"Prec"]; rcv<-tdf[1,"Sens"]
				rdf<-data.frame(Model=mm,Source=ss,Rec50=rcv,Prec50Rec=prv)
				sdf<-rbind(sdf,rdf)
			}
			return(sdf)
		},alldata=alldata)
rec50df<-subset(rec50df,Source!="Labeled data" | Model!="BirdNET")
rec50df$Rec50<-round(rec50df$Rec50,3); rec50df$Prec50Rec<-round(rec50df$Prec50Rec,3)

indicesdf<-merge(f05maxdf,f10maxdf,by=c("Model","Source"))
indicesdf<-merge(indicesdf,rec20df,by=c("Model","Source"))
indicesdf<-merge(indicesdf,rec50df,by=c("Model","Source"))
indicesdf<-subset(indicesdf,!grepl("softmax",Model))

for(nn in c("F05v","Prec05","F10v","Prec10","Rec20","Prec20Rec")){
	indicesdf[,nn]<-round(indicesdf[,nn],3)
}
indicesdf<-indicesdf[order(indicesdf$Source,indicesdf$Model),]


## Saving all the data into an RDatafile...
save(pretrainedShree,pretrainedGV,pretrainedLabeled,birdnetGV,birdnetLabeled,gvNoPretrainPerformance,roiNoPretrainPerformance,alldata,indicesdf,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/performanceData.RData")

