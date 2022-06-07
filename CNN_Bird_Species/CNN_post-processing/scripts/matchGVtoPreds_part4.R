# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("ggplot2","plyr","doParallel","foreach","data.table","fmsb","rminer","RMySQL")
lapply(libs, require, character.only = TRUE)

## For each species, for each model, and for each hurdle level
# How many GV right, and how many ROI right?
# Then compile the data for the logistic models

## Load the data
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrData_fullHour065.RData")
species<-unique(respdf$birdcode)

## Now prep the data for the logistic models
respdf$bval<-as.factor(as.character(respdf$binval))

## May need the VIF function
vif_func<-function(in_frame,thresh=10,trace=T,...){
	
	if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
	
	#get initial vif value for all comparisons of variables
	vif_init<-NULL
	var_names <- names(in_frame)
	for(val in var_names){
		regressors <- var_names[-which(var_names == val)]
		form <- paste(regressors, collapse = '+')
		form_in <- formula(paste(val, '~', form))
		vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
	}
	vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
	
	if(vif_max < thresh){
		if(trace==T){ #print output of each iteration
			prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
			cat('\n')
			cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
		}
		return(var_names)
	}
	else{
		
		in_dat<-in_frame
		
		#backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
		while(vif_max >= thresh){
			
			vif_vals<-NULL
			var_names <- names(in_dat)
			
			for(val in var_names){
				regressors <- var_names[-which(var_names == val)]
				form <- paste(regressors, collapse = '+')
				form_in <- formula(paste(val, '~', form))
				vif_add<-VIF(lm(form_in, data = in_dat, ...))
				vif_vals<-rbind(vif_vals,c(val,vif_add))
			}
			max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
			
			vif_max<-as.numeric(vif_vals[max_row,2])
			
			if(vif_max<thresh) break
			
			if(trace==T){ #print output of each iteration
				prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
				cat('\n')
				cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
				flush.console()
			}
			
			in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
			
		}
		
		return(names(in_dat))
		
	}
	
}

########################################################################################
## DOne once SKIP - see saved file below
## Loop here
tm<-Sys.time()
modelperf<-data.frame(); spmdllist<-list()
for(ss in species){
	resdf<-data.frame(SpeciesCode=ss,newAcc=0,testAcc=0,deltaAcc=0,N=0,NTP=NA,NFP=NA,note="Default")
	rmdl<-NA
	print(ss)
	#get and prep the data
	spdf<-subset(respdf,birdcode==ss)
	spdf<-spdf[which(!names(spdf) %in% c(ss,"source"))]
	if(nrow(spdf)<50){
		resdf<-data.frame(SpeciesCode=ss,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=NA,NFP=NA,note="Fewer than 50 predictions")
	}else if(sum(spdf$binval==0)<20){
		resdf<-data.frame(SpeciesCode=ss,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=NA,NFP=NA,note="Fewer than 20 false positives")
	}else{
		#vifdf<-spdf[,c(8:63)]
		#modelvars<-vif_func(in_frame=vifdf,thresh=2,trace=F)
		
		fmfml<-paste0("binval~",paste(names(spdf[8:63]),collapse="+"))
		fmfml_f<-paste0("bval~",paste(names(spdf[8:63]),collapse="+"))
		#divide into train and test 
		spdfP<-subset(spdf,binval==1); spdfN<-subset(spdf,binval==0)
		nP<-nrow(spdfP); nN<-nrow(spdfN)
		#even the samples
		if(nP > nN){
			pnR<-nN/nP
			nPT<-nP*pnR; nNT<-nN
		}else if(nN > nP){
			pnR<-nP/nN
			nNT<-nN*pnR; nPT<-nP
		}else{
			nPT<-nP; nNT<-nN
		}
		#if(nP > nN & nN/nP < 0.8){
		#	nPT<-nP; nP<-nN/0.8; nNT<-nN
		#}else if(nN > nP & nP/nN < 0.8){
		#	nNT<-nN; nN<-nP/0.8; nPT<-nP
		#}else{
		#	nPT<-nP; nNT<-nN
		#}
		trainp<-sample(1:nPT, 0.8*nPT); testp<-setdiff(1:nPT, trainp);#testp<-sample(testp, 0.2*nP)
		trainn<-sample(1:nNT, 0.8*nNT); testn<-setdiff(1:nNT, trainn);#testn<-sample(testn, 0.2*nN)
		train<-rbind(spdfP[trainp,],spdfN[trainn,])
		test<-rbind(spdfP[testp,],spdfN[testn,])
		## Metric: accuracy
		thrval<-0.5
		
		##Use RF when possible
		if(nrow(spdf)>=100){
			rfsearch=list(smethod="grid",search=list(mtry=c(2,3,4,5,6,8,10),ntree=c(50,100,200,400,800,1000)),
					convex=0,metric="AUC",method=c("kfold",5)) #mtry is the number of params to vary in each perm; convex=0 means look for global min
			rfom<-try(fit(x = as.formula(fmfml_f), data = train, model = "randomForest", eval_metric = "auc", importance = TRUE,
							search = rfsearch, scale ="none", task = "prob", transform = "none", fdebug=TRUE),silent=TRUE)
			if(!inherits(rfom,"try-error")){
				predrf<-as.data.frame(predict(rfom,newdata=test))
				test$pred<-sapply(1:nrow(test),function(ii,predrf){
							prv<-ifelse(predrf[[ii,"0"]]>predrf[[ii,"1"]],0,1)
							prv<-as.numeric(prv);return(prv)
						},predrf=predrf)
				test$ppv<-ifelse(test$pred>=thrval,1,0)
				newacc<-(sum(test$binval==1 & test$ppv==1) + sum(test$binval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
				testacc<-sum(test$binval)/nrow(test)  # the original accuracy in the test data
				deltaacc<-newacc-testacc
				resdf<-data.frame(SpeciesCode=ss,newAcc=newacc,testAcc=testacc,deltaAcc=deltaacc,N=nrow(spdf),NTP=nPT,NFP=nNT,note="Normal RF prediction")
				rmdl<-rfom
			}else{
				resdf<-data.frame(SpeciesCode=ss,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=nPT,NFP=nNT,note="Could not fit RF model")
			}
		}else{
			#Fit the model as logistic regression and predict to test
			mdl<-try(glm(fmfml,data=train,family="binomial"),silent=TRUE)
			if(!inherits(mdl,"try-error")){
				fmdl<-step(mdl,trace=0)
				test$pred<-predict(fmdl,newdata=test,type="response")
				test$ppv<-ifelse(test$pred>=thrval,1,0)
				newacc<-(sum(test$binval==1 & test$ppv==1) + sum(test$binval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
				testacc_f<-sum(test$binval)/nrow(test)  # the original accuracy in the test data
				
				## Now reduce further to vars with p-val < 0.3 see if it improves
				coefdf<-as.data.frame(summary(fmdl)$coefficients)
				names(coefdf)<-c("est","sdest","zval","pval")
				coefdf$varname<-row.names(coefdf)
				coefdf<-subset(coefdf,pval<0.3)
				cvn<-coefdf$varname; cvn<-subset(cvn,cvn!="(Intercept)")
				rfmfml<-paste("binval ~",paste(cvn,collapse=" + "))
				rmdl<-try(glm(rfmfml,data=train,family="binomial"),silent=TRUE)
				
				if(!inherits(rmdl,"try-error")){ # could fit the reduced model
					test$pred<-predict(rmdl,newdata=test,type="response")
					test$ppv<-ifelse(test$pred>=thrval,1,0)
					newacc<-(sum(test$binval==1 & test$ppv==1) + sum(test$binval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
					testacc<-sum(test$binval)/nrow(test)  # the original accuracy in the test data
					
					if(testacc<testacc_f){  #If it is not better, revert to previous
						test$pred<-predict(fmdl,newdata=test,type="response")
						test$ppv<-ifelse(test$pred>=thrval,1,0)
						newacc<-(sum(test$binval==1 & test$ppv==1) + sum(test$binval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
						testacc<-sum(test$binval)/nrow(test)  # the original accuracy in the test data
						rmdl<-fmdl
					}
				}else{
					test$pred<-predict(fmdl,newdata=test,type="response")
					test$ppv<-ifelse(test$pred>=thrval,1,0)
					newacc<-(sum(test$binval==1 & test$ppv==1) + sum(test$binval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
					testacc<-sum(test$binval)/nrow(test)  # the original accuracy in the test data
					rmdl<-fmdl
				}
				
				deltaacc<-newacc-testacc
				resdf<-data.frame(SpeciesCode=ss,newAcc=newacc,testAcc=testacc,deltaAcc=deltaacc,N=nrow(spdf),NTP=nPT,NFP=nNT,note="Normal prediction")
				
			}else{
				resdf<-data.frame(SpeciesCode=ss,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=nPT,NFP=nNT,note="Failed to fit a model")
			}
		}
		
	}				
	modelperf<-rbind(modelperf,resdf); spmdllist[[ss]]<-rmdl
}
Sys.time()-tm
print(subset(modelperf,!is.na(NTP)))
mean(subset(modelperf,!is.na(NTP))$deltaAcc)

## Because we use te lowest hurdle possible (i.e., no hurdle) 0.65, we maximized finding TP and FP to train the model
## We call it fullHour because we are counting the full hour number of detections of all other species. It is more informative.
save(modelperf,spmdllist,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065.RData")
########################################################################################
load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065.RData")

## Now we need to gather the covariates for the GV data and then correct the predictions...
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/allmaches_65to99999.RData")
gvmatches<-subset(allmatches,modelVersion=="Pre-trained 3 models" & PredictionScoreType=="sigmoid")
gvmatches$Model<-ifelse(gvmatches$ModelName=="cols_54cls_Resnet50","Resnet50::sigmoid",ifelse(gvmatches$ModelName=="cols_54cls_Resnet101","Resnet101::sigmoid","MobileNet::sigmoid"))
gvmatches<-gvmatches[,c(1:10,15)]

## Filter de gv data predictions based on species-model, Assign prediction dataset Id based on the models
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData")
spmodel$PredictionsDatasetId<-ifelse(spmodel$Model=="Resnet50::sigmoid",2,ifelse(spmodel$Model=="Resnet101::sigmoid",4,6))
spmodel<-spmodel[,c("Model","birdcode","PredictionsDatasetId")]; names(spmodel)<-c("Model","SpeciesCode","PredictionsDatasetId")
gvspmatches<-merge(gvmatches,spmodel,by=c("Model","SpeciesCode"),all.x=TRUE)
gvspmatches<-subset(gvspmatches,!is.na(PredictionsDatasetId))
## MUST ADD AudiofileId for each event

######################
## Get the covariate data for the species-events
## For each event need the collection of afids -> eventIddf
gvevents<-unique(gvspmatches$SamplingEvent)
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
eventIddf<-ldply(gvevents,function(ee,con){
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
			afid$SamplingEvent<-ee;afid$isevent<-ifelse(afid$AudiofileId==afeid$AudiofileId,1,0);afid$eventAfid<-afeid$AudiofileId
			return(afid)
		},con=con)
dbDisconnect(con)
Sys.time()-tm

## Merge the AudiofileId to gvspmathes by event:
iseventdf<-subset(eventIddf,isevent==1,select=c("SamplingEvent","AudiofileId"))
gvspmatches<-merge(gvspmatches,iseventdf, by="SamplingEvent",all.x=TRUE)

## Use the hurdle at 0.65 to get one pass at the covariates. These don't change with the hurdling
gvsph<-unique(gvspmatches[,c("SamplingEvent","Model","SpeciesCode","PredictionsDatasetId","AudiofileId")])

hurdval<-0.9*10^7
sppdf<-data.frame(SpeciesCode=spmodel$SpeciesCode)
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
gvcovardf<-ldply(1:nrow(gvsph),function(dd,gvsph,con,sppdf,hurdval,eventIddf){
			sev<-as.character(gvsph[dd,"SamplingEvent"])
			afv<-as.character(gvsph[dd,"AudiofileId"])
			
			#subset eventIddf to get the afids for the current event
			afid<-subset(eventIddf,eventAfid==afv)$AudiofileId;afid<-paste(afid,collapse=",")
			
			pdid<-as.numeric(gvsph[dd,"PredictionsDatasetId"])
			spcd<-as.character(gvsph[dd,"SpeciesCode"])
			rdf<-gvsph[dd,]
			
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
		},gvsph=gvsph,sppdf=sppdf,con=con,hurdval=hurdval,eventIddf=eventIddf)
dbDisconnect(con)
Sys.time()-tm

## Now take each species' model and predict on gvcovardf
gvpredadj<-ldply(spmodel$SpeciesCode,function(ss,spmdllist,gvcovardf){
			print(ss)
			spmdl<-spmdllist[[ss]]
			spdf<-subset(gvcovardf,SpeciesCode==ss)
			if(!is.null(spmdl) && class(spmdl)!="logical" && nrow(spdf)>0){
				if(class(spmdl)[1]=="glm"){
					preddf<-data.frame(pred=predict(spmdl,newdata=spdf,type="response"))
					preddf$P<-ifelse(preddf$pred<0.5,0,1)
				}else{
					preddf<-as.data.frame(predict(spmdl,newdata=spdf));names(preddf)<-c("N","Y")
					preddf$P<-ifelse(preddf$N>preddf$Y,0,1)
				}
				spdf$adjust<-preddf$P
			}else if(nrow(spdf)>0){
				spdf$adjust<-1
			}else{
				spdf<-data.frame()
			}
			
			return(spdf)
		},spmdllist=spmdllist,gvcovardf=gvcovardf)

## Now we merge to gvspmatches
gvspmatches<-merge(gvspmatches,gvpredadj[,c("SamplingEvent","Model","SpeciesCode","PredictionsDatasetId","AudiofileId","adjust")],by=c("SamplingEvent","Model","SpeciesCode","PredictionsDatasetId","AudiofileId"),all.x=T)
gvspadjmatches<-ldply(1:nrow(gvspmatches),function(rr,gvspmatches){
			dd<-gvspmatches[rr,]
			if(dd$adjust==0){
				dd$SpeciesCode<-NA
				dd$match<-ifelse(dd$match=="FALSEPOS","TRUENEG",ifelse(dd$match=="TRUEPOS","FALSENEG","TRUENEG"))
			}
			return(dd)
		},gvspmatches=gvspmatches)

## Ready to summarize to plot!
## Need a function for summary by species
summarizeToSampleAllSpecies<-function(matches){
	hurdsample<-ldply(unique(matches$hurdle),function(hh,matches){
				hmatches<-subset(matches, hurdle==hh)
					trval<-sum(hmatches$match=="TRUEPOS")
					fnval<-sum(hmatches$match=="FALSENEG")
					fpval<-sum(hmatches$match=="FALSEPOS")
					f1val<-trval/((trval*2)+fnval+fpval)
					sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
					prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
					miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
					fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
					specdf<-data.frame(count=nrow(hmatches),truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Threshold=hh)
				return(specdf)
			},matches=matches)
	return(hurdsample)
}

metrics<-summarizeToSampleAllSpecies(matches=gvspmatches); metrics$treatment<-"Uncorrected"
metricsadj<-summarizeToSampleAllSpecies(matches=gvspadjmatches); metricsadj$treatment<-"Corrected"
plotdf<-rbind(metrics,metricsadj)


## let's bun the miss rate so it's easy to see in the plot
p1<-ggplot(plotdf,aes(x=Threshold, y=Prec)) + 
		geom_line(aes(color=treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw()
p2<-ggplot(plotdf,aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw()
print(p1)
dev.new();print(p2)

pretrainedUncorrGV<-metrics
pretrainedCorrGV<-metricsadj
save(pretrainedUncorrGV,pretrainedCorrGV,file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/logisticCorrectionData.RData")

## This variation is for the labeled data, adjusted...
summarizeLabeledToSampleAllSpeciesLogisticCorrected<-function(gvspmatches,hurdles){
	hurdsample<-ldply(unique(hurdles),function(hh,gvspmatches){
				hv<-hh*(10^7)
				hmatches<-subset(gvspmatches, PredictionScore>=hv)
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

