# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## In this file we fit logistic correction models for each species where possible, for each CNN model where possible
## We store these models and use them to correct the database predictions

## The logic is to loop through each model and each species and fit according to amount of data available.


libs<-c("ggplot2","plyr","doParallel","foreach","data.table","fmsb","rminer","RMySQL")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

## For each species, for each model, and for each hurdle level
# How many GV right, and how many ROI right?
# Then compile the data for the logistic models

#pathToLocalGit<-"c:/users/lsalas/git/S2L_devel/"
#pathToLocalGit<-"/home/ubuntu/S2L_devel/"
pathToLocalGit<-"c:/users/salasle/git/S2L_devel/"

## Load the data
load(file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logCorr_modelCovarData_10222021.RData"))
species<-unique(c(mdlspcovlst[[1]]$SpeciesCode,mdlspcovlst[[2]]$SpeciesCode,mdlspcovlst[[3]]$SpeciesCode))	# 46 species
models<-names(mdlspcovlst)

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
## Done once SKIP - see saved file below
## Loop here
tm<-Sys.time()
modelperf<-data.frame(); mdlnmlst<-list()
for(mm in models){
	respdf<-mdlspcovlst[[mm]]
	respdf$bval<-as.factor(as.character(respdf$matchval))
	spmdllist<-list()
	for(ss in species){
		resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel=NA,newAcc=0,testAcc=0,deltaAcc=0,N=0,NTP=NA,NFP=NA,note="Default")
		rmdl<-NA
		print(paste(mm,ss))
		#get and prep the data
		spdf<-subset(respdf,SpeciesCode==ss)
		spdf<-spdf[which(!names(spdf) %in% c(ss,"source"))]
		if(nrow(spdf)<50){
			resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel=NA,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=NA,NFP=NA,note="Fewer than 50 predictions")
		}else if(sum(spdf$matchval==0)<20){
			resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel=NA,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=NA,NFP=NA,note="Fewer than 20 false positives")
		}else{
			#vifdf<-spdf[,c(8:63)]
			#modelvars<-vif_func(in_frame=vifdf,thresh=2,trace=F)
			
			fmfml<-paste0("matchval~",paste(names(spdf[7:62]),collapse="+"))
			fmfml_f<-paste0("bval~",paste(names(spdf[7:62]),collapse="+"))
			#divide into train and test 
			spdfP<-subset(spdf,matchval==1); spdfN<-subset(spdf,matchval==0)
			nP<-nrow(spdfP); nN<-nrow(spdfN)
			trainp<-sample(1:nP, 0.8*nP); testp<-setdiff(1:nP, trainp)
			trainn<-sample(1:nN, 0.8*nN); testn<-setdiff(1:nN, trainn)
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
					newacc<-(sum(test$matchval==1 & test$ppv==1) + sum(test$matchval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
					testacc<-sum(test$matchval)/nrow(test)  # the original accuracy in the test data
					deltaacc<-newacc-testacc
					resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel="RF",newAcc=newacc,testAcc=testacc,deltaAcc=deltaacc,N=nrow(spdf),NTP=nP,NFP=nN,note="Normal RF prediction")
					rmdl<-rfom
				}else{
					resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel=NA,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=nP,NFP=nN,note="Could not fit RF model")
				}
			}else{
				#Fit the model as logistic regression and predict to test
				mdl<-try(glm(fmfml,data=train,family="binomial"),silent=TRUE)
				if(!inherits(mdl,"try-error")){
					fmdl<-step(mdl,trace=0)
					test$pred<-predict(fmdl,newdata=test,type="response")
					test$ppv<-ifelse(test$pred>=thrval,1,0)
					newacc<-(sum(test$matchval==1 & test$ppv==1) + sum(test$matchval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
					testacc_f<-sum(test$matchval)/nrow(test)  # the original accuracy in the test data
					
					## Now reduce further to vars with p-val < 0.3 see if it improves
					coefdf<-as.data.frame(summary(fmdl)$coefficients)
					names(coefdf)<-c("est","sdest","zval","pval")
					coefdf$varname<-row.names(coefdf)
					coefdf<-subset(coefdf,pval<0.3)
					cvn<-coefdf$varname; cvn<-subset(cvn,cvn!="(Intercept)")
					rfmfml<-paste("matchval ~",paste(cvn,collapse=" + "))
					rmdl<-try(glm(rfmfml,data=train,family="binomial"),silent=TRUE)
					
					if(!inherits(rmdl,"try-error")){ # could fit the reduced model
						test$pred<-predict(rmdl,newdata=test,type="response")
						test$ppv<-ifelse(test$pred>=thrval,1,0)
						newacc<-(sum(test$matchval==1 & test$ppv==1) + sum(test$matchval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
						testacc<-sum(test$matchval)/nrow(test)  # the original accuracy in the test data
						
						if(testacc<testacc_f){  #If it is not better, revert to previous
							test$pred<-predict(fmdl,newdata=test,type="response")
							test$ppv<-ifelse(test$pred>=thrval,1,0)
							newacc<-(sum(test$matchval==1 & test$ppv==1) + sum(test$matchval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
							testacc<-sum(test$matchval)/nrow(test)  # the original accuracy in the test data
							rmdl<-fmdl
						}
					}else{
						test$pred<-predict(fmdl,newdata=test,type="response")
						test$ppv<-ifelse(test$pred>=thrval,1,0)
						newacc<-(sum(test$matchval==1 & test$ppv==1) + sum(test$matchval==0 & test$ppv==0))/nrow(test)  # (TP predicted as Positive + FP predicted as Negative)/sample size
						testacc<-sum(test$matchval)/nrow(test)  # the original accuracy in the test data
						rmdl<-fmdl
					}
					
					deltaacc<-newacc-testacc
					resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel="GLM",newAcc=newacc,testAcc=testacc,deltaAcc=deltaacc,N=nrow(spdf),NTP=nP,NFP=nN,note="Normal prediction")
					
				}else{
					resdf<-data.frame(ModelName=mm,SpeciesCode=ss,logModel=NA,newAcc=0,testAcc=0,deltaAcc=0,N=nrow(spdf),NTP=nP,NFP=nN,note="Failed to fit a logistic model")
				}
			}
			
		}				
		modelperf<-rbind(modelperf,resdf); spmdllist[[ss]]<-rmdl
	}
	mdlnmlst[[mm]]<-spmdllist
}
Sys.time()-tm
print(subset(modelperf,!is.na(NTP)))
mean(subset(modelperf,!is.na(NTP))$deltaAcc)

## Because we use the lowest hurdle possible (i.e., no hurdle) 0.65, we maximized finding TP and FP to train the model
## We call it fullHour because we are counting the full hour number of detections of all other species. It is more informative.
save(modelperf,mdlnmlst,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_10222021.RData"))
########################################################################################

## Apply to GV data
###################
load(file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_10222021.RData"))

## Now we need to gather the covariates for the GV data and then correct the predictions...
load(file=paste0(pathToLocalGit,"GVanalyses/data/modelsDataAll/allmaches_65to99999_040122.RData"))
## Use tstmatches, because these are from the database test_s2l, which is the pretrained results, see lines 394 and 415 0f matchGVtoPreds_par2.R
gvmatches<-subset(tstmatches,modelVersion=="Pre-trained 3 models" & PredictionScoreType=="sigmoid")  #Not using the ones pre-filtered by "best model for the species"
gvmatches$Model<-ifelse(gvmatches$ModelName=="cols_54cls_Resnet50","Resnet50::sigmoid",ifelse(gvmatches$ModelName=="cols_54cls_Resnet101","Resnet101::sigmoid","MobileNet::sigmoid"))
gvmatches<-gvmatches[,c(2:11,14)]
for(mmm in names(gvmatches)){if(class(gvmatches[,mmm])=="factor"){gvmatches[,mmm]<-as.character(gvmatches[,mmm])}}
## Ensure "match" has a value, using "matchDelta" values when there is an NA. REVIEW carefully.
## ALL THESE NAs seem to be matchDelta 1, not 9, and all TRUEPOS
if(sum(is.na(gvmatches$match))){
	gvmna<-subset(gvmatches,is.na(match))
	gvmna$matchDelta<-1;gvmna$match<-"TRUEPOS";gvmna$GVsecond<-gvmna$Second-1
	gvmnotna<-subset(gvmatches,!is.na(match))
	gvmatches<-rbind(gvmna,gvmnotna)
}

######################
## Get the covariate data for the species-events
## For each event need the collection of afids -> eventIddf
gvevents<-unique(gvmatches$SamplingEvent)
tm<-Sys.time()
#con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
con <- dbConnect(RMySQL::MySQL(), user = "lsalas", password = "&pBtt2G.", host = "localhost", port = 3306, dbname = "s2l_test")
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
Sys.time()-tm # 7.5 minutes

## Merge the AudiofileId to gvspmathes by event:
iseventdf<-subset(eventIddf,isevent==1,select=c("SamplingEvent","AudiofileId"))
gvspmatches<-merge(gvmatches,iseventdf, by="SamplingEvent",all.x=TRUE)
# So now all predictions (TP, FP and FN) in gvmatches have an audiofileId 

## Use the fields we need, only for the true events in GV and not the sorrounding events...
gvsph<-unique(gvspmatches[,c("SamplingEvent","Model","SpeciesCode","AudiofileId")])
## Need to add PredictionsDatasetId based on Model and Year, except that Year is < 2020, so...
gvsph$PredictionsDatasetId<-ifelse(gvsph$Model=="Resnet50::sigmoid",2,ifelse(gvsph$Model=="Resnet101::sigmoid",4,6))  #Only using sigmoid output

## Need this...
load(file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/chosenSpeciesModel.RData"))
sppdf<-data.frame(SpeciesCode=spmodel$birdcode)

##We take each record in gvsph, add the reminder of the covariates at each hurdle level from 0.65 to 0.99 - 
tm<-Sys.time()
con <- dbConnect(RMySQL::MySQL(), user = "lsalas", password = "&pBtt2G.", host = "localhost", port = 3306, dbname = "s2l_test")

gvcovarlst<-llply(c(0.65,0.80,0.95,0.99),function(hh,con,gvsph,eventIddf){    #seq(from=0.65,to=0.99,by=0.01)
			hurdval<-hh*10^7
			#con <- dbConnect(RMySQL::MySQL(), user = "leo", password = "Drs7Q59ZPfpcYzFW", host = "s2l-db-01.cuttlb1a3ul1.us-east-1.rds.amazonaws.com", port = 3306, dbname = "s2l_test")
			gvcovardf<-ldply(1:nrow(gvsph),function(dd,gvsph,con,hurdval,sppdf,eventIddf){
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
			return(gvcovardf)
		},con=con,gvsph=gvsph,eventIddf=eventIddf)
dbDisconnect(con)
Sys.time()-tm  #2.5 minutes
names(gvcovarlst)<-paste0("h",c(65,80,95,99))

## Now take each species and hurdle level and predict on gvcovardf for all three models if available
# need to loop again trhough each hurdle level
# So now the list hierarchy is SpeciesCode -> ModelName -> Hurdle value

#library(doParallel);library(foreach)
#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

tm<-Sys.time()
gvpreadadjlst<-llply(c(0.65,0.80,0.95,0.99),function(hh,sppdf,mdlnmlst,gvcovarlst,gvspmatches){   #seq(from=0.65,to=0.99,by=0.01)
			hnam<-paste0("h",(hh*100))
			gvcovardf<-gvcovarlst[[hnam]]
			
			## gvcovardf is the list of prediction covariates that is unique to each event, model, and species, so...
			## We can link the values in gvspmatches to all three, or to species and event only.
			## If we choose to only apply to species and event, there will be many cases where there is no model X covariates for species S and event E
			## We use the exact match of species, event, and model
			
			models<-names(mdlnmlst)
			# Need to loop through all three models = mm
			predadjmdllst<-list()
			for(mm in models){	#loop through models because the predicting model is specific to AI model
				mnam<-substr(mm,1,regexpr("::",mm)-1)
				gvcovarmdl<-subset(gvcovardf,Model==mm)
				spmdllist<-mdlnmlst[[mm]]
				species<-as.character(unique(gvcovarmdl$SpeciesCode))
				#parallelize this one!!
				gvpredadj<-ldply(.data=species,.parallel=FALSE,.fun=function(ss,spmdllist,gvcovarmdl){ #loop through species because the predicting model is species specific
							spmdl<-spmdllist[[ss]]
							spdf<-subset(gvcovarmdl,SpeciesCode==ss)
							if(nrow(spdf)>0){
								adjresdf<-ldply(c(0.5,0.65,0.75,0.85,0.95,0.99),function(lchurd,spmdl,spdf){
											rpdf<-spdf
											if(!is.null(spmdl) && class(spmdl)!="logical" && nrow(spdf)>0){
												if(class(spmdl)[1]=="glm"){
													preddf<-data.frame(pred=predict(spmdl,newdata=spdf,type="response"))
													rpdf$P<-ifelse(preddf$pred<lchurd,0,1)
												}else{
													preddf<-as.data.frame(predict(spmdl,newdata=spdf));names(preddf)<-c("N","Y")
													rpdf$P<-ifelse(preddf$Y>=lchurd,1,0)
												}
											}else{
												rpdf$P<-NA
											}	
											rpdf$lchurd<-lchurd
											return(rpdf)
										},spmdl=spmdl,spdf=spdf)
											
								adjresdf<-adjresdf[,c("SamplingEvent","Model","SpeciesCode","AudiofileId","lchurd","P")]
								
							}else{
								adjresdf<-data.frame()
							}
							return(adjresdf)
						},spmdllist=spmdllist,gvcovarmdl=gvcovarmdl)
				
				## Now we merge predictions to gvspmatches, only to the model mm though (so inner right joining...)
				gvspm<-merge(gvspmatches,gvpredadj[,c("SamplingEvent","Model","SpeciesCode","AudiofileId","lchurd","P")],
						by=c("SamplingEvent","Model","SpeciesCode","AudiofileId"),all.y=T)
				gvspm$AdjSpeciesCode<-gvspm$SpeciesCode
				
				gvspadjmatches<-ldply(1:nrow(gvspm),function(rr,gvspm){
							matchrow<-gvspm[rr,]
							if(!is.na(matchrow$P) & matchrow$P==0){   #this is stating that the prediction has been corrected to a negative detection
								if(matchrow$match=="FALSEPOS"){  #thus a false positive becomes a true negative
									matchrow$adjMatch<-"TRUENEG"; matchrow$AdjSpeciesCode<-NA
								}else if(matchrow$match=="TRUEPOS"){  #a true positive becomes a false negative
									matchrow$adjMatch<-"FALSENEG"
								}else{
									matchrow$adjMatch<-"TRUENEG"; matchrow$AdjSpeciesCode<-NA
								}
							}else{
								matchrow$adjMatch<-matchrow$match
							}
							return(matchrow)
						},gvspm=gvspm)
				#save(gvspadjmatches, file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrections_fullHour065080095_10222021_",hnam,"_",mnam,".RData"))
				predadjmdllst[[mm]]<-gvspadjmatches
				rm(list=c("gvcovarmdl","spmdllist","gvpredadj","gvspm","gvspadjmatches"));gc()
			}
			
			# Now rbind by model
			predadjmdldf<-as.data.frame(rbindlist(predadjmdllst))
			
			#Hopefully this is the same length as gvspmatches
			nrow(predadjmdldf);nrow(gvspmatches)
			return(predadjmdldf)
			
		},sppdf=sppdf,mdlnmlst=mdlnmlst,gvcovarlst=gvcovarlst,gvspmatches)

Sys.time()-tm
names(gvpreadadjlst)<-paste0("h",c(65,80,95,99))

# Need to make adjust<-P, but deal with the NA's from species not corrected, make them 0
# Need to add ModelName
		
gvpreadadjlst<-llply(names(gvpreadadjlst),function(hh,gvpreadadjlst){
					df<-gvpreadadjlst[[hh]]
					df$adjust<-ifelse(df$P==1,1,0)
					df$ModelName<-ifelse(df$Model=="Resnet50::sigmoid","cols_54cls_Resnet50",ifelse(df$Model=="Resnet101::sigmoid","cols_54cls_Resnet101","cols_54cls_mobnet_full_finetune"))
					return(df)
				},gvpreadadjlst=gvpreadadjlst)
names(gvpreadadjlst)<-paste0("h",c(65,80,95,99))

save(gvpreadadjlst,file=paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_predAdj65809599_lch5099_04022022.RData"))


## Ready to summarize to plot!  
## Need this function
summarizeToSampleAllSpecies<-function(matches){
	hurdsample<-ldply(sort(unique(matches$hurdle)),function(hh,matches){
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

## Looping through hurdle value filters, then by model
load(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_predAdj65809599_lch5099_01252022.RData"))
hfilts<-names(gvpreadadjlst)
summdf<-ldply(hfilts,function(hnm,gvpreadadjlst,summarizeToSampleAllSpecies){
			hfdf<-gvpreadadjlst[[hnm]]
			mdldf<-ldply(unique(hfdf$Model),function(mdl,hfdf,summarizeToSampleAllSpecies){
						matches<-subset(hfdf,Model==mdl)
						adjmatches<-matches[,which(names(matches)!="match")]
						names(adjmatches)<-gsub("adjMatch","match",names(adjmatches))
						lchurddf<-ldply(unique(adjmatches$lchurd),function(lch,matches,adjmatches,summarizeToSampleAllSpecies){
									lcmatches<-subset(matches,lchurd==lch);lcadjmatches<-subset(adjmatches,lchurd==lch)
									metricsraw<-summarizeToSampleAllSpecies(matches=lcmatches); metricsraw$treatment<-"Uncorrected"
									metricsadj<-summarizeToSampleAllSpecies(matches=lcadjmatches); metricsadj$treatment<-"Corrected"
									mtdf<-rbind(metricsraw,metricsadj)
									mtdf$lchurd<-paste0("lch",lch)
									return(mtdf)
								},matches=matches,adjmatches=adjmatches,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
						lchurddf$Model<-mdl
						return(lchurddf)
					},hfdf=hfdf,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
			mdldf$PredictionFilter<-hnm
			return(mdldf)
		},gvpreadadjlst=gvpreadadjlst,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)


## let's burn the miss rate so it's easy to see in the plot
plotdf<-subset(summdf,PredictionFilter=="h99")
p1<-ggplot(plotdf,aes(x=Threshold, y=Prec)) + 
		geom_line(aes(color=treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw() +
		facet_grid(lchurd~Model)
p2<-ggplot(plotdf,aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw() +
		facet_grid(lchurd~Model)
print(p1)
dev.new();print(p2)

## Conclusion: select predictions at the 99% penalizaion hurdle
## If the model is Resnet:101 do not apply corrections
## To the other two use lchurdle of 0.95

############## OLD VERSION
## Looping through hurdle value filters, then by model
load(paste0(pathToLocalGit,"GVanalyses/3models2outputs/data/logisticCorrModels_fullHour065_predAdj658095_10262021.RData"))
hfilts<-names(gvpreadadjlst)
summdf<-ldply(hfilts,function(hnm,gvpreadadjlst,summarizeToSampleAllSpecies){
			hfdf<-gvpreadadjlst[[hnm]]
			mdldf<-ldply(unique(hfdf$Model),function(mdl,hfdf,summarizeToSampleAllSpecies){
						matches<-subset(hfdf,Model==mdl)
						adjmatches<-matches[,which(names(matches)!="match")]
						names(adjmatches)<-gsub("adjMatch","match",names(adjmatches))
						metricsraw<-summarizeToSampleAllSpecies(matches=matches); metricsraw$treatment<-"Uncorrected"
						metricsadj<-summarizeToSampleAllSpecies(matches=adjmatches); metricsadj$treatment<-"Corrected"
						mtdf<-rbind(metricsraw,metricsadj)
						mtdf$Model<-mdl
						return(mtdf)
					},hfdf=hfdf,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)
			mdldf$PredictionFilter<-hnm
			return(mdldf)
		},gvpreadadjlst=gvpreadadjlst,summarizeToSampleAllSpecies=summarizeToSampleAllSpecies)




## let's burn the miss rate so it's easy to see in the plot
p1<-ggplot(summdf,aes(x=Threshold, y=Prec)) + 
		geom_line(aes(color=treatment),size=2) +
		labs(x="Threshold", y="Precision",color="Treatment") + theme_bw() +
		facet_grid(Model~PredictionFilter)
p2<-ggplot(summdf,aes(x=Sens, y=Prec)) + 
		geom_line(aes(color=treatment),size=2,) +
		labs(x="Recall", y="Precision",color="Treatment") + theme_bw() +
		facet_grid(Model~PredictionFilter)
print(p1)
dev.new();print(p2)

