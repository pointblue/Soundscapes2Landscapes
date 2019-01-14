# TODO: Add comment
# 
# Author: lsalas@pointblue.org
###############################################################################


## ATTENTION: If plotting, you may need this command...
#par(mar=c(1,1,1,1))
####

## Modeling definitions
## Any one or a list of any of the following:
## ATTENTION - we said to use only 15 or so?
#species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
#			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
#			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
#			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
#resolution<-c("250M","500M","1000M") #
#noise<-c("noised")
#gediyrs<-c("1yr","2yr","3yr")
#addGEDI<-c(TRUE,FALSE)
#percent.train<-0.8 	#the percent of data used to train the model
####

## Functions consider pre-compiling...?
stratifySample<-function(df,yvar,percTrain){
	qv<-unique(df[,yvar])
	resdf<-ldply(.data=qv, .fun=function(q,df,yvar,percTrain){
				tdf<-subset(df,df[,yvar]==q);nva<-nrow(tdf);
				tdf$inOut<-rbinom(nva,1,percTrain);
				return(tdf)			
			},df=df,yvar=yvar,percTrain=percTrain)
	return(resdf)
}

fitXGB<-function(trainset,testset,predgriddf){
	qq<-names(trainset)
	adn<-subset(qq,grepl("PresAbs",qq)==F & grepl("inOut",qq)==F)
	trainMatrix<-as.matrix(trainset[,adn])
	testMatrix<-as.matrix(testset[,adn])
	predMatrix<-as.matrix(predgriddf[,2:ncol(predgriddf)])
	sp.train<-list(data=trainMatrix,label=trainset$PresAbs)
	sp.test<-list(data=testMatrix,label=testset$PresAbs)
	dtrain <- xgb.DMatrix(sp.train$data, label = sp.train$label)
	dtest <- xgb.DMatrix(sp.test$data, label = sp.test$label)
	watchlist <- list(eval = dtest, train = dtrain)
	
	param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
			objective = "binary:logistic", eval_metric = "error", eval_metric = "auc")
	bst <- xgb.train(param, dtrain, nrounds = 100, watchlist, early_stopping_rounds=5, maximize=TRUE)
	
	#evaluate performance
	label = getinfo(dtest, "label")
	pred <- predict(bst, dtest)
	predgrid<-predict(bst,predMatrix)
	
	#varImportance
	importance_matrix <- xgb.importance(model = bst)
	imdf<-as.data.frame(importance_matrix)
	imdf<-imdf[order(imdf$Gain,decreasing=TRUE),]
	
	#RETURN: bst, preds,predgrid,imdf
	res=list(model=bst,predtest=pred,predgrid=predgrid,varimp=imdf)
}

getVariableImportance<-function(rfom,svmm,boom,xgbm,trainset){
	imptemp<-data.frame()
	if(!inherits(rfom,"try-error")){imprfo<-retrieveVarImp(mdl=rfom,trainset=trainset,type="RandomForests");imptemp<-rbind(imptemp,imprfo)}
	if(!inherits(svmm,"try-error")){impsvm<-retrieveVarImp(mdl=svmm,trainset=trainset,type="SVM");imptemp<-rbind(imptemp,impsvm)}
	if(!inherits(boom,"try-error")){impboo<-retrieveVarImp(mdl=boom,trainset=trainset,type="AdaBoost");imptemp<-rbind(imptemp,impboo)}
	if(!inherits(xgbm,"try-error")){
		impxgb<-xgbm$varimp[,c("Feature","Gain")];names(impxgb)<-c("Variable","AbsImportance")
		impxgb$Model<-"xgBoost";impxgb<-impxgb[1:10,]
		impxgb$RelImportance<-lapply(impxgb$AbsImportance,FUN=function(x,sumI){absi<-x/sumI;return(absi)},sumI=sum(impxgb$AbsImportance))
		imptemp<-rbind(imptemp,impxgb)
	}
	return(imptemp)
}

retrieveVarImp<-function(mdl,trainset,type){
	impres<-Importance(mdl, data=trainset)
	impdf<-data.frame(Variable=names(trainset),AbsImportance=impres$imp,Model=type)
	impdf<-impdf[order(impdf$AbsImportance,decreasing=TRUE),]
	impdf<-impdf[1:10,]
	impdf$RelImportance<-lapply(impdf$AbsImportance,FUN=function(x,sumI){absi<-x/sumI;return(absi)},sumI=sum(impdf$AbsImportance))
	return(impdf)
}

getVarMetaClass<-function(df){
	df$VarType<-ifelse(substr(df$Variable,1,3) %in% c("aet","cwd","pet","ppt","tmx","tmn"),"BCM",
				ifelse(substr(df$Variable,1,5) %in% c("Coast","Stree","Strea"),"Distance",
					ifelse(substr(df$Variable,1,4)=="N38W","DEM",
						ifelse(substr(df$Variable,1,4)=="ndvi","NDVI","GEDI"))))
	return(df)
}

checkSavePath<-function(svpth,rez){
	if(!dir.exists(svpth)){
		stop(paste("ERROR: The path to save files does not exist. Please create the folder", svpth, "before continuing with code execution."))
	}
	reserr<-""
	for(rr in rez){
		err<-""
		if(!dir.exists(paste0(svpth,rr))){
			err<-paste("The folder for resolution",rr,"in",svpth,"did not exist and was automatically created. \n")
			dir.create(paste0(svpth,rr))
			if(!dir.exists(paste0(svpth,rr))){
				err<-"ERROR: failed to create folder for at least one resolution level"
			}
		}
		reserr<-paste(reserr,err,sep="")
	}
	return(reserr)
}

getConfusionMatrix<-function(df,np){
	if(ncol(df)==5){
		qq<-adply(.data=df,.margins=1,.fun=function(rr,np){
					rf=ifelse(rr[2]>=np,1,0);
					sv=ifelse(rr[3]>=np,1,0);
					bo=ifelse(rr[4]>=np,1,0);
					gb=ifelse(rr[5]>=np,1,0);
					rdf<-data.frame(prfo=rf,psvm=sv,pboo=bo,xgbm=gb);
					return(rdf)},np=np)
		names(qq)<-c("observed","hprfo","hpsvm","hpboo","hxgbm")
		krfo<-cohen.kappa(qq[,c(1,2)]);ksvm<-cohen.kappa(qq[,c(1,3)]);
		kboo<-cohen.kappa(qq[,c(1,4)]);kxgb<-cohen.kappa(qq[,c(1,5)])
		df<-cbind(df,qq)
		dfp<-subset(df,observed>0);dfn<-subset(df,observed==0)
		mdf<-data.frame(Model=c("randF","SVM","Boost","XGBM"),
				truePos=c(sum(dfp$hprfo>0),sum(dfp$hpsvm>0),sum(dfp$hpboo>0),sum(dfp$hxgbm>0)),
				falsePos=c(sum(dfp$hprfo==0),sum(dfp$hpsvm==0),sum(dfp$hpboo==0),sum(dfp$hxgbm==0)),
				trueNeg=c(sum(dfn$hprfo==0),sum(dfn$hpsvm==0),sum(dfn$hpboo==0),sum(dfn$hxgbm==0)),
				falseNeg=c(sum(dfn$hprfo>0),sum(dfn$hpsvm>0),sum(dfn$hpboo>0),sum(dfn$hxgbm>0)),
				Kappa=c(krfo$kappa,ksvm$kappa,kboo$kappa,kxgb$kappa))
	}else{
		qq<-adply(.data=df,.margins=1,.fun=function(rr,np){
					rf=ifelse(rr[2]>=np,1,0);
					sv=ifelse(rr[3]>=np,1,0);
					bo=ifelse(rr[4]>=np,1,0);
					rdf<-data.frame(prfo=rf,psvm=sv,pboo=bo);
					return(rdf)},np=np)
		names(qq)<-c("observed","hprfo","hpsvm","hpboo")
		krfo<-cohen.kappa(qq[,c(1,2)]);ksvm<-cohen.kappa(qq[,c(1,3)]);
		kboo<-cohen.kappa(qq[,c(1,4)])
		df<-cbind(df,qq)
		dfp<-subset(df,observed>0);dfn<-subset(df,observed==0)
		mdf<-data.frame(Model=c("randF","SVM","Boost"),
				truePos=c(sum(dfp$hprfo>0),sum(dfp$hpsvm>0),sum(dfp$hpboo>0)),
				falsePos=c(sum(dfp$hprfo==0),sum(dfp$hpsvm==0),sum(dfp$hpboo==0)),
				trueNeg=c(sum(dfn$hprfo==0),sum(dfn$hpsvm==0),sum(dfn$hpboo==0)),
				falseNeg=c(sum(dfn$hprfo>0),sum(dfn$hpsvm>0),sum(dfn$hpboo>0)),
				Kappa=c(krfo$kappa,ksvm$kappa,kboo$kappa))
	}
	return(mdf)
}

getPredicted<-function(preds,predgriddf,test,testset,rfom,svmm,boom,xgbm){
	if(!inherits(rfom,"try-error")){
		prfom<-as.data.frame(predict(rfom,predgriddf))
		preds$vrfom<-as.numeric(prfom[,2])
		trfom<-as.data.frame(predict(rfom,testset))
		test$prfo<-as.numeric(trfom[,2])
	}
	if(!inherits(svmm,"try-error")){
		psvmm<-as.data.frame(predict(svmm,predgriddf))
		preds$vsvmm<-as.numeric(psvmm[,2])
		tsvmm<-as.data.frame(predict(svmm,testset))
		test$psvm<-as.numeric(tsvmm[,2])
	}
	if(!inherits(boom,"try-error")){
		pboom<-as.data.frame(predict(boom,predgriddf))
		preds$vboom<-as.numeric(pboom[,2])
		tboom<-as.data.frame(predict(boom,testset))
		test$pboo<-as.numeric(tboom[,2])
	}
	if(!inherits(xgbm,"try-error")){
		preds$vxgbm<-xgbm$predgrid
		test$xgbm<-xgbm$predtest
	}
	return(list(preds=preds,test=test))
	
}

fitCaseModel<-function(X,logf,ncores=NULL,percent.train=0.8,noise="noised"){
	logf<-zz;ncores=NULL;percent.train=0.8;noise="noised"
	
	pathToGit<-X[["gitpath"]];svpth<-X[["svpath"]];resolution<-X[["rez"]]
	spcd<-X[["spp"]];gediyr<-X[["yrsp"]];addGEDI<-X[["gedi"]]
	
	species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
	
	startdttm<-format(Sys.time(), "%Y%m%d_%H%M")		#starting datetime
	cat("START OF SDM MODEL FITTING RUN", file = logf, sep = "\n\n", append=TRUE)
	
	## Dependencies
	#libs<-c("rminer","raster","dismo","plyr","data.table","xgboost","doParallel","caret","kernlab","psych","compiler")
	#sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)
	#pathToGit<-"C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/"
	#svpth<-"c:/S2Ltemp/"
	####
	
	## Declare the cores to use for parallelization:
	#if(is.null(ncores)){ncores<-detectCores()}  
	#cl<-makeCluster(ncores)
	#registerDoParallel(cl)
	####
	
	## Check that we have the path to save the files
	chkpth<-checkSavePath(svpth=svpth,rez=resolution)  # Check that the folders exist in svpth and for each resolution level
	if(chkpth!=""){
		cat(chkpth)
		cat(chkpth, file=logf, sep = "\n", append=TRUE)	#log this
	}else{
		cat("Checking the results path for needed folders... OK", file = logf, sep = "\n", append=TRUE)
	}
	
	####
	
	#need to store in a single data frame:
	# species, resolution, model, top10 vars, and their value
	#topvars<-data.frame()
	
	#get the base grid for this resolution
	basegrid<-raster(paste0(pathToGit,"sdmTool/data/Coast_Distance/",resolution,"/CoastDistance_",tolower(resolution),"_Clip.tif"))
	basegrid[]<-NA
	cat(paste("Loaded base grid for resolution:",resolution), file = logf, sep = "\n", append=TRUE)
	
	# Load the deflated bird file and filter for the loop species
	dtpth<-paste0(pathToGit,"sdmTool/data/Birds/",resolution)
	load(file=paste0(dtpth,"/deflated_",resolution,".RData"))	
	cat("Loaded and preparing the corresponding bird data...", file = logf, sep = "\n", append=TRUE)
	
	#for(spcd in species){
		
		#select only the desired species from the data
		omitspecies<-subset(species,species!=spcd)
		omitnumdet<-paste0("NumDet",omitspecies)
		
		#get covars and the current species' data
		spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet))]
		spdata<-as.data.frame(na.omit(spdata))
		#create the prediction grid
		predgriddf<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet) & !names(deflatedcovardf) %in% c("x","y") & 
								!names(deflatedcovardf) %in% c(spcd,paste0("NumDet",spcd)))]
		predgriddf<-na.omit(predgriddf)
		#will need this too...
		xydf<-deflatedcovardf[,c("x","y",paste0("gId",resolution))];names(xydf)<-c("x","y","cellId")
		
		# Get the species data fr the right gediyr, and to include/exclude gedi
		spdata<-spdata[,which(!names(spdata) %in% c("x","y",paste0("gId",resolution)))]
		namspdat<-names(spdata)
		exclgedi<-subset(namspdat,grepl("_3yr_",namspdat) | grepl("_2yr_",namspdat) | grepl("_1yr_",namspdat))
		if(addGEDI==FALSE){
			spdata<-spdata[,which(!names(spdata) %in% exclgedi)]
			addgn<-"_noGEDI"
		}else{
			exclgedi<-subset(exclgedi,!grepl(gediyr,exclgedi))
			if(NROW(exclgedi)>0){spdata<-spdata[,which(!names(spdata) %in% exclgedi)]}
			addgn<-"_withGEDI"
		}
		cat("Dataset ready. Attempt model fitting... ", file = logf, sep = "\n", append=TRUE)
		
		# Then fit the stack, predict, and save the wighted average of all models   Need to vectorize this LEO!
		if(sum(spdata[,spcd]==1)>0.05*nrow(spdata)){
			cat("Dataset has > 5% of cells with presence", file = logf, sep = "\n", append=TRUE)
			
			names(spdata)<-gsub(spcd,"PresAbs",names(spdata))
			spdata$PresAbs_f<-as.factor(as.character(spdata$PresAbs))
			
			## make train and test sets  STRATIFY!
			spdata<-stratifySample(df=spdata,yvar="PresAbs_f",percTrain=percent.train)
			trainset<-subset(spdata,inOut==1);testset<-subset(spdata,inOut==0)
			
			trainsize<-round(percent.train*nrow(spdata))	#setting train size to 80%
			naivePrev<-sum(spdata$PresAbs)/nrow(spdata)
			
			## fitting  models
			nc<-ncol(trainset)-4
			fmlf<-paste("PresAbs_f~",paste(names(trainset[1:nc]),collapse="+"),sep=" ")
			fmln<-paste("PresAbs~",paste(names(trainset[3:nc]),collapse="+"),sep="")
			rfom<-try(fit(as.formula(fmlf), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE),silent=TRUE)
			svmm<-try(fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=2),silent=TRUE)
			boom<-try(fit(as.formula(fmlf), data=trainset, model="boosting",na.action=na.omit),silent=TRUE)
			xgbm<-try(fitXGB(trainset,testset,predgriddf),silent=TRUE)
			
			if(inherits(rfom,"try-error") && inherits(rfom,"try-error") && inherits(rfom,"try-error") && inherits(rfom,"try-error")){
				cat("None of the models attempted was able to converge and fit", file = logf, sep = "\n", append=TRUE)
			}else{
				cat("Some or all models were fitted. Evaluating fit and predicting...", file = logf, sep = "\n", append=TRUE)
				
				## predicting to test and to grid 
				preds<-data.frame(cellId=as.integer(predgriddf[,paste0("gId",resolution)]))
				predgriddf<-predgriddf[,names(predgriddf)[which(names(predgriddf) %in% names(trainset))]]
				test<-data.frame(observed=testset[,"PresAbs"])
				
				## predict to test set and eval the rmse
				predres<-getPredicted(preds=preds,predgriddf=predgriddf,test=test,testset=testset,rfom=rfom,svmm=svmm,boom=boom,xgbm=xgbm)
				preds<-predres$preds
				test<-predres$test
				
				## individual model support is then:
				rmse<-apply(test[,2:ncol(test)],2,FUN=function(x,obs)sqrt(sum((x-obs)^2)/NROW(x)),obs=test$observed)
				mv<-ceiling(max(rmse));supp<-mv-rmse
				#get the confusion matrix params too
				gofMetrics<-getConfusionMatrix(test,naivePrev)
				gofMetrics$RMSE<-rmse;gofMetrics$support<-supp
				
				cat("Creating rasters...", file = logf, sep = "\n", append=TRUE)
				## convert predicted values to logits...
				#preds<-adply(.data=preds[,2:5],.margins=1,.fun=function(x)log(x)-log(1-x))	#Too slow!
				preds<-data.table(preds)
				if(!inherits(rfom,"try-error")){preds[,lgvrfom:=log(vrfom)-log(1-vrfom),]}
				if(!inherits(svmm,"try-error")){preds[,lgvsvmm:=log(vsvmm)-log(1-vsvmm),]}
				if(!inherits(boom,"try-error")){preds[,lgvboom:=log(vboom)-log(1-vboom),]}
				if(!inherits(xgbm,"try-error")){preds[,lgvxgbm:=log(vxgbm)-log(1-vxgbm),]}
				
				## and weighted average is...3=3; 5=4:5; 7=5:7 9=6:9
				ssup<-sum(supp)
				ncp<-ncol(preds)
				if(ncp==3){ncprg<-3}else if(ncp==5){ncprg<-4:5}else if(ncp==7){ncprg<-5:7}else{ncprg<-6:9}
				preds[,lgweighted:=apply(X=preds,MARGIN=1,FUN=function(x,supp,ssup,ncprg)as.numeric(x[ncprg])%*%supp/ssup,supp=supp,ssup=ssup,ncprg=ncprg),]
				
				## convert weighted back to probabilities...
				preds[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
				
				## convert to raster and plot...
				rastres<-basegrid
				preds<-merge(preds,xydf,by="cellId",all.x=T)
				preds$cid<-cellFromXY(basegrid,preds[,c("x","y")])
				cid<-preds$cid;vals<-as.numeric(preds$weighted)
				rastres[cid]<-vals
				writeRaster(rastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_probPresence.tif",sep=""),format="GTiff",overwrite=T)
				
				## let's hurdle it by the naive prevalence...
				preds[,presence:=ifelse(weighted<=naivePrev,0,1),]
				trastres<-basegrid
				vals<-as.numeric(preds$presence)
				trastres[cid]<-vals
				## write as geotiff
				writeRaster(trastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_hurdle.tif",sep=""),format="GTiff",overwrite=T)
				
				cat("Calculating variable importance...", file = logf, sep = "\n", append=TRUE)
				#compile variable importance-top 10
				importance<-getVariableImportance(rfom,svmm,boom,xgbm,trainset)
				importance<-getVarMetaClass(df=importance)
				importance$Species<-spcd;importance$Resolution<-resolution
				
				cat("Saving results, wrapping up ...", file = logf, sep = "\n", append=TRUE)
				save(trainset,testset,test,gofMetrics,rfom,svmm,boom,xgbm,importance, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_modelResults.RData"))
				#topvars<-rbind(topvars,imptemp)
				
			}
			
			cat(paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn), file = logf, sep = "\n", append=TRUE)
			res<-paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn)
			
		}else{
			cat(paste0("Skipping ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn," because of <5% of sites have presence."), file = logf, sep = "\n", append=TRUE)
			res<-paste0("Skipping ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn," because of <5% of sites have presence.")
		}
		
	#}
	#save(topvars,file=paste0(svpth,resolution,"/topVariables_",resolution,".RData"))
	
}

####










