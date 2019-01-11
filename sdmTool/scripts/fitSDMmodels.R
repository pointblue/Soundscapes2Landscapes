# TODO: Add comment
# 
# Author: lsalas@pointblue.org
###############################################################################

## Remove objects -- to make sure no prior session variables are in memory
rm(list=ls(all=TRUE));gc()
####

startdttm<-format(Sys.time(), "%Y%m%d_%H%M")		#starting datetime
write("START OF SDM MODEL FITTING RUN \n\n", file=paste0(svpth,"SDMfit_",startdttm,".log"), append=FALSE)	#open new log file

## Dependencies
libs<-c("rminer","raster","dismo","plyr","data.table","xgboost","doParallel","caret","kernlab");sapply(libs, require, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)
pathToGit<-"C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/"
svpth<-"c:/S2Ltemp/"
####

## Declare the cores to use for parallelization:
ncores<-detectCores()  #ATTENTION: replace this with the number of cores you want to use. Otherwise the algo will use all.
cl<-makeCluster(ncores)
registerDoParallel(cl)

## ATTENTION: If plotting, you may need this command...
#par(mar=c(1,1,1,1))
####

## Modeling definitions
## Any one or a list of any of the following:
## ATTENTION - we said to use only 15 or so?
species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
resolution<-c("250M","500M","1000M") #
noise<-c("noised")
gediyrs<-c("1yr","2yr","3yr")
addGEDI<-c(TRUE,FALSE)
percent.train<-0.8 	#the percent of data used to train the model
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

fitCaseModels<-function(X){
	sp<-X[1];rz<-X[2];nz<-X[3];yr<-X[4];gd<-X[5]
	print(paste(sp,rz,nz,yr,gd))
}

####

## Check that we have the path to save the files
chkpth<-checkSavePath(svpth=svpth,rez=resolution)  # Check that the folders exist in svpth and for each resolution level
if(chkpth!=""){
	cat(chkpth)
	write(paste(chkpth,"\n\n"), file=paste0(svpth,"SDMfit_",startdttm,".log"), append=TRUE)	#log this
}



results<-adply(.data=cases,.margins=1,.fun=fitCaseModels(X,...))

for(zz in resolution){

	#need to store in a single data frame:
	# species, resolution, model, top10 vars, and their value
	topvars<-data.frame()
	
	#get the base grid for this resolution
	basegrid<-raster(paste0(pathToGit,"Coast_Distance/",zz,"/CoastDIstance_",zz,"_Clip.tif"))
	basegrid[]<-NA
	
	# Load the deflated bird file and filter for the loop species
	dtpth<-paste0(pathToGit,"birds/",zz)
	load(file=paste0(dtpth,"/deflated_",zz,".RData"))	
	
	for(spcd in species){

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
		xydf<-deflatedcovardf[,c("x","y",paste0("gId",zz))];names(xydf)<-c("x","y","cellId")

		# Then fit the stack, predict, and save the wighted average of all models
		spdata<-spdata[,which(!names(spdata) %in% c("x","y",paste0("gId",zz)))]
		
		#Need to vectorize this LEO!
		if(sum(spdata[,spcd]==1)>0.05*nrow(spdata)){
			names(spdata)<-gsub(spcd,"PresAbs",names(spdata))
			spdata$PresAbs_f<-as.factor(as.character(spdata$PresAbs))
			
			## make train and test sets  STRATIFY!
			spdata<-stratifySample(df=spdata,yvar="PresAbs_f",percTrain=percent.train)
			trainset<-subset(spdata,inOut==1);testset<-subset(spdata,inOut==0)
			
			trainsize<-round(percent.train*nrow(spdata))	#setting train size to 80%
			naivePrev<-sum(spdata$PresAbs)/nrow(spdata)
			
			## fitting  models
			nc<-ncol(trainset)-4
			fmlf<-paste("PresAbs_f~",paste(names(trainset[1:nc]),collapse="+"),sep="")
			fmln<-paste("PresAbs~",paste(names(trainset[3:nc]),collapse="+"),sep="")
			svmm<-fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=2)
			rfom<-fit(as.formula(fmlf), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE)
			boom<-fit(as.formula(fmlf), data=trainset, model="boosting",na.action=na.omit)
			xgbm<-try(fitXGB(trainset,testset,predgriddf),silent=TRUE)
			
			## predicting to stack
			preds<-data.frame(cellId=as.integer(predgriddf[,paste0("gId",zz)]))
			prfom<-as.data.frame(predict(rfom,predgriddf))
			preds$vrfom<-as.numeric(prfom[,2])
			psvmm<-as.data.frame(predict(svmm,predgriddf))
			preds$vsvmm<-as.numeric(psvmm[,2])
			pboom<-as.data.frame(predict(boom,predgriddf))
			preds$vboom<-as.numeric(pboom[,2])
			if(!inherits(xgbm,"try-error")){preds$vxgbm<-xgbm$predgrid}
			
			## predict to test set and eval the rmse
			test<-data.frame(observed=testset[,"PresAbs"])
			trfom<-as.data.frame(predict(rfom,testset))
			test$prfo<-as.numeric(trfom[,2])
			tsvmm<-as.data.frame(predict(svmm,testset))
			test$psvm<-as.numeric(tsvmm[,2])
			tboom<-as.data.frame(predict(boom,testset))
			test$pboo<-as.numeric(tboom[,2])
			if(!inherits(xgbm,"try-error")){
				test$xgbm<-xgbm$predtest
			}
			
			## individual model support is then:
			supp<-apply(test[,2:ncol(test)],2,FUN=function(x,obs)sqrt(sum((x-obs)^2)/NROW(x)),obs=test$observed)
			mv<-ceiling(max(supp));supp<-mv-supp
			
			save(trainset,testset,test,supp,rfom,svmm,boom,xgbm, file=paste0(svpth,zz,"/",spcd,"_",zz,"_modelResults.RData"))
			
			## convert predicted values to logits...
			#preds<-adply(.data=preds[,2:5],.margins=1,.fun=function(x)log(x)-log(1-x))	#Too slow!
			preds<-data.table(preds)
			preds[,lgvrfom:=log(vrfom)-log(1-vrfom),]
			preds[,lgvsvmm:=log(vsvmm)-log(1-vsvmm),]
			preds[,lgvboom:=log(vboom)-log(1-vboom),]
			preds[,lgvxgbm:=log(vxgbm)-log(1-vxgbm),]
			
			## and weighted average is...
			ssup<-sum(supp)
			preds[,lgweighted:=apply(X=preds,MARGIN=1,FUN=function(x,supp,ssup)as.numeric(x[6:9])%*%supp/ssup,supp=supp,ssup=ssup),]
			## convert it back to probabilities...
			preds[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
			
			## convert to raster and plot...
			rastres<-basegrid
			preds<-merge(preds,xydf,by="cellId",all.x=T)
			preds$cid<-cellFromXY(basegrid,preds[,c("x","y")])
			cid<-preds$cid;vals<-as.numeric(preds$weighted)
			rastres[cid]<-vals
			plot(rastres)
			writeRaster(rastres,filename=paste0(svpth,zz,"/",spcd,"_",zz,"_probPresence.tif",sep=""),format="GTiff",overwrite=T)
			
			## let's hurdle it by the naive prevalence...
			preds[,presence:=ifelse(weighted<=naivePrev,0,1),]
			trastres<-basegrid
			vals<-as.numeric(preds$presence)
			trastres[cid]<-vals
			plot(trastres)
			## write as geotiff
			writeRaster(trastres,filename=paste0(svpth,zz,"/",spcd,"_",zz,"_hurdle.tif",sep=""),format="GTiff",overwrite=T)
			
			#compile variable importance-top 10
			imptemp<-data.frame()
			impsvm<-retrieveVarImp(mdl=svmm,trainset=trainset,type="SVM");imptemp<-rbind(imptemp,impsvm)
			imprfo<-retrieveVarImp(mdl=rfom,trainset=trainset,type="RandomForests");imptemp<-rbind(imptemp,imprfo)
			impboo<-retrieveVarImp(mdl=boom,trainset=trainset,type="AdaBoost");imptemp<-rbind(imptemp,impboo)
			impxgb<-xgbm$varimp[,c("Feature","Gain")];names(impxgb)<-c("Variable","AbsImportance")
			impxgb$Model<-"xgBoost";impxgb<-impxgb[1:10,]
			impxgb$RelImportance<-lapply(impxgb$AbsImportance,FUN=function(x,sumI){absi<-x/sumI;return(absi)},sumI=sum(impxgb$AbsImportance))
			imptemp<-rbind(imptemp,impxgb)
			imptemp<-getVarMetaClass(df=imptemp)
			imptemp$Species<-spcd;imptemp$Resolution<-zz
			
			topvars<-rbind(topvars,imptemp)
			
			print(paste("Done with",spcd,"at resolution",zz))
			
		}else{
			print(paste("Skipping",spcd,"at resolution",zz,"because of <5% of sites have presence."))
		}
		
	}
	save(topvars,file=paste0(svpth,zz,"/topVariables_",zz,".RData"))
}











