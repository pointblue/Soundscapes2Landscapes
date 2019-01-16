# TODO: Add comment
# 
# Author: lsalas@pointblue.org
###############################################################################


## Functions consider pre-compiling...?

fitXGB<-function(trainset,testset,predgriddf,alldata){	
	qq<-names(trainset)
	adn<-subset(qq,grepl("PresAbs",qq)==F & grepl("inOut",qq)==F)
	trainMatrix<-as.matrix(trainset[,adn])
	testMatrix<-as.matrix(testset[,adn])
	predMatrix<-as.matrix(predgriddf[,adn])
	allMatrix<-as.matrix(alldata[,adn])
	sp.train<-list(data=trainMatrix,label=trainset$PresAbs)
	sp.test<-list(data=testMatrix,label=testset$PresAbs)
	dtrain <- xgb.DMatrix(sp.train$data, label = sp.train$label)
	dtest <- xgb.DMatrix(sp.test$data, label = sp.test$label)
	watchlist <- list(eval = dtest, train = dtrain)
	
	#param <- list(max_depth = 2, eta=1, silent = 1,  nthread = 2,
	#		objective = "binary:logistic", eval_metric = "error", eval_metric = "auc")
	#bst <- xgb.train(param, dtrain, nrounds = 100, watchlist, early_stopping_rounds=10, maximize=TRUE)
	
	param.grid<-expand.grid(max_depth = c(2,3,5),gamma = c(1, 2, 3), colsample_bytree = c(0.4, 0.7, 1.0), 
			min_child_weight = c(0.5, 1, 1.5), eta=1, silent = 0,  nthread = 2,			
			objective = "binary:logistic", eval_metric = "error", eval_metric = "auc")
	
	xgboptim<-data.frame()
	for(kk in 1:nrow(param.grid)){
		param<-as.list(param.grid[kk,])
		xbst <- xgb.train(param, dtrain, nrounds = 100, watchlist, early_stopping_rounds=10, maximize=TRUE,verbose=0)
		merr<-min(xbst$evaluation_log$eval_error);mauc<-max(xbst$evaluation_log$eval_auc)
		tdf<-data.frame(row=kk,minerror=merr,maxauc=mauc)
		xgboptim<-rbind(xgboptim,tdf)
	}
	#use the model with max AUC
	xgboptim<-xgboptim[order(xgboptim$maxauc,decreasing=TRUE),]
	toprow<-as.numeric(xgboptim[1,"row"])
	param<-as.list(param.grid[toprow,])
	xbst <- xgb.train(param, dtrain, nrounds = 100, watchlist, early_stopping_rounds=10, maximize=TRUE)
	
	#evaluate performance
	label = getinfo(dtest, "label")
	pred <- predict(xbst, dtest)
	predgrid<-predict(xbst,predMatrix)
	predall<-predict(xbst,allMatrix)
	
	#varImportance
	importance_matrix <- xgb.importance(model = xbst)
	imdf<-as.data.frame(importance_matrix)
	imdf<-imdf[order(imdf$Gain,decreasing=TRUE),]
	
	#RETURN: bst, preds,predgrid,imdf
	res=list(model=xbst,predtest=pred,predgrid=predgrid,predall=predall,varimp=imdf)
}

getVariableImportance<-function(rfom,svmm,boom,xgbm,trainset){
	imptemp<-data.frame()
	if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){imprfo<-retrieveVarImp(mdl=rfom,trainset=trainset,type="RandomForests");imptemp<-rbind(imptemp,imprfo)}
	if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){impsvm<-retrieveVarImp(mdl=svmm,trainset=trainset,type="SVM");imptemp<-rbind(imptemp,impsvm)}
	if(!inherits(boom,"try-error") && class(boom@object)!="character"){impboo<-retrieveVarImp(mdl=boom,trainset=trainset,type="AdaBoost");imptemp<-rbind(imptemp,impboo)}
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
	ncd<-ncol(df)
	if(ncd>1){
		qq<-as.data.frame(apply(df[,2:ncd],MARGIN=2,FUN=function(rr,np){z<-ifelse(rr>=np,1,0);return(z)},np=np))
		names(qq)<-paste0("h",names(qq))
	}
	df<-cbind(df,qq)
	dfp<-subset(df,observed>0);dfn<-subset(df,observed==0)
	mdf<-data.frame();naqq<-names(qq)
	for(cc in 1:ncol(qq)){
		vanm<-naqq[cc]
		ccnam<-ifelse(vanm=="hprfo","randF",ifelse(naqq[cc]=="hpsvm","SVM",ifelse(naqq[cc]=="hpboo","Boost","XGBM")))
		truePos=sum(dfp[,vanm]>0);falsePos=sum(dfp[,vanm]==0);trueNeg=sum(dfn[,vanm]==0);falseNeg=sum(dfn[,vanm]>0)
		kappaval<-cohen.kappa(cbind(df[,1],qq[,cc]))
		tdf<-data.frame(Model=ccnam,truePos=truePos,falsePos=falsePos,trueNeg=trueNeg,falseNeg=falseNeg,Kappa=kappaval$kappa)
		mdf<-rbind(mdf,tdf)
	}
	return(mdf)
}

getGOF<-function(testres,thresh){
	ncd<-ncol(testres)
	if(ncd>1){
		qq<-as.data.frame(t(laply(2:ncd,.fun=function(rr,tedf,thv){
							np<-thv[rr-1];
							z<-ifelse(tedf[,rr]>=np,1,0);
							return(z)},tedf=testres,thv=thresh)))
		names(qq)<-paste0("h",names(testres)[2:ncd])
	}
	df<-cbind(testres,qq)
	dfp<-subset(df,observed>0);dfn<-subset(df,observed==0)
	mdf<-data.frame();naqq<-names(qq)
	for(cc in 1:ncol(qq)){
		vanm<-naqq[cc]
		ccnam<-ifelse(vanm=="hprfo","randF",ifelse(vanm=="hpsvm","SVM",ifelse(vanm=="hpboo","Boost","XGBM")))
		truePos=sum(dfp[,vanm]>0);falsePos=sum(dfp[,vanm]==0);trueNeg=sum(dfn[,vanm]==0);falseNeg=sum(dfn[,vanm]>0)
		kappaval<-cohen.kappa(cbind(testres[,1],qq[,cc]))
		rmseval<-mmetric(x=testres[,1],y=testres[,(cc+1)],metric="RMSE",D=thresh[cc])
		nt<-nrow(testres);phiv<-cor(testres[,1],testres[,(cc+1)])
		aucval<-try(AUC(BR=(truePos+falseNeg)/nt,SR=(truePos+falsePos)/nt,Phi=phiv),silent=TRUE)
		if(inherits(aucval,"try-error")){
			aucval<-list(AUC=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA)
		}
		tdf<-data.frame(Model=ccnam,Threshold=thresh[cc],truePos=truePos,falsePos=falsePos,trueNeg=trueNeg,falseNeg=falseNeg,Kappa=kappaval$kappa,rmse=rmseval,
				AUC=aucval$AUC,Accuracy=aucval$Accuracy,Sensitivity=aucval$Sensitivity,Specificity=aucval$Specificity)
		mdf<-rbind(mdf,tdf)
	}
	return(mdf)
}

getPredicted<-function(preds,predgriddf,testset,alldata,rfom,svmm,boom,xgbm){
	test<-data.frame(observed=testset[,"PresAbs"])
	alld<-data.frame(observed=alldata[,"PresAbs"])
	
	if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){
		prfom<-as.data.frame(predict(rfom,predgriddf))
		preds$vrfom<-as.numeric(prfom[,2])
		trfom<-as.data.frame(predict(rfom,testset))
		test$prfo<-as.numeric(trfom[,2])
		arfom<-as.data.frame(predict(rfom,alldata))
		alld$prfo<-arfom[,2]
	}
	if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){
		psvmm<-as.data.frame(predict(svmm,predgriddf))
		preds$vsvmm<-as.numeric(psvmm[,2])
		tsvmm<-as.data.frame(predict(svmm,testset))
		test$psvm<-as.numeric(tsvmm[,2])
		asvmm<-as.data.frame(predict(svmm,alldata))
		alld$psvm<-asvmm[,2]
	}
	if(!inherits(boom,"try-error") && class(boom@object)!="character"){
		pboom<-as.data.frame(predict(boom,predgriddf))
		preds$vboom<-as.numeric(pboom[,2])
		tboom<-as.data.frame(predict(boom,testset))
		test$pboo<-as.numeric(tboom[,2])
		aboom<-as.data.frame(predict(boom,alldata))
		alld$pboo<-aboom[,2]
	}
	#if(!inherits(xgbm,"try-error") && class(xgbm@object)!="character"){
	#	newdat<-predgriddf;newdat$PresAbs<-0 #rmner uses the model.matrix and thus requires the column - the value we use is irrelevant; can be 9 for example
	#	pxgbm<-as.data.frame(predict(xgbm,newdat))
	#	preds$vxgbm<-as.numeric(pxgbm[,1])
	#	txgbm<-as.data.frame(predict(xgbm,testset))
	#	test$xgbm<-as.numeric(trfom[,1])
	#}
	if(!inherits(xgbm,"try-error")){
		preds$vxgbm<-xgbm$predgrid
		test$xgbm<-xgbm$predtest
		alld$xgbm<-xgbm$predall
	}
	return(list(preds=preds,test=test,alldata=alld))
	
}

findThreshold<-function(df){
	dfp<-subset(df,observed==1);dfn<-subset(df,observed==0)
	#TThe goal is to find the best threshold for each,
	#Basically the value where SS=SP, or t such that sum(dfp$pred>=t)/nrow(dfp)==sum(dfn$pred<t)/nrow(dfn)
	#Per: Global Ecology and Biogeography, (Global Ecol. Biogeogr.) (2015) 24, 276–292
	#And: Journal of Biogeography, (2013) 40, 778–789
	#eval who is higher
	vnm<-names(df)[2:ncol(df)]
	thresh<-numeric()
	for(vv in vnm){
		hval<-ifelse(round(sum(dfp[,vv]>=0.01)/nrow(dfp),2)>round(sum(dfn[,vv]<0.01)/nrow(dfn),2),1,
				ifelse(round(sum(dfp[,vv]>=0.01)/nrow(dfp),2)<round(sum(dfn[,vv]<0.01)/nrow(dfn),2),0,2))
		if(hval==2){
			thresh<-c(thresh,0.01)
		}else{
			for(t in seq(0.01,0.99,by=0.01)){
				SS<-round(sum(dfp[,vv]>=t)/nrow(dfp),2)
				SP<-round(sum(dfn[,vv]<t)/nrow(dfn),2)
				nhval<-ifelse(SS>SP,1,ifelse(SS<SP,0,2))
				if(nhval!=hval){
					thv<-ifelse(nhval==2,t,t-0.01)
					thresh<-c(thresh,thv)
					break
				}
			}
		}
		
	}
	names(thresh)<-vnm
	return(thresh)
}

fitCaseModel<-function(X,logf,ncores=NULL,percent.train=0.8,noise="noised"){
	res<-"Attempting fit.."
	
	pathToGit<-X[["gitpath"]];svpth<-X[["svpath"]];resolution<-X[["rez"]]
	spcd<-X[["spp"]];gediyr<-X[["yrsp"]];addGEDI<-X[["gedi"]]
	
	species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
	
	startdttm<-format(Sys.time(), "%Y%m%d_%H%M")		#starting datetime
	cat("START OF SDM MODEL FITTING RUN", file = logf, sep = "\n\n", append=TRUE)
		
	## Check that we have the path to save the files
	chkpth<-checkSavePath(svpth=svpth,rez=resolution)  # Check that the folders exist in svpth and for each resolution level
	if(chkpth!=""){
		cat(chkpth)
		cat(chkpth, file=logf, sep = "\n", append=TRUE)	#log this
	}else{
		cat("Checking the results path for needed folders... OK", file = logf, sep = "\n", append=TRUE)
	}
	
	####
	
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
		H<-holdout(spdata$PresAbs_f,ratio=percent.train)
		trainset<-spdata[H$tr,];testset<-spdata[H$ts,]
		
		trainsize<-nrow(trainset)	#setting train size to 80%
		naivePrev<-sum(spdata$PresAbs)/nrow(spdata)
		
		#remove from memory if they exist
		if(TRUE %in% grepl("rfom",ls())){	
			rm(list=c("rfom","svmm","boom","xgbm"));gc()
		}
		
		## optimization grids
		#Use ?mmetric to see the list of optimization criteria. AUC is default, 
		#but not a good choice when we have negcases >>  poscases
		#Look in ?miner to understand "smethod". With KAPPA we can use kfold, and 3 folds 
		#is good given the size of our datasets. Setting it to train error
		rfsearch=list(smethod="grid",search=list(mtry=c(2,4,6),ntree=c(100,200,500,1000)),
				convex=0,metric="KAPPA",method=c("kfold",5,12345)) #mtry is the number of params to vary in each perm; convex=0 means look for global min
		boostsearch=list(smethod="grid",search=list(mfinal=c(50,100,200),minsplit=c(1,2,3)))
		
		## fitting  models  
		nc<-ncol(trainset)-4
		fmlf<-paste("PresAbs_f~",paste(names(trainset[1:nc]),collapse="+"),sep=" ")
		fmln<-paste("PresAbs~",paste(names(trainset[1:nc]),collapse="+"),sep="")
		rfom<-try(fit(as.formula(fmlf), data=trainset, model="randomForest",eval_metric="auc", importance=TRUE,search=rfsearch),silent=TRUE)
		svmm<-try(fit(as.formula(fmlf), data=trainset, model="ksvm", eval_metric="auc", search="heuristic10"),silent=TRUE)
		#svmm<-try(fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=NA,epsilon=NA),silent=TRUE)
		boom<-try(fit(as.formula(fmlf), data=trainset, model="boosting",cp=0.01,eval_metric="auc",search=boostsearch),silent=TRUE)
		#xgbm<-try(fit(as.formula(fmln), data=trainset, model="xgboost",verbose=0,max.depth=3,eta=1,nthread=2,objective="binary:logistic",nrounds=100),silent=TRUE)	#eval_metric="auc",
		xgbm<-try(fitXGB(trainset=trainset,testset=testset,predgriddf=predgriddf,alldata=spdata),silent=TRUE)
		
		
		if((inherits(rfom,"try-error") || class(rfom@object)=="character") && (inherits(svmm,"try-error") || class(svmm@object)=="character") && 
				(inherits(boom,"try-error") || class(boom@object)=="character") && (inherits(xgbm,"try-error") || class(xgbm@object)=="character")){
			cat("None of the models attempted was able to converge and fit", file = logf, sep = "\n", append=TRUE)
		}else{
			cat("Some or all models were fitted. Evaluating fit and predicting...", file = logf, sep = "\n", append=TRUE)
			
			## predicting to test and to grid 
			preds<-data.frame(cellId=as.integer(predgriddf[,paste0("gId",resolution)]))
			predgriddf<-predgriddf[,names(predgriddf)[which(names(predgriddf) %in% names(trainset))]]
						
			## predict to test set and eval the rmse
			print("Predicting")
			predres<-getPredicted(preds=preds,predgriddf=predgriddf,testset=testset,alldata=spdata,rfom=rfom,svmm=svmm,boom=boom,xgbm=xgbm)
			preds<-predres$preds
			testres<-predres$test
			alldata<-predres$alldata
			
			#get threshold based on SS=SP
			thresh<-findThreshold(df=alldata)
			
			## individual model support is then:  REVIEW for xgbm
			print("GOF")
			gofs<-getGOF(testres=testres,thresh=thresh)
			#CAUTION!! Using RMSE for support
			srmse<-sum(gofs$rmse,na.rm=TRUE);supp<-gofs$rmse/srmse
			gofs$Support<-supp
			
			print("Rasterizing")
			cat("Creating rasters...", file = logf, sep = "\n", append=TRUE)
			## convert predicted values to logits...
			#preds<-adply(.data=preds[,2:5],.margins=1,.fun=function(x)log(x)-log(1-x))	#Too slow!
			preds<-data.table(preds)
			if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){preds[,lgvrfom:=log(vrfom)-log(1-vrfom),]}
			if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){preds[,lgvsvmm:=log(vsvmm)-log(1-vsvmm),]}
			if(!inherits(boom,"try-error") && class(boom@object)!="character"){preds[,lgvboom:=log(vboom)-log(1-vboom),]}
			if(!inherits(xgbm,"try-error")){preds[,lgvxgbm:=log(vxgbm)-log(1-vxgbm),]}
			
			## and weighted average is...3=3; 5=4:5; 7=5:7 9=6:9
			ncp<-ncol(preds)
			if(ncp==3){ncprg<-3}else if(ncp==5){ncprg<-4:5}else if(ncp==7){ncprg<-5:7}else{ncprg<-6:9}
			preds[,lgweighted:=apply(X=preds,MARGIN=1,FUN=function(x,supp,ncprg){as.numeric(x[ncprg])%*%supp},supp=supp,ncprg=ncprg),]
			
			## convert weighted back to probabilities...
			preds[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
			
			## convert to raster and plot...
			rastres<-basegrid
			preds<-merge(preds,xydf,by="cellId",all.x=T)
			preds$cid<-cellFromXY(basegrid,preds[,c("x","y")])
			cid<-preds$cid;vals<-as.numeric(preds$weighted)
			rastres[cid]<-vals
			writeRaster(rastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_probPresence_Optimized.tif",sep=""),format="GTiff",overwrite=T)
			
			## let's hurdle it by the weighted prevalence...
			wmthresh<-ifelse(NROW(supp)==NROW(thresh),thresh%*%supp,mean(thresh))
			preds[,presence:=ifelse(weighted<=wmthresh,0,1),]
			trastres<-basegrid
			vals<-as.numeric(preds$presence)
			trastres[cid]<-vals
			## write as geotiff
			writeRaster(trastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_hurdle_Optimized.tif",sep=""),format="GTiff",overwrite=T)
			
			cat("Calculating variable importance...", file = logf, sep = "\n", append=TRUE)
			print("Variable importance")
			#compile variable importance-top 10
			importance<-getVariableImportance(rfom,svmm,boom,xgbm,trainset)
			importance<-getVarMetaClass(df=importance)
			importance$Species<-spcd;importance$Resolution<-resolution
			
			cat("Saving results, wrapping up ...", file = logf, sep = "\n", append=TRUE)
			save(trainset,testset,predres,testres,preds,gofs,rfom,svmm,boom,xgbm,importance, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_modelResults_Optimized.RData"))
			#topvars<-rbind(topvars,imptemp)
			
		}
		
		cat(paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn), file = logf, sep = "\n", append=TRUE)
		res<-paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn)
		
	}else{
		cat(paste0("Skipping ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn," because of <5% of sites have presence."), file = logf, sep = "\n", append=TRUE)
		res<-paste0("Skipping ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn," because of <5% of sites have presence.")
	}
	
	return(res)
	
}


