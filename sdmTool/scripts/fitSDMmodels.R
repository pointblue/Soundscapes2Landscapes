# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## Dependencies
libs<-c("rminer","raster","dismo","plyr","data.table","xgboost")
lapply(libs, require, character.only = TRUE)


## Definitions
## Any one or a list of any of the following:
species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")

resolution<-c("250M","500M","1000M") 
noise<-"noised"
gediyrs<-"3yr"

spcd<-"WESJ"
rzz<-"1000M"
percent.train<-0.8 	#the percent of data used to train the model

stratifySample<-function(df,yvar,percTrain){
	qv<-unique(df[,yvar])
	resdf<-ldply(.data=qv, .fun=function(q,df,yvar,percTrain){
				tdf<-subset(df,df[,yvar]==q);nva<-nrow(tdf);
				tdf$inOut<-rbinom(nva,1,percTrain);
				return(tdf)			
			},df=df,yvar=yvar,percTrain=percTrain)
	return(resdf)
}

calcRMSE<-function(xgb,testMatrix,naivePrev,rmsedf){
	rmsedf$predAbs<-predict(xgb,testMatrix)
	rmsedf$pred<-ifelse(rmsedf$predAbs<=naivePrev,0,1)
	#calculate RMSE for binomial data
	#for now the percent positives
	rmsedf$diff<-(rmsedf$obs-rmsedf$pred)^2
	rmse<-sqrt(sum(rmsedf$diff))
	return(rmse)
}

fitXGB<-function(trainset,testset,naivePrev,deflatedcovardf){
	trainMatrix<-as.matrix(trainset[,c(1:23)])
	testMatrix<-as.matrix(testset[,c(1:23)])
	predgrid<-deflatedcovardf[,1:26]
	predMatrix<-as.matrix(predgrid[,2:24])
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
	predgrid$predicted<-predict(bst,predMatrix)
	err <- as.numeric(sum(as.integer(pred > naivePrev) != label))/length(label)
	importance_matrix <- xgb.importance(model = bst)
	names <- dimnames(trainMatrix)[[2]]
	featuredf<-data.frame(Feature=1:NROW(names), FeatureName=names)
	imdf<-as.data.frame(importance_matrix)
	imdf<-merge(imdf,featuredf,by="Feature")
	imdf<-imdf[order(imdf$Gain,decreasing=TRUE),]
	#RETURN: bst, preds,err,imdf,predgrid
	res=list(model=bst,preds=pred,error=err,varImp=imdf,predgrid=predgrid)
}

# HERE: Loop through resolutions...
# Load the deflated bird file and filter for the desired species
dtpth<-paste0("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/birds/",rzz)
load(file=paste0(dtpth,"/deflated_",rzz,".RData"))

# HERE: Loop through species
#select only the desired species
omitspecies<-subset(species,species!=spcd)
omitnumdet<-paste0("NumDet",omitspecies)
spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet))]
spdata<-as.data.frame(na.omit(spdata))

# Here fit the stack, predict, and save the wighted average of all models
#(get rid of the following columns: x, y, gId100M)
spdata<-spdata[,which(!names(spdata) %in% c("x","y",paste0("gId",rzz)))]



#Need to vectorize this
if(sum(spdata[,spcd]==1)>0.1*nrow(spdata)){
	names(spdata)<-gsub(spcd,"PresAbs",names(spdata))
	spdata$PresAbs_f<-as.factor(as.character(spdata$PresAbs))
	
	## make train and test sets  STRATIFY!
	spdata<-stratifySample(df=spdata,yvar="PresAbs_f",percTrain=percent.train)
	trainset<-subset(spdata,inOut==1);testset<-subset(spdata,inOut==0)
	
	trainsize<-round(percent.train*nrow(spdata))	#setting train size to 80%
	naivePrev<-sum(spdata$PresAbs)/nrow(spdata)
	
	
	
	## fitting  models _HERE!!!!
	
	nc<-ncol(trainset)-3
	fmlf<-paste("PresAbs_f~",paste(names(trainset[1:nc]),collapse="+"),sep="")
	fmln<-paste("PresAbs~",paste(names(trainset[3:nc]),collapse="+"),sep="")
	svmm<-fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=2)
	rfom<-fit(as.formula(fmlf), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE)
	boom<-fit(as.formula(fmlf), data=trainset, model="boosting",na.action=na.omit)
	xgbm<-fitXGB(trainset,testset,naivePrev)
	
	## predicting to stack
	covardf<-as.data.frame(covarstack)
	covardf<-covardf[,optimcovars]
	covardf$cellId<-row.names(covardf)
	covardf<-as.data.frame(na.omit(covardf))
	cid<-as.integer(covardf$cellId)
	
	preds<-data.frame(cellId=cid)
	prfom<-as.data.frame(predict(rfom,covardf))
	preds$vrfom<-as.numeric(prfom[,2])
	psvmm<-as.data.frame(predict(svmm,covardf))
	preds$vsvmm<-as.numeric(psvmm[,2])
	pboom<-as.data.frame(predict(boom,covardf))
	preds$vboom<-as.numeric(pboom[,2])
	preds$vxgbm<-xgbm$preds
	
	## predict to test set and eval the rmse
	test<-data.frame(observed=testset[,"p_est"])
	trfom<-as.data.frame(predict(rfom,testset))
	test$prfo<-as.numeric(trfom[,2])
	tsvmm<-as.data.frame(predict(svmm,testset))
	test$psvm<-as.numeric(tsvmm[,2])
	tboom<-as.data.frame(predict(boom,testset))
	test$pboo<-as.numeric(tboom[,2])
	tbrt<-as.data.frame(predict(brtm,testset,n.trees=nt,type="response"))
	test$pbrt<-as.numeric(tbrt[,1])
	
	## individual model support is then:
	supp<-apply(test[,2:5],2,FUN=function(x,obs)sqrt(sum((x-obs)^2)/NROW(x)),obs=test$observed)
	mv<-ceiling(max(supp));supp<-mv-supp
	save(trainset,testset,test,supp,rfom,svmm,boom,brtm, file=paste(dpth,spcd,"_modelResults.RData"))
	
	## convert predicted values to logits...
	#preds<-adply(.data=preds[,2:5],.margins=1,.fun=function(x)log(x)-log(1-x))	#Too slow!
	preds<-data.table(preds)
	preds[,lgvrfom:=log(vrfom)-log(1-vrfom),]
	preds[,lgvsvmm:=log(vsvmm)-log(1-vsvmm),]
	preds[,lgvboom:=log(vboom)-log(1-vboom),]
	preds[,lgvbrtm:=log(vbrtm)-log(1-vbrtm),]
	
	## and weighted average is...
	ssup<-sum(supp)
	preds[,lgweighted:=apply(X=preds,MARGIN=1,FUN=function(x,supp,ssup)as.numeric(x[6:9])%*%supp/ssup,supp=supp,ssup=ssup),]
	## convert it back to probabilities...
	preds[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
	
	## convert to raster and plot...
	rastres<-covarstack[[1]]; rastres[]<-NA
	cid<-preds$cellId;vals<-as.numeric(preds$weighted)
	rastres[cid]<-vals
	plot(rastres)
	## let's hurdle it by the naive prevalence...
	thresh<-sum(spdata[,spcd])/nrow(spdata)
	preds[,presence:=ifelse(weighted<=thresh,0,1),]
	trastres<-covarstack[[1]]; trastres[]<-NA
	vals<-as.numeric(preds$presence)
	trastres[cid]<-vals
	plot(trastres)
	
	## write as geotiff
	writeRaster(rastres,filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/predrasters/",spcd,".tif",sep=""),format="GTiff",overwrite=T)
	print(paste("Done with",spcd))
}else{
	print(paste("Skipping",spcd,"because of <10% of sites have presence."))
}
	







# ...next species
# ...next resolution




# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


