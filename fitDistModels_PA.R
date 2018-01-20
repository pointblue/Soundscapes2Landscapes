# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## dependendencies
libs<-c("rminer","raster","dismo","plyr","data.table")
lapply(libs, require, character.only = TRUE)
# files: 
#	covarstack.grd (the full stack of covariates - from createStack_attributeObs.R)
#	optimcovars.RData (the file with the list of VIF-selected covariates - from createStack_attributeObs.R)
#	allspecies_allrecs_site.summaries_geo.csv (the file that lists the detections by species and site - from file appendNewSpecies_toAllSpecies.R)

## paths
gpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Geodata/"
dpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

## selection inputs
percent.train<-0.8 	#the percent of data used to train the model

################### load the data - divide into train and test
load(file=paste(dpth,"optimcovars.RData",sep=""))
data<-read.csv(paste(dpth,"allspecies_allrecs_site.summaries_geo.csv",sep=""))
resmx<-data[,c("Easting","Northing")]
covarstack<-stack(paste(gpth,"covarstack.grd",sep=""))

Species<-c("WREN","MODO","NOFL","SPTO","WEME","WIWA","CAVI","BGGN","CAQU","ACWO")	# Careful with ACWO
Species<-Species[which(Species %in% names(data))]

#Need to vectorize this
for(spcd in Species){
	spdata<-data[,c("Site",spcd)]
	if(sum(spdata[,spcd]==1)>0.1*nrow(spdata)){
		resext<-extract(covarstack,resmx,df=TRUE)
		presest<-cbind(spdata,resext[,optimcovars])
		names(presest)<-gsub(spcd,"p_est",names(presest))
		presest$p_est_f<-as.factor(as.character(presest$p_est))
		
		#covars are normalized
		## inspecting the covar data columns - it should all be fine  ############
		## NO NEED TO DO THIS AGAIN
		#lapply(optimcovars,FUN=function(x){y<-summary(presest[,x]);return(y)})
		
		## two records missing, so...
		presest<-as.data.frame(na.omit(presest))
		
		## make train and test sets
		nrecs<-nrow(presest)
		trainsize<-round(percent.train*nrecs)	#setting train size to 80%
		trainind<-sample(1:nrecs,trainsize);testind<-c(1:nrecs)[-trainind]
		trainset<-presest[trainind,];testset<-presest[testind,]
		
		##################################
		## fitting models
		nc<-ncol(trainset)-1
		fmlf<-paste("p_est_f~",paste(names(trainset[3:nc]),collapse="+"),sep="")
		fmln<-paste("p_est~",paste(names(trainset[3:nc]),collapse="+"),sep="")
		svmm<-fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=2)
		rfom<-fit(as.formula(fmlf), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE)
		boom<-fit(as.formula(fmlf), data=trainset, model="boosting",na.action=na.omit)
		if(spcd=="ACWO"){
			brtm<-gbm.step(data=trainset, gbm.x=3:nc, gbm.y=2, tree.complexity = 2,
					learning.rate = 0.00001, bag.fraction = 0.85, n.folds = 10, family = "bernoulli", n.trees = 10, step.size = 5, max.trees = 2000,
					plot.main = TRUE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
			
		}else{
			brtm<-gbm.step(data=trainset, gbm.x=3:nc, gbm.y=2, tree.complexity = 3,
					learning.rate = 0.001, bag.fraction = 0.75, n.folds = 10, family = "bernoulli", n.trees = 20, step.size = 20, max.trees = 2000,
					plot.main = TRUE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
			
		}
		
		nt<-brtm$n.trees
		
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
		pbrtm<-as.data.frame(predict(brtm,covardf,n.trees=nt,type="response"))
		preds$vbrtm<-as.numeric(pbrtm[,1])
		
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
	
}

