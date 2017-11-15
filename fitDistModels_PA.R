# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("rminer","raster","dismo")
lapply(libs, require, character.only = TRUE)

## paths
gpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Geodata/"
dpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

#functions
getLogit<-function(x){
	lgv<-log(x)-log(1-x)
	return(lgv)
}

## selection inputs
spcd<-"WREN"
percent.train<-0.8 	#the percent of data used to train the model

################### load the data - divide into train and test
load(file=paste(dpth,"optimcovars.RData",sep=""))
data<-read.csv(paste(dpth,"allspecies_allrecs_site.summaries_geo.csv",sep=""))
spdata<-data[,c("Site",spcd)]
resmx<-data[,c("Easting","Northing")]
covarstack<-stack(paste(gpth,"covarstack.grd",sep=""))
resext<-extract(covarstack,resmx,df=TRUE)
presest<-cbind(spdata,resext[,optimcovars])
names(presest)<-gsub(spcd,"p_est",names(presest))
presest$p_est_f<-as.factor(as.character(presest$p_est))

#covars are normalized
## inspecting the covar data columns - it should all be fine  ############
lapply(optimcovars,FUN=function(x){y<-summary(presest[,x]);return(y)})
## two records missing, so...
presest<-as.data.frame(na.omit(presest))

nrecs<-nrow(presest)
trainsize<-round(percent.train*nrecs)	#setting train size to 80%
trainind<-sample(1:nrecs,trainsize);testind<-c(1:nrecs)[-trainind]
trainset<-presest[trainind,];testset<-presest[testind,]

##################################
nc<-ncol(trainset)-1
fmlf<-paste("p_est_f~",paste(names(trainset[3:nc]),collapse="+"),sep="")
fmln<-paste("p_est~",paste(names(trainset[3:nc]),collapse="+"),sep="")
svmm<-fit(as.formula(fmlf), data=trainset, model="svm", cross=10, C=2)
rfom<-fit(as.formula(fmlf), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE)
boom<-fit(as.formula(fmlf), data=trainset, model="boosting",na.action=na.omit)
#nbam<-fit(as.formula(fmlf), data=trainset, model="naiveBayes",na.action=na.omit)
#glmm<-fit(as.formula(fmlf), data=trainset, model="cv.glmnet",family="binomial")
brtm<-gbm.step(data=trainset, gbm.x=3:nc, gbm.y=2, tree.complexity = 3,
		learning.rate = 0.004, bag.fraction = 0.75, n.folds = 10, family = "bernoulli", n.trees = 20, step.size = 20, max.trees = 2000,
		plot.main = TRUE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)

#predicting to stack to test
pbrt<-predict(covarstack,brtm,n.trees=140,type="response")

covardf<-as.data.frame(covarstack)
covardf<-covardf[,optimcovars]
covardf$cellId<-row.names(covardf)
covardf<-as.data.frame(na.omit(covardf))
cid<-as.integer(covardf$cellId)


prfom<-as.data.frame(predict(rfom,covardf))
preds<-data.frame(vrfom=as.numeric(prfom[,2]))
psvmm<-as.data.frame(predict(svmm,covardf))
preds$vsvmm<-as.numeric(psvmm[,2])
pboom<-as.data.frame(predict(boom,covardf))
preds$vboom<-as.numeric(pboom[,2])
#pnbam<-as.data.frame(predict(nbam,covardf))
#preds$vnbam<-as.numeric(pnbam[,2])
#pglmm<-as.data.frame(predict(glmm,covardf))
#preds$vglmm<-as.numeric(pglmm[,2])
pbrtm<-as.data.frame(predict(brtm,covardf,n.trees=140,type="response"))
preds$vbrtm<-as.numeric(pbrtm[,1])

#predict to test set and eval the rmse
test<-data.frame(observed=testset[,"p_est"])
trfom<-as.data.frame(predict(rfom,testset))
test$prfo<-as.numeric(trfom[,2])

tsvmm<-as.data.frame(predict(svmm,testset))
test$psvm<-as.numeric(tsvmm[,2])

tboom<-as.data.frame(predict(boom,testset))
test$pboo<-as.numeric(tboom[,2])

#tnbam<-as.data.frame(predict(nbam,testset))
#test$pnba<-as.numeric(tnbam[,2])

tbrt<-as.data.frame(predict(brtm,testset,n.trees=140,type="response"))
test$pbrt<-as.numeric(tbrt[,1])

#support is then:
supp<-apply(test[,2:5],2,FUN=function(x,obs)sqrt(sum((x-obs)^2)),obs=test$observed)
preds$lgrfom<-getLogit(preds$vrfom)
preds$lgsvmm<-getLogit(preds$vsvmm)
preds$lgboom<-getLogit(preds$vboom)
preds$lgbrtm<-getLogit(preds$vbrtm)


base<-covarstack[[1]]
base[]<-NA
base[cid]<-vals



