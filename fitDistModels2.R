# TODO: Add comment
# 
# Author: lsalas
###############################################################################


### Depemdencies
## we will use: dismo for gbm, mda for mars, randomForest, svm with e1071 following: https://cran.ms.unimelb.edu.au/web/packages/e1071/vignettes/svmdoc.pdf
libs<-c("rminer")
lapply(libs, require, character.only = TRUE)

## paths
dpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

### load the data - divide into train and test
load(file=paste(dpth,"occu_est.RData",sep=""))
nrecs<-nrow(occuest)
trainsize<-round(0.8*nrecs)	#setting train size to 80%
trainind<-sample(1:nrecs,trainsize);testind<-c(1:nrecs)[-trainind]
trainset<-occuest[trainind,];testset<-occuest[testind,]

## inspecting the data columns  ############
lapply(optimcovars,FUN=function(x){y<-summary(occuest[,x]);return(y)})
# some issues, so:
omitvars<-character()
for (vv in optimcovars){
	if(sum(occuest[,vv],na.rm=T)==0){
		omitvars<-c(omitvars,vv)
	}else if(sum(is.na(occuest[,vv]))==nrow(occuest)){
		omitvars<-c(omitvars,vv)
	}else if(sd(occuest[,vv],na.rm=T)==0){
		omitvars<-c(omitvars,vv)
	}else{}
}
testset<-testset[,which(!names(testset) %in% omitvars)]
trainset<-trainset[,which(!names(trainset) %in% omitvars)]
nc<-ncol(trainset)

##################################
fml<-paste("lgt_est~",paste(names(trainset[9:nc]),collapse="+"),sep="")
svmm<-fit(as.formula(fml), data=trainset, model="svm", cross=10, C=2)
rfom<-fit(as.formula(fml), data=trainset, model="randomForest",na.action=na.omit,importance=TRUE)
boom<-fit(as.formula(fml), data=trainset, model="boosting",na.action=na.omit)
nbam<-fit(as.formula(fml), data=trainset, model="naiveBayes",na.action=na.omit)
rvmm<-fit(as.formula(fml), data=trainset, model="rvm",na.action=na.omit)



#why are these not working?
#Do GOF and Importance
#predict
glmm<-fit(as.formula(fml), data=trainset, model="cv.glmnet",na.action=na.omit)
marm<-fit(as.formula(fml), data=trainset, model="mars",na.action=na.omit)
xbrt<- fit(data=trainset, model="xgboost",x=as.matrix(trainset[,9:nc]),label=trainset[,4])
