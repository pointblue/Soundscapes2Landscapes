# TODO: Add comment
# 
# Author: lsalas
###############################################################################


### Depemdencies
## we will use: dismo for gbm, mda for mars, randomForest, svm with e1071 following: https://cran.ms.unimelb.edu.au/web/packages/e1071/vignettes/svmdoc.pdf
libs<-c("dismo","mda","randomForest","e1071")
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
##############################################

## fit the gbm
brt<-gbm.step(data=trainset, gbm.x=9:nc, gbm.y=4, tree.complexity = 3,
		learning.rate = 0.004, bag.fraction = 0.75, n.folds = 10, family = "gaussian", n.trees = 20, step.size = 20, max.trees = 2000,
		plot.main = TRUE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, keep.fold.fit = TRUE)
#save(brt,file=paste(dpth,"weme_brt.RData",sep=""))

## fit the mars - finding a simgularity?
fml<-paste("lgt_est~",paste(names(trainset[9:nc]),collapse="+"),sep="")
mar <- mda(formula=fml, data = trainset)

## fit the RF
rfm<-randomForest(as.formula(fml),data=trainset,importance=TRUE,na.action=na.omit)		# nPerm=5,xtest=testset[,9:nc],ytest=testset$lgt_est
vimp<-rfm$importance;vimp<-vimp[order(vimp[,1],decreasing=T),]

## fit the svm
svmm<-svm(as.formula(fml),data=trainset,scale=TRUE,cross=10)

#### TEST all vs testset using LRMSE

#### Eval GOF using global vs cvfold LRMSE


