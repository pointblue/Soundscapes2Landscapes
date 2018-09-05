# TODO: Add comment
# 
# Author: lsalas
###############################################################################


# The purpose of this file is to develop visuals of GOF for each species' predictive models
# Consider adding up the % contribution to models by category in a stacked bar plot, 
#	where each species is a bar, and each color of the stack is the % contribution of each of the following categories: 
#		GEDI, Hyspiri, Sentinel-I, Landsat (NDVI), GIS distance, BCM.

# ROC curves per species per model and ensembled (5 per species)

# Table of predictive accuracy (thresholded by some measure) by species and model

# Top 10 most important variables per species (think of a table bar plot with each row being a variable and 
# the bar being a stack or dodge of contribution in each species’ models)

## dependendencies
libs<-c("rminer","ggplot2","dismo","plyr","data.table","gbm","verification")
lapply(libs, require, character.only = TRUE)

## paths
gpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Geodata/"
dpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

#NO: do a permutation test for all, where you permute the OOB set on the variable being evaluated n times. Compare each result's accuracy vs. the unpermuted set
#like the French guys say to do.
getSumVarImportance<-function(rfom,svmm,boom,brtm,trainset){  #All these are loaded when the species' file is loaded
	q<-Importance(rfom,data=trainset,method="GSA")
	vidf<-data.frame(Variable=names(trainset),Importance=q$imp,Method="randomForests")
	q<-Importance(svmm,data=trainset,method="GSA")
	tdf<-data.frame(Variable=names(trainset),Importance=q$imp,Method="SVM")
	vidf<-rbind(vidf,tdf)
	q<-Importance(boom,data=trainset,method="GSA")
	tdf<-data.frame(Variable=names(trainset),Importance=q$imp,Method="BDT")
	vidf<-rbind(vidf,tdf)
	vidf<-subset(vidf,!Variable %in% c("Site","p_est","p_est_f"))
	
	varinf<-summary(brtm,plotit=FALSE);row.names(varinf)<-NULL
	sumbrtImp<-sum(varinf$rel.inf)
	varinf$rel.inf<-varinf$rel.inf/sumbrtImp
	tdf<-data.frame(Variable=varinf$var,Importance=varinf$rel.inf,Method="BRT")
	vidf<-rbind(vidf,tdf)
	
	vidf$VarCategory<-ifelse(grepl("sonoma_hyspiri",vidf$Variable),"HyspIRI",
			ifelse(grepl("sonoma_bcm",vidf$Variable),"BCM",
					ifelse(grepl("NDVI",vidf$Variable,fixed=T),"NDVI",
							ifelse(grepl("Kriging",vidf$Variable),"GEDI",
									ifelse(grepl("S1GRD_mean",vidf$Variable),"Sentinel-1","GIS other")))))
	return(vidf)
}

#Not use this
getPermutationVarImportance<-function(rfom,svmm,boom,brtm,supp,testset,nper=100){
	#1) predict against original set and store value; DONE, in the support (supp)
	#2) loop through each variable in the test set:
	#	2.1) loop through nper
	#		2.1.1) permute, predict, estimate prediction error (RMSE)
	#		2.1.2) calculate difference vs supp
	#	2.2) Calculate mean and variance of RMSE across permutations
	#3) sort by greatest to smallest prediction error
	#4) report
	
	testvals<-data.frame(observed=testset[,"p_est"])
	nt<-brtm$n.trees
	predictors<-names(testset);predictors<-predictors[which(!predictors %in% c("Site","p_est","p_est_f"))]
	varimpdf<-data.frame()
	for(vv in predictors){
		pervals<-testset[,vv]
		rmsedf<-data.frame()
		for(pp in 1:nper){
			testset[,vv]<-sample(pervals)	#permuting variable vv
			## predict to test set and eval the rmse
			trfom<-as.data.frame(predict(rfom,testset))
			test$prfo<-as.numeric(trfom[,2])
			tsvmm<-as.data.frame(predict(svmm,testset))
			test$psvm<-as.numeric(tsvmm[,2])
			tboom<-as.data.frame(predict(boom,testset))
			test$pboo<-as.numeric(tboom[,2])
			tbrt<-as.data.frame(predict(brtm,testset,n.trees=nt,type="response"))
			test$pbrt<-as.numeric(tbrt[,1])
			
			#rmse is:
			supper<-apply(test[,2:5],2,FUN=function(x,obs)sqrt(sum((x-obs)^2)/NROW(x)),obs=test$observed)
			
			#calculate difference in rmse vs the original. Most influential variable should have the most difference vs original
			vsup<-supper-supp
			
			#collect results of this permutation
			tsup<-as.data.frame(t(vsup))
			rmsedf<-rbind(rmsedf,tsup)
		}
		#summarize results of all permutations
		resv<-apply(rmsedf,2,FUN=mean)
		resdf<-as.data.frame(t(resv))
		resdf$Variable<-vv
		varimpdf<-rbind(varimpdf,resdf)
	}
	
	varimpdf$VarCategory<-ifelse(grepl("sonoma_hyspiri",varimpdf$Variable),"HyspIRI",
			ifelse(grepl("sonoma_bcm",varimpdf$Variable),"BCM",
					ifelse(grepl("NDVI",varimpdf$Variable,fixed=T),"NDVI",
							ifelse(grepl("Kriging",varimpdf$Variable),"GEDI",
									ifelse(grepl("S1GRD_mean",varimpdf$Variable),"Sentinel-1","GIS other")))))
	return(varimpdf)
}

getROCdata<-function(test){	  #test is a df loaded when the species' file is loaded
	pobj<-try(roc.plot(x=test$observed,pred=test$prfo,thresholds=seq(0,1,0.1),plot=NULL),silent=T)
	if(!inherits(pobj,"try-error")){
		rocdf<-as.data.frame(pobj$plot.data);rocdf$method<-"randomForests"
		pobj<-roc.plot(x=test$observed,pred=test$psvm,thresholds=seq(0,1,0.05),plot=NULL);rocsvm<-as.data.frame(pobj$plot.data);rocsvm$method<-"SVM";rocdf<-rbind(rocdf,rocsvm)
		pobj<-roc.plot(x=test$observed,pred=test$pboo,thresholds=seq(0,1,0.05),plot=NULL);rocboo<-as.data.frame(pobj$plot.data);rocboo$method<-"BDT";rocdf<-rbind(rocdf,rocboo)
		pobj<-roc.plot(x=test$observed,pred=test$pbrt,thresholds=seq(0,1,0.05),plot=NULL);rocbrt<-as.data.frame(pobj$plot.data);rocbrt$method<-"BRT";rocdf<-rbind(rocdf,rocbrt)
	}else{
		rocdf<-"Error calculating the ROC. Not enough data."
	}
	
	return(rocdf)
}

getROCvals<-function(test,spcd){
	aucrf<-try(roc.area(test$observed,test$prfo)$A,silent=T);aucrf<-ifelse(inherits(aucrf,"try-error"),NA,aucrf)
	aucsv<-try(roc.area(test$observed,test$psvm)$A,silent=T);aucsv<-ifelse(inherits(aucsv,"try-error"),NA,aucsv)
	aucbo<-try(roc.area(test$observed,test$pboo)$A,silent=T);aucbo<-ifelse(inherits(aucbo,"try-error"),NA,aucbo)
	aucbt<-try(roc.area(test$observed,test$pbrt)$A,silent=T);aucbt<-ifelse(inherits(aucbt,"try-error"),NA,aucbt)
	resdf<-data.frame(Species=spcd,rf.auc=aucrf,svm.auc=aucsv,bdt.auc=aucbo,brt.auc=aucbt)
	return(resdf)
}

Species<-c("WREN","MODO","NOFL","SPTO","WIWA","CAVI","BGGN","CAQU","ACWO")	# Careful with ACWO, can't do WEME
allsp_vi<-data.frame();all_rmse<-data.frame();roc.vals<-data.frame()
for(spcd in Species){
	load(paste(dpth,spcd,"_modelResults.RData"))
		
	#How many among the top 10 are in each category of variable? Subset by Method, sort by Importance
	#take first 10 and count how many in each cat of variable
	dfa<-getSumVarImportance(rfom,svmm,boom,brtm,trainset)
	ncat<-aggregate(as.formula("Importance~VarCategory"),data=dfa,FUN=NROW);ncat$Importance<-ncat$Importance/4
	names(ncat)<-c("VarCategory","NumVars")
	
	varImpMethod<-data.frame()
	for(mm in unique(dfa$Method)){
		tdf<-subset(dfa,Method==mm)
		tdf<-tdf[order(tdf$Importance,decreasing=T),]
		tdf<-tdf[1:10,]
		res<-sapply(X=ncat$VarCategory,FUN=function(X,tdf){
					rv<-sum(tdf$VarCategory==X);return(rv)
				},tdf=tdf)
		rdf<-data.frame(Method=mm,VarCategory=ncat$VarCategory,count=res)
		varImpMethod<-rbind(varImpMethod,rdf)
	}
	varImpMethod$Species<-spcd
	allsp_vi<-rbind(allsp_vi,varImpMethod)
	ref<-data.frame(Method="Reference",VarCategory=ncat$VarCategory,count=ncat$NumVars*10/sum(ncat$NumVars),Species="Reference")
	varImpMethod<-rbind(varImpMethod,ref)
	
	p<-ggplot(data=varImpMethod,aes(x=Method,y=count)) + 
			geom_bar(aes(color=VarCategory,fill=VarCategory),position="stack",stat="identity") +
			scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
			coord_flip() +
			labs(x="Modeling method",color="Variable type",fill="Variable type",y="Number of variables",title=spcd)
	jpeg(file=paste(dpth,"/Gof_VarImp_plots/",spcd,"_VarImp.jpeg",sep=""),quality=100,width=320,height=180)
	print(p)
	dev.off()
	
	
	#ROC curves
	rocdf<-getROCdata(test)
	if(class(rocdf)=="character"){
		print(paste(spcd,": ",rocdf,sep=""))
	}else{
		rocdf<-rocdf[,c(1,2,3,6)];names(rocdf)<-c("Threshold","Sensitivity","InvSpecificity","Method")
		rocdf<-subset(rocdf,Threshold>=0)
		rdf<-aggregate(as.formula("Sensitivity~InvSpecificity+Method"),data=rocdf,FUN=mean)
		p<-ggplot(data=rdf,aes(x=InvSpecificity,y=Sensitivity)) + geom_smooth(aes(color=Method),span=1.5,se=F,size=1) +
				scale_y_continuous(limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1)) +
				geom_abline(slope=1)+
				labs(x="False positive rate",y="True positive rate",color="Modeling method")
		jpeg(file=paste(dpth,"/Gof_VarImp_plots/",spcd,"_ROC.jpeg",sep=""),quality=100,width=300,height=180)
		print(p)
		dev.off()
		
	}
	aucv<-getROCvals(test,spcd)
	roc.vals<-rbind(roc.vals,aucv)
	
	
	
	#table of predictive accuracies as RMSE
	rmsqdf<-as.data.frame(t(round(supp,3)))
	rmsqdf$Species<-spcd
	all_rmse<-rbind(all_rmse,rmsqdf)
	
	print(paste("Done with", spcd))
}

refrf<-data.frame(Method="randomForests",VarCategory=ncat$VarCategory,count=ncat$NumVars*10/sum(ncat$NumVars),Species="Reference")
refsv<-data.frame(Method="SVM",VarCategory=ncat$VarCategory,count=ncat$NumVars*10/sum(ncat$NumVars),Species="Reference")
refbd<-data.frame(Method="BDT",VarCategory=ncat$VarCategory,count=ncat$NumVars*10/sum(ncat$NumVars),Species="Reference")
refbr<-data.frame(Method="BRT",VarCategory=ncat$VarCategory,count=ncat$NumVars*10/sum(ncat$NumVars),Species="Reference")
allsp_vi<-rbind(allsp_vi,refrf);allsp_vi<-rbind(allsp_vi,refsv);allsp_vi<-rbind(allsp_vi,refbd);allsp_vi<-rbind(allsp_vi,refbr)
factorder<-data.frame(Species=c(sort(Species),"Reference"),rov=c(1:10))
allsp_vi<-merge(allsp_vi,factorder,all.x=T)
allsp_vi$Species<-reorder(allsp_vi$Species,allsp_vi$rov)

#Plot of top 10 varImp variables by category of source
varorder<-data.frame(VarCategory=c("BCM","GEDI","HyspIRI","NDVI","Sentinel-1","GIS other"),orderingVal=c(1:6))
allsp_vi<-merge(allsp_vi,varorder,by="VarCategory",all.x=T)
allsp_vi<-within(allsp_vi,{
			VarCategory<-reorder(VarCategory,orderingVal)
		})

p<-ggplot(data=allsp_vi,aes(x=Species,y=count)) +
		geom_bar(aes(fill=VarCategory),position="stack",stat="identity",color="black") +
		scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
		scale_fill_brewer(palette = "Set1") +
		theme(axis.text=element_text(size=24)) +
		theme(axis.title=element_text(size=28)) +
		theme(strip.text=element_text(size=24)) +
		theme(legend.text=element_text(size=24), legend.title=element_text(size=24), legend.key.size=unit(2, 'lines')) +
		coord_flip() +
		facet_wrap(~Method,ncol=2) + 
		labs(x="",color="Variable type",fill="Variable type",y="Number of variables")

jpeg(file=paste(dpth,"/Gof_VarImp_plots/All_Species_VarImp.jpeg",sep=""),quality=100,width=960,height=800)
print(p)
dev.off()

## RMSE plot by species and machine learning model type
rmsePlot<-reshape(all_rmse[,1:4],varying=list(1:4),v.names="RMSE",direction="long")
rmsePlot$time<-ifelse(rmsePlot$time==1,"randomForests",
		ifelse(rmsePlot$time==2,"SVM",
				ifelse(rmsePlot$time==3,"BDT","BRT")))
names(rmsePlot)<-gsub("time","Method",names(rmsePlot))
rmsePlot$Species<-rep(all_rmse$Species,4)
refmeans<-data.frame(Method=c("ranromForests","SVM","BDT","BRT"),meanv=apply(all_rmse[,1:4],2,mean))

p<-ggplot(data=rmsePlot,aes(x=Method,y=RMSE)) + geom_point(aes(color=Species)) +
		scale_y_continuous(limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
		geom_point(x=1,y=refmeans$meanv[1],color="black",shape=17) +
		geom_point(x=2,y=refmeans$meanv[2],color="black",shape=17) +
		geom_point(x=3,y=refmeans$meanv[3],color="black",shape=17) +
		geom_point(x=4,y=refmeans$meanv[4],color="black",shape=17) +
		geom_hline(yintercept=mean(rmsePlot$RMSE),color="black",linetype="dotted",size=0.7)

jpeg(file=paste(dpth,"/Gof_VarImp_plots/All_Species_RMSE.jpeg",sep=""),quality=100,width=340,height=220)
print(p)
dev.off()

####################################################################################################
## IGNORE
pdf<-aggregate(as.formula("Importance~Method+VarCategory"),data=dfbr,FUN=sum)
p<-ggplot(data=pdf,aes(x=Method,y=Importance)) + 
		geom_bar(aes(color=VarCategory,fill=VarCategory),position="stack",stat="identity") +
		labs(x="Modeling method",color="Variable type",fill="Variable type")

#take the 10 globally most important variables and plot their relImp across models
rankedImp<-aggregate(as.formula("Importance~Variable"),data=dfbr,FUN=sum); names(rankedImp)<-c("Variable","GlobalImportance")
rankedImp<-rankedImp[order(rankedImp$GlobalImportance,decreasing=TRUE),];rankedImp$Rank<-1:nrow(rankedImp)
df<-merge(dfbr,rankedImp,by="Variable")
pdf<-subset(df,Rank<11)
pdf$Variable<-reorder(pdf$Variable,pdf$Rank*-1)
p<-ggplot(data=pdf,aes(x=Variable,y=Importance)) + 
		geom_bar(aes(color=Method,fill=Method),stat="identity",position="stack") + 
		coord_flip() + labs(x="",color="Modeling method",fill="Modeling method")
