# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(lmtest); library(plyr)

species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
		"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
		"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
		"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
resolution<-c("250M","500M","1000M") #
gediyrs<-c("1yr","2yr","3yr")
gitpath<-"/home/ubuntu/Soundscapes2Landscapes/"
#gitpath<-"C:/users/lsalas/git/Soundscapes2Landscapes/"
#svpath<-"c:/s2ltemp/sdmtool/results/"
svpath<-"/home/ubuntu/S2Lanalyses/results/"
#logdir<-"c:/s2ltemp/sdmtool/logs/"
logdir<-"/home/ubuntu/S2Lanalyses/logs/"

cases<-expand.grid(spp=species,rez=resolution,yrspan=gediyrs,stringsAsFactors=FALSE)

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

evalLogisticModel<-function(logm,stepm){
	## logm
	logmaic<-logm$aic
	logmrmse<-sd(logm$residuals)
	coeflogm<-as.data.frame(summary(logm)$coef)
	coeflogm$Parameter<-row.names(coeflogm);row.names(coeflogm)<-NULL
	coeflogm<-coeflogm[,c(5,1:4)]
	logmNgedi<-sum(grepl("noised",coeflogm$Parameter))
	coeflogmgedi<-subset(coeflogm,grepl("noised",Parameter))
	logmgedi05<-sum(coeflogmgedi[,5]<0.05)
	logmgedi10<-sum(coeflogmgedi[,5]<0.1)
	
	## stepm
	stepmaic<-stepm$aic
	stepmrmse<-sd(stepm$residuals)
	coefstepm<-as.data.frame(summary(stepm)$coef)
	coefstepm$Parameter<-row.names(coefstepm);row.names(coefstepm)<-NULL
	coefstepm<-coefstepm[,c(5,1:4)]
	stepmNgedi<-sum(grepl("noised",coefstepm$Parameter))
	coefstepmgedi<-subset(coefstepm,grepl("noised",Parameter))
	stepmgedi05<-sum(coefstepmgedi[,5]<0.05)
	stepmgedi10<-sum(coefstepmgedi[,5]<0.1)
	
	res<-list(logmaic=logmaic,logmrmse=logmrmse,logmNgedi=logmNgedi,logmgedi05=logmgedi05,logmgedi10=logmgedi10,coeflogm=coeflogm,
			stepmaic=stepmaic,stepmrmse=stepmrmse,stepmNgedi=stepmNgedi,stepmgedi05=stepmgedi05,stepmgedi10=stepmgedi10,coefstepm=coefstepm)
	return(res)
}

subsampData<-function(dat,ratioAP=1.5){
	dfa<-subset(dat,PresAbs==1);nrp<-nrow(dfa)
	dfb<-subset(dat,PresAbs==0)
	ratAP<-ratioAP
	if((nrow(dfb)/nrp) < ratioAP){
		ratAP<-1.25
	}
	sizbb<-round(nrp*ratAP)
	dfbb<-dfb[sample(1:nrow(dfb),size=sizbb),]
	spd<-rbind(dfa,dfbb)
	return(spd)
}

loopFitLogiticModel<-function(namspdat,spcd,spd,exclgedi,logf){
	## fitting  model with gedi vars
	fmln<-paste("PresAbs~",paste(subset(namspdat,namspdat!=spcd),collapse="+"),sep="")
	logm<-try(glm(as.formula(fmln), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	k<-log(nrow(spd))
	stepm<-try(step(logm,trace=-1),silent=TRUE)	#using AIC as the selection criterion, improvement in loglike (k)=2
	## Matt suggested: fit here without gedi and then do a likelihood ratio test
	fmlnng<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,exclgedi)),collapse="+"),sep="")
	logmng<-try(glm(as.formula(fmlnng), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	stepmng<-try(step(logmng,trace=-1),silent=TRUE)
	
	if(inherits(logm,"try-error") || inherits(stepm,"try-error")){
		cat("Failed to fit a logistic model or to perform its stepwise optimization", file = logf, sep = "\n", append=TRUE)
		reslst<-NA
	}else{
		cat("Able to fit and optimize logistic model. Evaluating fit and predicting...", file = logf, sep = "\n", append=TRUE)
		
		## evaluate and get predicted values: get AUC, RMSE, fitted.vals, residuals, coefficients
		## Matt suggested: do an up-down stepwise to fit
		reseval<-try(evalLogisticModel(logm,stepm),silent=TRUE)
		if(inherits(reseval,"try-error")){
			cat(reseval, file = logf, sep = "\n", append=TRUE)
			reslst<-NA
		}
		
		## Try LRT
		cat("Attempting likelihood ratio test vs model without GEDI...", file = logf, sep = "\n", append=TRUE)
		lglklogm<-NA;lglkstepm<-NA;logmlrtdf<-NA;stepmlrtdf<-NA;logmlrtX<-NA;stepmlrtX<-NA;logmlrtPr<-NA;stepmlrtPr<-NA
		if(inherits(logmng,"try-error") || inherits(stepmng,"try-error")){
			cat("Failed to fit a logistic model without GEDI data or to perform its stepwise optimization", file = logf, sep = "\n", append=TRUE)
			reslst<-NA
		}else{
			logmlrt<-lrtest(logmng,logm)
			lglklogm<-logmlrt$LogLik[1]/logmlrt$LogLik[2]
			logmlrtdf<-logmlrt$Df[2];logmlrtX<-logmlrt$Chisq[2];logmlrtPr<-logmlrt[[5]][2]
			
			stepmlrt<-lrtest(stepmng,stepm)
			lglkstepm<-stepmlrt$LogLik[1]/stepmlrt$LogLik[2]
			stepmlrtdf<-stepmlrt$Df[2];stepmlrtX<-stepmlrt$Chisq[2];stepmlrtPr<-stepmlrt[[5]][2]
			
			## report the AUC, RMSE, coefs, and the LRT
			coeflogm<-reseval$coeflogm;coefstepm<-reseval$coefstepm
			resdf<-data.frame(Model=c("WithGEDI","WithGEDIoptimized"),
					AIC=c(reseval$logmaic,reseval$stepmaic),
					lgRMSE=c(reseval$logmrmse,reseval$stepmrmse),
					numGEDI=c(reseval$logmNgedi,reseval$stepmNgedi),
					numGEDI05=c(reseval$logmgedi05,reseval$stepmgedi05),
					numGEDI10=c(reseval$logmgedi10,reseval$stepmgedi10),
					LRtest=c(lglklogm,lglkstepm),
					LRTdf=c(logmlrtdf,stepmlrtdf),
					LRTchisq=c(logmlrtX,stepmlrtX),
					LRTpval=c(logmlrtPr,stepmlrtPr))
			reslst<-list(resdf=resdf,coeflogm=coeflogm,coefstepm=coefstepm)
		}
	}
	return(reslst)
}

fitLogistic<-function(X,logf,percent.train=0.8,noise="noised",species,resamp,ratioAP){
	#logf<-zz;percent.train=0.8;noise="noised";species=species
	
	pathToGit<-X[["gitpath"]];svpth<-X[["svpath"]];resolution<-X[["rez"]]
	spcd<-X[["spp"]];gediyr<-X[["yrsp"]]
		
	startdttm<-format(Sys.time(), "%Y%m%d_%H%M")		#starting datetime
	cat("START OF LOGISTIC MODEL FITTING RUN", file = logf, sep = "\n\n", append=TRUE)
	
	## Check that we have the path to save the files
	chkpth<-checkSavePath(svpth=svpth,rez=resolution)  # Check that the folders exist in svpth and for each resolution level
	if(chkpth!=""){
		cat(chkpth)
		cat(chkpth, file=logf, sep = "\n", append=TRUE)	#log this
	}else{
		cat("Checking the results path for needed folders... OK", file = logf, sep = "\n", append=TRUE)
	}
	
	####
	# Load the deflated bird file and filter for the loop species
	dtpth<-paste0(pathToGit,"sdmTool/data/Birds/",resolution)
	load(file=paste0(dtpth,"/deflated_",resolution,".RData"))	
	cat("Loaded and preparing the corresponding bird data...", file = logf, sep = "\n", append=TRUE)
	
	#select only the desired species from the data
	omitspecies<-subset(species,species!=spcd)
	omitnumdet<-paste0("NumDet",omitspecies)
	
	#get covars and the current species' data
	spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet))]
	spdata<-as.data.frame(na.omit(spdata))
	
	#select only the desired species from the data
	omitspecies<-subset(species,species!=spcd)
	omitnumdet<-paste0("NumDet",omitspecies)
	
	#get covars and the current species' data
	spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet))]
	spdata<-as.data.frame(na.omit(spdata))
	
	# Get the species data for the right gediyr, and to include/exclude gedi
	# these filters are applied NOT to the data, but to the model formula
	# see line 136 below for example
	namspdat<-names(spdata)
	namspdat<-subset(namspdat,!namspdat %in% c("x","y",paste0("gId",resolution),paste0("NumDet",spcd)))
	exclyr<-c("1yr","2yr","3yr");exclyr<-exclyr[which(exclyr!=gediyr)]
	namspdat<-subset(namspdat,!grepl(exclyr[1],namspdat) & !grepl(exclyr[2],namspdat))
	
	exclgedi<-subset(namspdat,grepl("_3yr_",namspdat) | grepl("_2yr_",namspdat) | grepl("_1yr_",namspdat))
	
	cat("Dataset ready. Attempt model fitting... ", file = logf, sep = "\n", append=TRUE)
	
	# Then fit the model, predict, and save
	if(sum(spdata[,spcd]==1)>0.05*nrow(spdata)){
		cat("Dataset has > 5% of cells with presence", file = logf, sep = "\n", append=TRUE)
		
		names(spdata)<-gsub(spcd,"PresAbs",names(spdata))
		
		if(resamp>0 && !is.na(ratioAP) && ratioAP>1){
			res<-llply(.data=c(1:resamp),.fun=function(bb,spdata,ratioAP){
						spd<-subsampData(dat=spdata,ratioAP=ratioAP)
						resbit<-loopFitLogiticModel(namspdat=namspdat,spcd=spcd,spd=spd,exclgedi=exclgedi,logf=logf)
						return(resbit)
					},spdata=spdata,ratioAP=ratioAP)
			cat("Saving results and wrapping up", file = logf, sep = "\n", append=TRUE)
			filen<-paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,"_","_balanced_logisticModelResults.RData")
		}else{
			res<-loopFitLogiticModel(namspdat=namspdat,spcd=spcd,spd=spdata,exclgedi=exclgedi,logf=logf)
			cat("Saving results and wrapping up", file = logf, sep = "\n", append=TRUE)
			filen<-paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,"_","_AsIs_logisticModelResults.RData")
		}
		save(res,file=filen)
	}
}

## Vectorized - use option .parallel to parallelize; see details in ?l_ply
## Fitting the models to the data AsIs


aa<-l_ply(.data=1:nrow(cases),.fun=function(bb,cases,gitpath,svpath,logdir,speciesVect,resamp,ratioAP){
			X<-list(gitpath=gitpath,svpath=svpath,rez=cases[bb,"rez"],spp=cases[bb,"spp"],yrsp=cases[bb,"yrspan"])
			filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
			logfile<-paste(logdir,filen,".step",sep="")
			zz <- try(file(logfile, "w"),silent=T)
			if(inherits(zz,"try-error")){
				stop("Could not open log file")
			}
			
			reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=speciesVect,resamp=resamp,ratioAP=NA)
			close(zz)
			#print(paste("Done with logistic model for",ss,"at resolution",rr,"for span",gg))
		},cases=cases,gitpath=gitpath,svpath=svpath,logdir=logdir,speciesVect=species,resamp=0,ratioAP=NA)

## Fitting the models to more balanced data
tm<-Sys.time()
aa<-l_ply(.data=1:nrow(cases),.fun=function(bb,cases,gitpath,svpath,logdir,speciesVect,resamp,ratioAP){
			X<-list(gitpath=gitpath,svpath=svpath,rez=cases[bb,"rez"],spp=cases[bb,"spp"],yrsp=cases[bb,"yrspan"])
			filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
			logfile<-paste(logdir,filen,".step",sep="")
			zz <- try(file(logfile, "w"),silent=T)
			if(inherits(zz,"try-error")){
				stop("Could not open log file")
			}
			
			reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=speciesVect,resamp=resamp,ratioAP=ratioAP)
			close(zz)
			#print(paste("Done with logistic model for",ss,"at resolution",rr,"for span",gg))
		},cases=cases,gitpath=gitpath,svpath=svpath,logdir=logdir,speciesVect=species,resamp=500,ratioAP=1.5)

Sys.time()-tm

###########################################################################################################################

## NOT DO: Serialized...
for(ss in species){
	for(rr in resolution){
		for(gg in gediyrs){
			X<-list(gitpath=gitpath,svpath=svpath,rez="250M",spp="WESJ",yrsp="3yr")
			filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
			logfile<-paste(logdir,filen,".step",sep="")
			zz <- try(file(logfile, "w"),silent=T)
			if(inherits(zz,"try-error")){
				stop("Could not open log file")
			}
			
			reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=species)
			close(zz)
			print(paste("Done with logistic model for",ss,"at resolution",rr,"for span",gg))
		}
	}
}

## debugging
#X<-list(gitpath=gitpath,svpath=svpath,rez="250M",spp="WESJ",yrsp="1yr")
#filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
#logfile<-paste(logdir,filen,".step",sep="")
#zz <- try(file(logfile, "w"),silent=T)
#if(inherits(zz,"try-error")){
#	stop("Could not open log file")
#}
#reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=species)
