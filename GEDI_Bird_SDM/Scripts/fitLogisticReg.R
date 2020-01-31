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
#gitpath<-"/home/ubuntu/Soundscapes2Landscapes/"
gitpath<-"C:/users/lsalas/git/Soundscapes2Landscapes/"
svpath<-"c:/s2ltemp/sdmtool/results/"
#svpath<-"/home/ubuntu/S2Lanalyses/results/"
logdir<-"c:/s2ltemp/sdmtool/logs/"
#logdir<-"/home/ubuntu/S2Lanalyses/logs/"

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

evalLogisticModel<-function(logm,stepm,logniM2,loggLAI010,loggLAI102,loggLAI203,loggLAI304,loggVDRm,loggVDRb){
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
	
	## logniM2
	logniM2aic<-logniM2$aic
	logniM2rmse<-sd(logniM2$residuals)
	coeflogniM2<-as.data.frame(summary(logniM2)$coef)
	coeflogniM2$Parameter<-row.names(coeflogniM2);row.names(coeflogniM2)<-NULL
	coeflogniM2<-coeflogniM2[,c(5,1:4)]
	logniM2Ngedi<-6
	coefgedi<-subset(coeflogniM2,grepl("noised",Parameter))
	logniM2gedi05<-sum(coefgedi[,5]<0.05)
	logniM2gedi10<-sum(coefgedi[,5]<0.1)
	
	## loggLAI010
	loggLAI010aic<-loggLAI010$aic
	loggLAI010rmse<-sd(loggLAI010$residuals)
	coefloggLAI010<-as.data.frame(summary(loggLAI010)$coef)
	coefloggLAI010$Parameter<-row.names(coefloggLAI010);row.names(coefloggLAI010)<-NULL
	coefloggLAI010<-coefloggLAI010[,c(5,1:4)]
	loggLAI010Ngedi<-6
	coefgedi<-subset(coefloggLAI010,grepl("noised",Parameter))
	loggLAI010gedi05<-sum(coefgedi[,5]<0.05)
	loggLAI010gedi10<-sum(coefgedi[,5]<0.1)
	
	## loggLAI102
	loggLAI102aic<-loggLAI102$aic
	loggLAI102rmse<-sd(loggLAI102$residuals)
	coefloggLAI102<-as.data.frame(summary(loggLAI102)$coef)
	coefloggLAI102$Parameter<-row.names(coefloggLAI102);row.names(coefloggLAI102)<-NULL
	coefloggLAI102<-coefloggLAI102[,c(5,1:4)]
	loggLAI102Ngedi<-6
	coefgedi<-subset(coefloggLAI102,grepl("noised",Parameter))
	loggLAI102gedi05<-sum(coefgedi[,5]<0.05)
	loggLAI102gedi10<-sum(coefgedi[,5]<0.1)
	
	## loggLAI203
	loggLAI203aic<-loggLAI203$aic
	loggLAI203rmse<-sd(loggLAI203$residuals)
	coefloggLAI203<-as.data.frame(summary(loggLAI203)$coef)
	coefloggLAI203$Parameter<-row.names(coefloggLAI203);row.names(coefloggLAI203)<-NULL
	coefloggLAI203<-coefloggLAI203[,c(5,1:4)]
	loggLAI203Ngedi<-6
	coefgedi<-subset(coefloggLAI203,grepl("noised",Parameter))
	loggLAI203gedi05<-sum(coefgedi[,5]<0.05)
	loggLAI203gedi10<-sum(coefgedi[,5]<0.1)
	
	## loggLAI304
	loggLAI304aic<-loggLAI304$aic
	loggLAI304rmse<-sd(loggLAI304$residuals)
	coefloggLAI304<-as.data.frame(summary(loggLAI304)$coef)
	coefloggLAI304$Parameter<-row.names(coefloggLAI304);row.names(coefloggLAI304)<-NULL
	coefloggLAI304<-coefloggLAI304[,c(5,1:4)]
	loggLAI304Ngedi<-6
	coefgedi<-subset(coefloggLAI304,grepl("noised",Parameter))
	loggLAI304gedi05<-sum(coefgedi[,5]<0.05)
	loggLAI304gedi10<-sum(coefgedi[,5]<0.1)
	
	## loggVDRm
	loggVDRmaic<-loggVDRm$aic
	loggVDRmrmse<-sd(loggVDRm$residuals)
	coefloggVDRm<-as.data.frame(summary(loggVDRm)$coef)
	coefloggVDRm$Parameter<-row.names(coefloggVDRm);row.names(coefloggVDRm)<-NULL
	coefloggVDRm<-coefloggVDRm[,c(5,1:4)]
	loggVDRmNgedi<-6
	coefgedi<-subset(coefloggVDRm,grepl("noised",Parameter))
	loggVDRmgedi05<-sum(coefgedi[,5]<0.05)
	loggVDRmgedi10<-sum(coefgedi[,5]<0.1)
	
	## loggVDRb
	loggVDRbaic<-loggVDRb$aic
	loggVDRbrmse<-sd(loggVDRb$residuals)
	coefloggVDRb<-as.data.frame(summary(loggVDRb)$coef)
	coefloggVDRb$Parameter<-row.names(coefloggVDRb);row.names(coefloggVDRb)<-NULL
	coefloggVDRb<-coefloggVDRb[,c(5,1:4)]
	loggVDRbNgedi<-6
	coefgedi<-subset(coefloggVDRb,grepl("noised",Parameter))
	loggVDRbgedi05<-sum(coefgedi[,5]<0.05)
	loggVDRbgedi10<-sum(coefgedi[,5]<0.1)
	
	res<-list(logmaic=logmaic,logmrmse=logmrmse,logmNgedi=logmNgedi,logmgedi05=logmgedi05,logmgedi10=logmgedi10,coeflogm=coeflogm,
			stepmaic=stepmaic,stepmrmse=stepmrmse,stepmNgedi=stepmNgedi,stepmgedi05=stepmgedi05,stepmgedi10=stepmgedi10,coefstepm=coefstepm,
			logniM2aic=logniM2aic,logniM2rmse=logniM2rmse,logniM2Ngedi=logniM2Ngedi,logniM2gedi05=logniM2gedi05,logniM2gedi10=logniM2gedi10,coeflogniM2=coeflogniM2,
			loggLAI010aic=loggLAI010aic,loggLAI010rmse=loggLAI010rmse,loggLAI010Ngedi=loggLAI010Ngedi,loggLAI010gedi05=loggLAI010gedi05,loggLAI010gedi10=loggLAI010gedi10,coefloggLAI010=coefloggLAI010,
			loggLAI102aic=loggLAI102aic,loggLAI102rmse=loggLAI102rmse,loggLAI102Ngedi=loggLAI102Ngedi,loggLAI102gedi05=loggLAI102gedi05,loggLAI102gedi10=loggLAI102gedi10,coefloggLAI102=coefloggLAI102,
			loggLAI203aic=loggLAI203aic,loggLAI203rmse=loggLAI203rmse,loggLAI203Ngedi=loggLAI203Ngedi,loggLAI203gedi05=loggLAI203gedi05,loggLAI203gedi10=loggLAI203gedi10,coefloggLAI203=coefloggLAI203,
			loggLAI304aic=loggLAI304aic,loggLAI304rmse=loggLAI304rmse,loggLAI304Ngedi=loggLAI304Ngedi,loggLAI304gedi05=loggLAI304gedi05,loggLAI304gedi10=loggLAI304gedi10,coefloggLAI304=coefloggLAI304,
			loggVDRmaic=loggVDRmaic,loggVDRmrmse=loggVDRmrmse,loggVDRmNgedi=loggVDRmNgedi,loggVDRmgedi05=loggVDRmgedi05,loggVDRmgedi10=loggVDRmgedi10,coefloggVDRm=coefloggVDRm,
			loggVDRbaic=loggVDRbaic,loggVDRbrmse=loggVDRbrmse,loggVDRbNgedi=loggVDRbNgedi,loggVDRbgedi05=loggVDRbgedi05,loggVDRbgedi10=loggVDRbgedi10,coefloggVDRb=coefloggVDRb)
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

loopFitLogiticModel<-function(namspdat,spcd,spd,exclgedi,logf,dostep){
	## fitting  model with gedi vars
	fmln<-paste("PresAbs~",paste(subset(namspdat,namspdat!=spcd),collapse="+"),sep="")
	logm<-try(glm(as.formula(fmln), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	k<-log(nrow(spd))
	
	## Matt suggested: fit here without gedi and then do a likelihood ratio test
	fmlnng<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,exclgedi)),collapse="+"),sep="")
	logmng<-try(glm(as.formula(fmlnng), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	#Fit reduced model without each gedi var one at the time
	#gedvars<-c("_niM2_","_gLAI010_","_gLAI102_","_gLAI203_","_gLAI304_","_gVDRm_","_gVDRb_")
	gedvarex1<-subset(namspdat,grepl("_niM2_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	logniM2<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gLAI010_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggLAI010<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gLAI102_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggLAI102<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gLAI203_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggLAI203<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gLAI304_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggLAI304<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gVDRm_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggVDRm<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	gedvarex1<-subset(namspdat,grepl("_gVDRb_",namspdat));fmlg1<-paste("PresAbs~",paste(subset(namspdat,!namspdat %in% c(spcd,gedvarex1)),collapse="+"),sep="")
	loggVDRb<-try(glm(as.formula(fmlg1), data=spd, na.action=na.omit,family="binomial"),silent=TRUE)
	
	
	if(dostep==TRUE){
		stepm<-try(step(logm,trace=-1),silent=TRUE)	#using AIC as the selection criterion, improvement in loglike (k)=2
		stepmng<-try(step(logmng,trace=-1),silent=TRUE)
	}else{
		stepm<-logm; stepmng<-logmng
	}
	
	if(inherits(logm,"try-error") || inherits(stepm,"try-error")){
		cat("Failed to fit a logistic model or to perform its stepwise optimization", file = logf, sep = "\n", append=TRUE)
		reslst<-NA
	}else{
		cat("Able to fit (and optimize if requested) the logistic model. Evaluating fit and predicting...", file = logf, sep = "\n", append=TRUE)
		
		## evaluate and get predicted values: get AUC, RMSE, fitted.vals, residuals, coefficients
		## Matt suggested: do an up-down stepwise to fit
		reseval<-try(evalLogisticModel(logm,stepm,logniM2,loggLAI010,loggLAI102,loggLAI203,loggLAI304,loggVDRm,loggVDRb),silent=TRUE)
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
			
			## Repeat the LRT for each of the single-var-out models
			logniM2lrt<-lrtest(logniM2,logm)
			lglklogniM2<-logniM2lrt$LogLik[1]/logniM2lrt$LogLik[2]
			logniM2lrtdf<-logniM2lrt$Df[2];logniM2lrtX<-logniM2lrt$Chisq[2];logniM2lrtPr<-logniM2lrt[[5]][2]
			
			loggLAI010lrt<-lrtest(loggLAI010,logm)
			lglkloggLAI010<-loggLAI010lrt$LogLik[1]/loggLAI010lrt$LogLik[2]
			loggLAI010lrtdf<-loggLAI010lrt$Df[2];loggLAI010lrtX<-loggLAI010lrt$Chisq[2];loggLAI010lrtPr<-loggLAI010lrt[[5]][2]
			
			loggLAI102lrt<-lrtest(loggLAI102,logm)
			lglkloggLAI102<-loggLAI102lrt$LogLik[1]/loggLAI102lrt$LogLik[2]
			loggLAI102lrtdf<-loggLAI102lrt$Df[2];loggLAI102lrtX<-loggLAI102lrt$Chisq[2];loggLAI102lrtPr<-loggLAI102lrt[[5]][2]
			
			loggLAI203lrt<-lrtest(loggLAI203,logm)
			lglkloggLAI203<-loggLAI203lrt$LogLik[1]/loggLAI203lrt$LogLik[2]
			loggLAI203lrtdf<-loggLAI203lrt$Df[2];loggLAI203lrtX<-loggLAI203lrt$Chisq[2];loggLAI203lrtPr<-loggLAI203lrt[[5]][2]
			
			loggLAI304lrt<-lrtest(loggLAI304,logm)
			lglkloggLAI304<-loggLAI304lrt$LogLik[1]/loggLAI304lrt$LogLik[2]
			loggLAI304lrtdf<-loggLAI304lrt$Df[2];loggLAI304lrtX<-loggLAI304lrt$Chisq[2];loggLAI304lrtPr<-loggLAI304lrt[[5]][2]
			
			loggVDRmlrt<-lrtest(loggVDRm,logm)
			lglkloggVDRm<-loggVDRmlrt$LogLik[1]/loggVDRmlrt$LogLik[2]
			loggVDRmlrtdf<-loggVDRmlrt$Df[2];loggVDRmlrtX<-loggVDRmlrt$Chisq[2];loggVDRmlrtPr<-loggVDRmlrt[[5]][2]
			
			loggVDRblrt<-lrtest(loggVDRb,logm)
			lglkloggVDRb<-loggVDRblrt$LogLik[1]/loggVDRblrt$LogLik[2]
			loggVDRblrtdf<-loggVDRblrt$Df[2];loggVDRblrtX<-loggVDRblrt$Chisq[2];loggVDRblrtPr<-loggVDRblrt[[5]][2]
			
			## report the AUC, RMSE, coefs, and the LRT
			coeflogm<-reseval$coeflogm;coefstepm<-reseval$coefstepm
			resdf<-data.frame(Model=c("WithGEDI","WithGEDIoptimized","No_niM2","No_gLAI010","No_gLAI102","No_gLAI203","No_gLAI304","No_gVDRm","No_gVDRb"),
					AIC=c(reseval$logmaic,reseval$stepmaic, reseval$logniM2aic,reseval$loggLAI010aic,reseval$loggLAI102aic,reseval$loggLAI203aic,reseval$loggLAI304aic,reseval$loggVDRmaic,reseval$loggVDRbaic),
					lgRMSE=c(reseval$logmrmse,reseval$stepmrmse, reseval$logniM2rmse,reseval$loggLAI010rmse,reseval$loggLAI102rmse,reseval$loggLAI203rmse,reseval$loggLAI304rmse,reseval$loggVDRmrmse,reseval$loggVDRbrmse),
					numGEDI=c(reseval$logmNgedi,reseval$stepmNgedi, reseval$logniM2Ngedi,reseval$loggLAI010Ngedi,reseval$loggLAI102Ngedi,reseval$loggLAI203Ngedi,reseval$loggLAI304Ngedi,reseval$loggVDRmNgedi,reseval$loggVDRbNgedi),
					numGEDI05=c(reseval$logmgedi05,reseval$stepmgedi05, reseval$logniM2gedi05,reseval$loggLAI010gedi05,reseval$loggLAI102gedi05,reseval$loggLAI203gedi05,reseval$loggLAI304gedi05,reseval$loggVDRmgedi05,reseval$loggVDRbgedi05),
					numGEDI10=c(reseval$logmgedi10,reseval$stepmgedi10, reseval$logniM2gedi10,reseval$loggLAI010gedi10,reseval$loggLAI102gedi10,reseval$loggLAI203gedi10,reseval$loggLAI304gedi10,reseval$loggVDRmgedi10,reseval$loggVDRbgedi10),
					LRtest=c(lglklogm,lglkstepm,lglklogniM2,lglkloggLAI010,lglkloggLAI102,lglkloggLAI203,lglkloggLAI304,lglkloggVDRm,lglkloggVDRb),
					LRTdf=c(logmlrtdf,stepmlrtdf,logniM2lrtdf,loggLAI010lrtdf,loggLAI102lrtdf,loggLAI203lrtdf,loggLAI304lrtdf,loggVDRmlrtdf,loggVDRblrtdf),
					LRTchisq=c(logmlrtX,stepmlrtX,logniM2lrtX,loggLAI010lrtX,loggLAI102lrtX,loggLAI203lrtX,loggLAI304lrtX,loggVDRmlrtX,loggVDRblrtX),
					LRTpval=c(logmlrtPr,stepmlrtPr,logniM2lrtPr,loggLAI010lrtPr,loggLAI102lrtPr,loggLAI203lrtPr,loggLAI304lrtPr,loggVDRmlrtPr,loggVDRblrtPr))
			reslst<-list(resdf=resdf,coeflogm=coeflogm,coefstepm=coefstepm)
		}
	}
	return(reslst)
}

fitLogistic<-function(X,logf,percent.train=0.8,noise="noised",species,resamp,ratioAP,dostep=FALSE){
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
		
		if(resamp>0 && !is.na(ratioAP) && ratioAP>=1){
			res<-llply(.data=c(1:resamp),.fun=function(dd,spdata,ratioAP){
						spd<-subsampData(dat=spdata,ratioAP=ratioAP)
						resbit<-loopFitLogiticModel(namspdat=namspdat,spcd=spcd,spd=spd,exclgedi=exclgedi,logf=logf,dostep=dostep)
						return(resbit)
					},spdata=spdata,ratioAP=ratioAP)
			cat("Saving results and wrapping up", file = logf, sep = "\n", append=TRUE)
			filen<-paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,"_","_balanced_logisticModelResults.RData")
		}else{
			res<-loopFitLogiticModel(namspdat=namspdat,spcd=spcd,spd=spdata,exclgedi=exclgedi,logf=logf,dostep=dostep)
			cat("Saving results and wrapping up", file = logf, sep = "\n", append=TRUE)
			filen<-paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,"_","_AsIs_logisticModelResults.RData")
		}
		save(res,file=filen)
	}
}

## Vectorized - use option .parallel to parallelize; see details in ?l_ply
## Fitting the models to the data AsIs

### Omit step optimization so that df remain constant:
dostep<-FALSE

aa<-l_ply(.data=1:nrow(cases),.fun=function(bb,cases,gitpath,svpath,logdir,species,resamp,ratioAP,dostep){
			X<-list(gitpath=gitpath,svpath=svpath,rez=cases[bb,"rez"],spp=cases[bb,"spp"],yrsp=cases[bb,"yrspan"])
			filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
			logfile<-paste(logdir,filen,".step",sep="")
			zz <- try(file(logfile, "w"),silent=T)
			if(inherits(zz,"try-error")){
				stop("Could not open log file")
			}
			
			reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=speciesVect,resamp=resamp,ratioAP=NA,dostep)
			close(zz)
			#print(paste("Done with logistic model for",ss,"at resolution",rr,"for span",gg))
		},cases=cases,gitpath=gitpath,svpath=svpath,logdir=logdir,species=species,resamp=0,ratioAP=NA,dostep=dostep)

## Fitting the models to more balanced data
tm<-Sys.time()
aa<-l_ply(.data=1:nrow(cases),.fun=function(bb,cases,gitpath,svpath,logdir,species,resamp,ratioAP,dostep){
			X<-list(gitpath=gitpath,svpath=svpath,rez=cases[bb,"rez"],spp=cases[bb,"spp"],yrsp=cases[bb,"yrspan"])
			filen<-paste("FitLogisticBatch",format(Sys.time(),"%Y%m%d_%H%M"),sep="_")
			logfile<-paste(logdir,filen,".step",sep="")
			zz <- try(file(logfile, "w"),silent=T)
			if(inherits(zz,"try-error")){
				stop("Could not open log file")
			}
			
			reslogistic<-fitLogistic(X,logf=zz,percent.train=0.8,noise="noised",species=species,resamp=resamp,ratioAP=ratioAP,dostep=dostep)
			close(zz)
			#print(paste("Done with logistic model for",ss,"at resolution",rr,"for span",gg))
		},cases=cases,gitpath=gitpath,svpath=svpath,logdir=logdir,species=species,resamp=100,ratioAP=1,dostep=dostep)

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
