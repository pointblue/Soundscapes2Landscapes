# Title: fitSDMmodels_optimized_allOpts.R
# Author: Patrick Burns [pb463@nau.edu] & Leo Salas [lsalas@pointblue.org]
# Purpose: set of functions used in fitSDMbatch.R. 
#  ***Note this version of the script includes a routine for subsampling the species observation data. 
# TODO: 
###############################################################################



####
#### 0. Functions 
####

# Fit XGB model. Most models are fit with the rminer package. Although XGB is an option within rminer, rminer::fit() throws an error for this type of model. Therefore we wrote a custom fitting function instead. 
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
  
  # Custom kappa eval_metric
  # kappa <- function(preds, dtrain) {
  #   label=getinfo(dtrain,"label")
  #   k=vcd::Kappa(table(preds>0.5,label))
  #   return(list(metric = "kappa", value = as.numeric(k$Unweighted[1])))
  # }
  
  param.grid<-expand.grid(max_depth = c(2,3,5),gamma = c(1, 2, 3), colsample_bytree = c(0.4, 0.7, 1.0), 
                          min_child_weight = c(0.5, 1, 1.5), eta=1, silent = 0,  nthread = 2,			
                          objective = "binary:logistic", eval_metric = "error", eval_metric = "auc")
  
  xgboptim<-data.frame()
  for(kk in 1:nrow(param.grid)){
    param<-as.list(param.grid[kk,])
    xbst <- xgb.train(param, dtrain, nrounds = 100, watchlist, early_stopping_rounds=10, maximize=TRUE,verbose=0)
    merr<-min(xbst$evaluation_log$eval_error);mauc<-max(xbst$evaluation_log$eval_auc)#chg
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
  predtrain<-predict(xbst,dtrain)
  predtest <- predict(xbst, dtest)
  predgrid<-predict(xbst,predMatrix)
  predall<-predict(xbst,allMatrix)
  
  #varImportance
  importance_matrix <- xgb.importance(model = xbst)
  imdf<-as.data.frame(importance_matrix)
  imdf<-imdf[order(imdf$Gain,decreasing=TRUE),]
  
  #RETURN: bst, preds,predgrid,imdf
  res=list(model=xbst,predtrain=predtrain,predtest=predtest,predgrid=predgrid,predall=predall,varimp=imdf)
}

# Uses rminer::Importance to measure variable importance
retrieveVarImp<-function(mdl,trainset,type, meth){
  impres<-Importance(M = mdl, data = trainset, RealL = 7, method = meth, measure = "AAD") #Ref: https://www.sciencedirect.com/science/article/pii/S0020025512007098?via%3Dihub. Recommend 7 levels.
  impdf<-data.frame(Variable=names(trainset),AbsImportance=impres$imp,Model=type)
  impdf<-impdf[order(impdf$AbsImportance,decreasing=TRUE),]
  # impdf<-impdf[1:10,] # PB20190305
  impdf$RelImportance<-lapply(impdf$AbsImportance,FUN=function(x,sumI){absi<-x/sumI;return(absi)},sumI=sum(impdf$AbsImportance))
  return(impdf)
}

# Aggregates importance measurements from different models into one data frame
getVariableImportance<-function(rfom,svmm,boom,xgbm,mlpe,glmn,knn,trainset,meth){
  imptemp<-data.frame()
  if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){imprfo<-retrieveVarImp(mdl=rfom,trainset=trainset,type="RF",meth=meth);imptemp<-rbind(imptemp,imprfo)}
  if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){impsvm<-retrieveVarImp(mdl=svmm,trainset=trainset,type="SVM",meth=meth);imptemp<-rbind(imptemp,impsvm)}
  if(!inherits(boom,"try-error") && class(boom@object)!="character"){impboo<-retrieveVarImp(mdl=boom,trainset=trainset,type="Boo",meth=meth);imptemp<-rbind(imptemp,impboo)}
  if(!inherits(xgbm,"try-error")){
    xgbImpRows = nrow(xgbm$varimp)
    impxgb<-xgbm$varimp[,c("Feature","Gain")];names(impxgb)<-c("Variable","AbsImportance")
    impxgb$Model<-"XGB";impxgb<-impxgb[1:xgbImpRows,]
    impxgb$RelImportance<-lapply(impxgb$AbsImportance,FUN=function(x,sumI){absi<-x/sumI;return(absi)},sumI=sum(impxgb$AbsImportance))
    imptemp<-rbind(imptemp,impxgb)
  }
  if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){impmlp<-retrieveVarImp(mdl=mlpe,trainset=trainset,type="MLPE",meth=meth);imptemp<-rbind(imptemp,impmlp)}
  if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){impglm<-retrieveVarImp(mdl=glmn,trainset=trainset,type="GLMN",meth=meth);imptemp<-rbind(imptemp,impglm)}
  if(!inherits(knn,"try-error") && class(knn@object)!="character"){impknn<-retrieveVarImp(mdl=knn,trainset=trainset,type="KNN",meth=meth);imptemp<-rbind(imptemp,impknn)}
  return(imptemp)
}

# Associates specific variables with broader variable type groups
getVarMetaClass<-function(df){
  df$VarType<-ifelse(substr(df$Variable,1,3) %in% c("aet","cwd","pet","ppt","tmx","tmn"),"BCM",
                     ifelse(substr(df$Variable,1,5) %in% c("Coast","Stree","Strea"),"AUX",
                            ifelse(substr(df$Variable,1,4)=="dem_","AUX",
                                   ifelse(substr(df$Variable,1,4)=="ndvi","NDVI","GEDI"))))
  return(df)
}

# Make sure the save path exists
checkSavePath<-function(svpth,rez){
  if(!dir.exists(svpth)){
    stop(paste("ERROR: The path to save files does not exist. Please create the folder", svpth, "before continuing with code execution."))
  }

  inputPath = paste0(svpath, "input/")
  if(!dir.exists(inputPath)){
  dir.create(inputPath)
  }
  
  mergedResPath = paste0(svpath, "mergedResults/")
  if(!dir.exists(mergedResPath)){
    dir.create(mergedResPath)
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

# Calculate a variety of goodness of fit metrics
getGOF<-function(testres,thresh){
  ncd<-ncol(testres)
  if(ncd==2){
    qq<-data.frame(V1=laply(2:ncd,.fun=function(rr,tedf,thv){
      np<-thv[rr-1];
      z<-ifelse(tedf[,rr]>=np,1,0);
      return(z)},tedf=testres,thv=thresh))
    names(qq)<-paste0("h",names(testres)[2:ncd]) 
  } else if(ncd>2){
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
    ccnam<-ifelse(vanm=="hprfo","RF",
                  ifelse(vanm=="hpsvm","SVM",
                         ifelse(vanm=="hpboo","Boo",
                                ifelse(vanm=="hpxgb","XGB", 
                                       ifelse(vanm=="hpmlp","MLPE", 
                                              ifelse(vanm=="hpglm", "GLMN", 
                                                     ifelse(vanm=="hpknn", "KNN",
                                                            ifelse(vanm=="hweighted", "EWA", "other?"))))))))
    truePos=sum(dfp[,vanm]>0);falseNeg=sum(dfp[,vanm]==0);trueNeg=sum(dfn[,vanm]==0);falsePos=sum(dfn[,vanm]>0) #chg
    
    # metrics computed with rminer
    accval_RM<-mmetric(y=as.factor(testres[,1]),x=testres[,(cc+1)],metric="ACC",D=thresh[cc])
    kappaval_RM<-mmetric(y=as.factor(testres[,1]),x=testres[,(cc+1)],metric="KAPPA",D=thresh[cc])
    berval_RM<-mmetric(y=as.factor(testres[,1]),x=testres[,(cc+1)],metric="BER",D=thresh[cc])
    rmseval_RM<-mmetric(y=testres[,1],x=testres[,(cc+1)],metric="RMSE")
    
    px=matrix(nrow=nrow(testres),ncol=2)
    px[,2]=testres[,(cc+1)] # class 1 (presence)
    px[,1]=1-px[,2] # class 0 (absence)
    aucval_RM<-mmetric(y=as.factor(testres[,1]),x=px,metric="AUC")
    
    # metrics computed with psych (questionable)
    kappaval_PSY<-cohen.kappa(cbind(testres[,1],qq[,cc]))
    nt<-nrow(testres);phiv<-cor(testres[,1],testres[,(cc+1)])
    aucval_PSY<-try(AUC(BR=(truePos+falseNeg)/nt,SR=(truePos+falsePos)/nt,Phi=phiv,plot="n"),silent=TRUE)
    if(inherits(aucval_PSY,"try-error")){
      aucval_PSY<-list(AUC=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA)
    }
    
    # metrics computed manually
    precval = truePos/(truePos + falsePos)
    sensval = truePos/(truePos + falseNeg)
    specval = trueNeg/(trueNeg + falsePos)
    baccval = (sensval + specval)/2
    f1val = (2 * truePos)/(2 * truePos + falsePos + falseNeg)
    TSS = sensval + specval - 1
    
    # Combine all the metrics together
    tdf<-data.frame(Model=ccnam,Threshold=thresh[cc],truePos=truePos,falsePos=falsePos,trueNeg=trueNeg,falseNeg=falseNeg,
                    Kappa_PSY=kappaval_PSY$kappa, Kappa_RM=kappaval_RM,
                    AUC_PSY=aucval_PSY$AUC, AUC_RM=aucval_RM,
                    RMSE=rmseval_RM, Acc=accval_RM, 
                    BalAcc=baccval, BalErr=berval_RM, 
                    Precision=precval, Sensitivity=sensval, Specificity=specval,
                    F1=f1val, TSS=TSS)
    mdf<-rbind(mdf,tdf)
  }
  return(mdf)
}

# Use fitted models to make predictions
getPredicted<-function(preds,predgriddf,trainset,testset,alldata,rfom,svmm,boom,xgbm,mlpe,glmn,knn){
  trained<-data.frame(observed=trainset[,"PresAbs"])
  test<-data.frame(observed=testset[,"PresAbs"])
  alld<-data.frame(observed=alldata[,"PresAbs"])
  
  if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){
    prfom<-as.data.frame(predict(rfom,predgriddf))
    preds$prfo<-round(as.numeric(prfom[,2]),4)
    trfom<-as.data.frame(predict(rfom,testset))
    test$prfo<-round(as.numeric(trfom[,2]),4)
    trrfom<-as.data.frame(predict(rfom,trainset))
    trained$prfo<-round(as.numeric(trrfom[,2]),4)
    arfom<-as.data.frame(predict(rfom,alldata))
    alld$prfo<-round(as.numeric(arfom[,2]),4)
  }
  if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){
    psvmm<-as.data.frame(predict(svmm,predgriddf))
    preds$psvm<-round(as.numeric(psvmm[,2]),4)
    tsvmm<-as.data.frame(predict(svmm,testset))
    test$psvm<-round(as.numeric(tsvmm[,2]),4)
    trsvmm<-as.data.frame(predict(svmm,trainset))
    trained$psvm<-round(as.numeric(trsvmm[,2]),4)
    asvmm<-as.data.frame(predict(svmm,alldata))
    alld$psvm<-round(as.numeric(asvmm[,2]),4)
  }
  if(!inherits(boom,"try-error") && class(boom@object)!="character"){
    pboom<-as.data.frame(predict(boom,predgriddf))
    preds$pboo<-round(as.numeric(pboom[,2]),4)
    tboom<-as.data.frame(predict(boom,testset))
    test$pboo<-round(as.numeric(tboom[,2]),4)
    trboom<-as.data.frame(predict(boom,trainset))
    trained$pboo<-round(as.numeric(trboom[,2]),4)
    aboom<-as.data.frame(predict(boom,alldata))
    alld$pboo<-round(as.numeric(aboom[,2]),4)
  }
  #if(!inherits(xgbm,"try-error") && class(xgbm@object)!="character"){
  #	newdat<-predgriddf;newdat$PresAbs<-0 #rmner uses the model.matrix and thus requires the column - the value we use is irrelevant; can be 9 for example
  #	pxgbm<-as.data.frame(predict(xgbm,newdat))
  #	preds$vxgbm<-as.numeric(pxgbm[,1])
  #	txgbm<-as.data.frame(predict(xgbm,testset))
  #	test$xgbm<-as.numeric(trfom[,1])
  #}
  if(!inherits(xgbm,"try-error")){
    preds$pxgb<-round(as.numeric(xgbm$predgrid),4)
    test$pxgb<-round(as.numeric(xgbm$predtest),4)
    trained$pxgb<-round(as.numeric(xgbm$predtrain),4)
    alld$pxgb<-round(as.numeric(xgbm$predall),4)
  }
  if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){
    pmlpe<-as.data.frame(predict(mlpe,predgriddf[,]))
    preds$pmlp<-round(as.numeric(pmlpe[,2]),4)
    tmlpe<-as.data.frame(predict(mlpe,testset))
    test$pmlp<-round(as.numeric(tmlpe[,2]),4)
    trmlpe<-as.data.frame(predict(mlpe,trainset))
    trained$pmlp<-round(as.numeric(trmlpe[,2]),4)
    amlpe<-as.data.frame(predict(mlpe,alldata))
    alld$pmlp<-round(as.numeric(amlpe[,2]),4)
  }
  if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){
    # for some reason we need to add a dummy column for PresAbs_f to predgriddf
    predgriddf2 = predgriddf
    predgriddf2$PresAbs_f=as.factor(0)
    pglmn<-as.data.frame(predict(glmn,predgriddf2[,]))
    preds$pglm<-round(as.numeric(pglmn[,2]),4)
    # testset should already have VALID PresAbs_f column
    tglmn<-as.data.frame(predict(glmn,testset))
    test$pglm<-round(as.numeric(tglmn[,2]),4)
    trglmn<-as.data.frame(predict(glmn,trainset))
    trained$pglm<-round(as.numeric(trglmn[,2]),4)
    # all data (spdata) should have VALID PresAbs_f as well...
    colsToKeep = colnames(predgriddf2)
    alldata2 = alldata
    alldata2 = alldata2 %>% dplyr::select(one_of(colsToKeep))
    aglmn<-as.data.frame(predict(glmn,alldata2))
    alld$pglm<-round(as.numeric(aglmn[,2]),3)
  }
  if(!inherits(knn,"try-error") && class(knn@object)!="character"){
    pknn<-as.data.frame(predict(knn,predgriddf))
    preds$pknn<-round(as.numeric(pknn[,2]),4)
    tknn<-as.data.frame(predict(knn,testset))
    test$pknn<-round(as.numeric(tknn[,2]),4)
    trknn<-as.data.frame(predict(knn,trainset))
    trained$pknn<-round(as.numeric(trknn[,2]),4)
    aknn<-as.data.frame(predict(knn,alldata))
    alld$pknn<-round(as.numeric(aknn[,2]),4)
  }
  
  return(list(preds=preds,trained=trained,test=test,alldata=alld))
}

# Determine a binary presence/absence threshold 
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
    #print(vv)
    hval<-ifelse(round(sum(dfp[,vv]>=0.0001)/nrow(dfp),4)>round(sum(dfn[,vv]<0.0001)/nrow(dfn),4),1,
                 ifelse(round(sum(dfp[,vv]>=0.0001)/nrow(dfp),4)<round(sum(dfn[,vv]<0.0001)/nrow(dfn),4),0,2))
    #print(hval)
    if(hval==2){
      thresh<-c(thresh,0.0001)
    }else{
      tsearch = seq(0.0001,0.9999,by=0.0001)
      for(t in tsearch){
        #print(paste0("thresh: ", t))
        SS<-round(sum(dfp[,vv]>=t)/nrow(dfp),4)
        #print(paste0("SS: ",SS))
        SP<-round(sum(dfn[,vv]<t)/nrow(dfn),4)
        #print(paste0("SP: ",SP))
        nhval<-ifelse(SS>SP,1,ifelse(SS<SP,0,2))
        #print(nhval)
        
        if(nhval!=hval){
          thv<-ifelse(nhval==2,t,t-0.0001)
          thresh<-c(thresh,thv)
          break
        } else if (nhval == hval && t == max(tsearch)){
          # some "bad models" don't work with the above logic. just set these thresh to 0.5 manually
          thv=0.5000
          thresh=c(thresh,thv)
        }
      }
    }
  }
  names(thresh)<-vnm
  return(thresh)
}

# Big function to: load data, subsample, fit models, evaluate, predict and map
# Many hard-coded vars which would need to be adjusted for different species/area: species, fams, thinRad, stratRas, LCras, SonomaCtyShp, ALSvegHt_path
fitCaseModel<-function(X, logf, percent.train=0.8){
  
  cat("\n")
  startdttm<-format(Sys.time(), "%Y%m%d_%H%M")
  cat("SDM Start time: ",startdttm,"\n")
  
  ####
  #### 1. Data import and setup 
  ####
  cat("\n")
  cat("Part 1 - Variable definitions and data import \n")
  cat("\n")
  
  res<-"Attempting fit.."
  
  pathToGit<-X[["gitpath"]];svpth<-X[["svpath"]];resolution<-X[["rez"]]
  spcd<-X[["spp"]];gediyr<-X[["yrsp"]];addGEDI<-X[["gedi"]];useBal<-X[["bal"]];useVIF<-X[["vif"]];iterNo<-X[["iter"]];varType<-X[["varT"]]
  
  # Print some info for the SLURM log
  cat(paste0("Species: ",spcd,"\n"))
  cat(paste0("Spatial Res.: ",resolution,"\n"))
  cat(paste0("GEDI used: ",addGEDI,"\n"))
  cat(paste0("GEDI years: ",gediyr,"\n"))
  cat(paste0("Variable types used: ",varType,"\n"))
  cat(paste0("VIF used: ",useVIF,"\n"))
  cat(paste0("Balance used: ",useBal,"\n"))
  cat(paste0("Percent of data used for training: ",percent.train,"\n"))
  cat(paste0("Model Iteration: ",iterNo,"\n"))
  cat("\n")
  
  ## Check that we have the path to save the files
  chkpth<-checkSavePath(svpth=svpth,rez=resolution)  # Check that the folders exist in svpth and for each resolution level
  if(chkpth!=""){
    cat(chkpth,"\n")
  }else{
    cat("Checking the results path for needed folders...OK \n")
  }
  
  #get the base grid for this resolution
  basegrid<-raster(paste0(pathToGit,"sdmTool/data/CoastDistance/",resolution,"/CoastDistance_",tolower(resolution),".tif"))
  basegrid[]<-NA
  cat(paste0("Loaded base grid for resolution: ",resolution,"\n"))
  
  # Load the deflated bird file and filter for the loop species
  if (useVIF == "VIF"){
    dtpth<-paste0(pathToGit,"sdmTool/data/Birds/",resolution)
  } else if (useVIF == "NoVIF"){
    dtpth<-paste0(pathToGit,"sdmTool/data/Birds_NoVIF/",resolution)  
  } else {
    cat("Error interpretting VIF input \n")
  }
  
  # Choose whether or not to load all variable types, all but GEDI, or just one group (ex: justBCM)
  # This is specified in the batch run file
  if (varType == "All_wGEDI"){
    load(file=paste0(dtpth,"/deflated_",resolution,".RData"))	
    # loaded DF is already named deflatedcovardf 
  } else if (varType == "All_wGEDI_20m"){
    load(file=paste0(dtpth,"/deflated_",resolution,"_20m.RData"))
    # loaded DF is already named deflatedcovardf 
  } else if (varType == "All_woutGEDI"){
    load(file=paste0(dtpth,"/deflatedNoGEDI_",resolution,".RData"))	
    deflatedcovardf = deflatedNoGEDIdf
  } else if (varType == "justBCM"){
    load(file=paste0(dtpth,"/deflatedBCM_",resolution,".RData"))
    deflatedcovardf = deflatedBCMdf
  } else if (varType == "justGEDI"){
    load(file=paste0(dtpth,"/deflatedGEDI2yr_",resolution,".RData"))
    deflatedcovardf = deflatedGEDI2yrdf # could switch to 1 yr or 3 yr here
  } else if (varType == "justNDVI"){
    load(file=paste0(dtpth,"/deflatedNDVI_",resolution,".RData"))
    deflatedcovardf = deflatedNDVIdf
  } else {
    cat("Error interpreting varType argument... \n")
  }
  
  cat("Loaded and preparing the corresponding bird data... \n")
  
  #select only the desired species from the data
  species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
             "CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
             "SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
             "WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
  
  omitspecies<-subset(species,species!=spcd)
  omitnumdet<-paste0("NumDet",omitspecies)
  
  #get covars and the current species' data
  spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet,paste0("NumDet",spcd)))]
  spdata<-as.data.frame(na.omit(spdata))
  
  #create the prediction grid
  predgriddf<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet) & !names(deflatedcovardf) %in% c("x","y") & 
                                       !names(deflatedcovardf) %in% c(spcd,paste0("NumDet",spcd)))]
  predgriddf<-na.omit(predgriddf)
  
  # Get XY coords and raster cell ID
  xydf<-deflatedcovardf[,c("x","y",paste0("gId",resolution))]
  names(xydf)<-c("x","y","cellId")
  
  # Get the species data for the right gediyr, and to include/exclude gedi
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
  
  names(spdata)<-gsub(spcd,"PresAbs",names(spdata))
  # Convert pres/abs column to factor
  spdata$PresAbs_f<-as.factor(as.character(spdata$PresAbs))
  
  # Write all spdata
  outPath_spdata = paste0(svpath, "input/spdata/")
  if (!dir.exists(outPath_spdata)){
    dir.create(outPath_spdata)
  }
  outName_spdata = paste0(spcd, "_", resolution, "_", gediyr, addgn, "_", useVIF, "_spdata.csv")
  if (!file.exists(paste0(outPath_spdata, outName_spdata))){
    write.csv(x = spdata, file = paste0(outPath_spdata, outName_spdata), row.names = FALSE)
  }
  
  # Save the prediction grid
  outPath_predg = paste0(svpath, "input/predgrids/")
  if (!dir.exists(outPath_predg)){
    dir.create(outPath_predg)
  }
  outName_predg = paste0(spcd, "_", resolution, "_", gediyr, addgn, "_", useVIF, "_predGrid.csv")
  if (!file.exists(paste0(outPath_predg, outName_predg))){
    write.csv(x = predgriddf, file = paste0(outPath_predg, outName_predg), row.names = FALSE)
  }
  
  # Write raster XY file
  outPath_rasg = paste0(svpath, "input/rastergrids/")
  if (!dir.exists(outPath_rasg)){
    dir.create(outPath_rasg)
  }
  outName_rasg = paste0(spcd, "_", resolution, "_", gediyr, addgn, "_", useVIF, "_rasterGrid.csv")
  if (!file.exists(paste0(outPath_rasg, outName_rasg))){
    write.csv(x = xydf, file = paste0(outPath_rasg, outName_rasg), row.names = FALSE)
  }
  
  
  ####
  #### 2. Build train and test sets
  ####
  cat("\n")
  cat("Part 2 - Making training and test sets \n")
  cat("\n")
  
  ##
  ## Spatially thin points. Try thinning to exclude neighboring pixels (includes corner cells)
  ## Need to manually adjust thinning radius (thinRad) below
  ##
  cat("Spatially thinning points \n")
  cat("\n")
  
  xycols = c("x", "y")
  xydf_cp = xydf
  xypts = SpatialPointsDataFrame(data = xydf_cp, coords = xydf_cp[,xycols])
  crs(xypts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  spdata_pts = SpatialPointsDataFrame(data = spdata, coords = spdata[,xycols])
  crs(spdata_pts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Need to transform coordinates to geographic WGS84 for spThin to work
  spdata_pts_geo = sp::spTransform(x = spdata_pts, CRSobj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdata_pts_geo@data$xNew = spdata_pts_geo@coords[,1]
  spdata_pts_geo@data$yNew = spdata_pts_geo@coords[,2]
  spdata_pts_geo@data$spec = spcd
  
  # # Option 1 - Thin presences and absences separately. Then thin the combined result
  # thinRad = round(((as.numeric(strsplit(x = resolution, split = "M")[[1]]) * sqrt(2)) + 100)/1000,2) # thin points within 1 pixel (in km)
  # P_th=spThin::thin(loc.data = spdata_pts_geo@data[spdata_pts_geo@data$PresAbs==1,], lat.col = "yNew", long.col = "xNew", thin.par = thinRad, 
  #                   reps = 5, verbose = TRUE, spec.col = "spec", write.files = FALSE, write.log.file = FALSE, locs.thinned.list.return = TRUE)
  # P_thCor = P_th[[5]]
  # P_thCor$PresAbs = 1
  # A_th=spThin::thin(loc.data = spdata_pts_geo@data[spdata_pts_geo@data$PresAbs==0,], lat.col = "yNew", long.col = "xNew", thin.par = thinRad, 
  #                   reps = 5, verbose = TRUE, spec.col = "spec", write.files = FALSE, write.log.file = FALSE, locs.thinned.list.return = TRUE)
  # A_thCor = A_th[[5]]
  # A_thCor$PresAbs = 0
  # 
  # # Combine thinned P and A
  # PA_thCor = rbind(P_thCor, A_thCor)
  # PA_thCor$xNew = PA_thCor$Longitude
  # PA_thCor$yNew = PA_thCor$Latitude
  # PA_thData = dplyr::left_join(PA_thCor, spdata_pts_geo@data)
  # PA_thData_pts = SpatialPointsDataFrame(data = PA_thData, coords = PA_thData[,xycols])
  # crs(PA_thData_pts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # 
  # # Thin so that P and A are not within specified dist
  # PA_th_2x=spThin::thin(loc.data = PA_thData_pts@data, lat.col = "yNew", long.col = "xNew", thin.par = thinRad, 
  #                       reps = 5, verbose = TRUE, spec.col = "spec", write.files = FALSE, write.log.file = FALSE, locs.thinned.list.return = TRUE)
  # PA_th_2x_Cor = PA_th_2x[[5]]
  # PA_th_2x_Cor$xNew = PA_th_2x_Cor$Longitude
  # PA_th_2x_Cor$yNew = PA_th_2x_Cor$Latitude
  # PA_th_2x_Data = dplyr::left_join(PA_th_2x_Cor, spdata_pts_geo@data)
  # PA_th_2x_Data_pts = SpatialPointsDataFrame(data = PA_th_2x_Data, coords = PA_th_2x_Data[,xycols])
  # crs(PA_th_2x_Data_pts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # 
  # # Make the result from above looks like orig spdata
  # spdata_Names = names(spdata)
  # spdata = PA_th_2x_Data %>% dplyr::select(spdata_Names)
  
  
  # Option 2 - Thin presences and absences in areas with higher density of samples using
  #            either a land cover classification or sampling density map
  
  # Load stratification raster. This is a reclassified sampling density raster. 
  #Values==1 correspond to baseline sample density. Values==0 correspond to high sampling density and should be thinned
  #File path is hard-coded for now
  stratRas = raster(paste0(pathToGit,"sdmTool/data/Ancillary/allSpec_merged_diss_dens250m_matchDEM_rec.tif"))
  
  # Specify thinning radius for class 0 of stratification raster
  thinRad = 1.5 # value is kilometers
  
  # Extract values from stratification raster
  xydf_cp$strat = raster::extract(x = stratRas, y = xypts)
  spdata_strat = dplyr::left_join(x = spdata, y = xydf_cp, by = c("x", "y"))
  spdata_strat_pts = SpatialPointsDataFrame(data = spdata_strat, coords = spdata_strat[,xycols])
  crs(spdata_strat_pts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Transform to WGS84 for thinning (required for spThin)
  spdata_strat_pts_geo = sp::spTransform(x = spdata_strat_pts, CRSobj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdata_strat_pts_geo@data$xNew = spdata_strat_pts_geo@coords[,1]
  spdata_strat_pts_geo@data$yNew = spdata_strat_pts_geo@coords[,2]
  spdata_strat_pts_geo@data$spec = spcd
  
  # Points with a stratification value of 1 don't need to be thinned. These are low density sampling areas
  no_th = spdata_strat_pts_geo@data[spdata_strat_pts_geo@data$strat == 1,]
  
  # Points with a stratification value of 0 DO need to be thinned (high density sampling areas)
  strat_th=spThin::thin(loc.data = spdata_strat_pts_geo@data[spdata_strat_pts_geo@data$strat==0,], lat.col = "yNew", long.col = "xNew", thin.par = thinRad, 
                        reps = 5, verbose = TRUE, spec.col = "spec", write.files = FALSE, write.log.file = FALSE, locs.thinned.list.return = TRUE)
  
  strat_thCor = strat_th[[5]]
  strat_thCor$strat = 0
  strat_thCor$xNew = strat_thCor$Longitude
  strat_thCor$yNew = strat_thCor$Latitude
  strat_thData = dplyr::left_join(strat_thCor, spdata_strat_pts_geo@data)
  
  # Join back thinned points with unthinned data
  strat_thData = strat_thData %>% dplyr::select(one_of(colnames(no_th)))
  comb_thData = rbind(no_th, strat_thData)
  
  # Convert to sp for plotting later on
  comb_thData_pts = SpatialPointsDataFrame(data = comb_thData, coords = comb_thData[,xycols])
  crs(comb_thData_pts) = sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Make the result from above looks like original spdata
  spdata_Names = names(spdata)
  spdata = comb_thData %>% dplyr::select(spdata_Names)
  cat("\n")
  
  ##
  ## Balance Presence and Absence data
  ##
  cat("Balancing Presence and Absence data for training and testing \n")
  
  if (useBal == "Bal"){
    # Balance - Make sure the same number of P and A are used for training. Everything else held out for testing
    cat("Using equally balanced presence and absence data \n")
    
    allPres = spdata[spdata$PresAbs==1,]
    allAbs = spdata[spdata$PresAbs==0,]
    
    if (nrow(allPres) < nrow(allAbs)){
      subAbs = allAbs %>% dplyr::sample_n(nrow(allPres), replace = FALSE)
    } else if (nrow(allPres) >= nrow(allAbs)){
      subAbs = allAbs
    } else {
      cat("Error with equal balancing \n")
    }
    
    P_subA = rbind(allPres, subAbs)
    
    H<-holdout(P_subA$PresAbs_f,ratio=percent.train)
    trainset<-P_subA[H$tr,];testset<-P_subA[H$ts,]
    
    numPres_Tr = nrow(trainset[trainset$PresAbs==1,])
    numAbs_Tr = nrow(trainset[trainset$PresAbs==0,])
    
  } else if (useBal == "varA"){
    # Always use the same number of presences, but vary the number of absences used for training
    cat("Using same number of presences but variable number of absences \n")
    
    numPres_Tot = sum(spdata[,"PresAbs"]==1)
    numPres_Tr = floor(numPres_Tot * percent.train)
    Pres_Tr = spdata %>% dplyr::filter(PresAbs == 1) %>%
      dplyr::sample_n(numPres_Tr, replace = FALSE)
    
    numAbs_Tot = sum(spdata[,"PresAbs"]==0)
    numAbs_Tr_max = floor(numAbs_Tot * percent.train)
    numAbs_Tr_rand = sample(numPres_Tr:numAbs_Tr_max,1, replace = F)
    Abs_Tr = spdata %>% dplyr::filter(PresAbs == 0) %>%
      dplyr::sample_n(numAbs_Tr_rand, replace = FALSE)
    numAbs_Tr = numAbs_Tr_rand
    
    trainset = rbind(Pres_Tr, Abs_Tr)
    testset = dplyr::anti_join(spdata, trainset)
    
  } else if (useBal == "stratPA"){
    # use a variable ratio of presence and absence for training and testing, based on a max A:P ratio of 1.5
    cat("Stratifying Presence and Absence based on hard-coded, fixed ratio \n")
    
    allPres = spdata[spdata$PresAbs==1,]
    allAbs = spdata[spdata$PresAbs==0,]
    
    # Split up all presences
    H_Pres = holdout(allPres$PresAbs_f, ratio = percent.train, 
                     mode = "random")
    Pres_Tr = allPres[H_Pres$tr,]
    Pres_Ts = allPres[H_Pres$ts,]
    
    # Sample absences (1.5 * P for 40/60 P/A). Should be at least this many more than P
    if ((nrow(allPres)*1.5) < nrow(allAbs)){
      numAbs_samp_max = floor(nrow(allPres) * 1.5)
      numAbs_samp = sample(nrow(allPres):numAbs_samp_max,1)
    } else if ((nrow(allPres)*1.5) >= nrow(allAbs)){
      numAbs_samp = nrow(allAbs)
    } else {
      cat("Error with stratPA balancing \n")
    }
    
    Abs_samp = allAbs %>% dplyr::sample_n(numAbs_samp, replace = F)
    H_Abs = holdout(Abs_samp$PresAbs_f, ratio = percent.train,
                    mode = "random")
    Abs_Tr = Abs_samp[H_Abs$tr,]
    Abs_Ts = Abs_samp[H_Abs$ts,]
    
    # Combine PA subsets for training and testing 
    trainset = rbind(Pres_Tr, Abs_Tr)
    testset = rbind(Pres_Ts, Abs_Ts)
    
    numPres_Tr = nrow(trainset[trainset$PresAbs==1,])
    numAbs_Tr = nrow(trainset[trainset$PresAbs==0,])
    
  } else if (useBal == "NoBal") {
    # Use the data as they are
    cat("Using original Presence and Absence data \n")
    
    H<-holdout(spdata$PresAbs_f,ratio=percent.train)
    trainset<-spdata[H$tr,];testset<-spdata[H$ts,]
    
    numPres_Tr = nrow(trainset[trainset$PresAbs==1,])
    numAbs_Tr = nrow(trainset[trainset$PresAbs==0,])
    
  } else {
    cat("Error interpretting Balance input. Double check the input CSV to make sure CRs have been removed \n")
  }
  
  # Summarize Training and Test data sizes
  cat("Training Info: \n")
  cat("Balancing method: Variable Absence \n")
  cat(paste0("Number of Presences: ",numPres_Tr),"\n")
  cat(paste0("Number of Absences: ",numAbs_Tr),"\n")
  cat("\n")
  
  #####  
  
  trainsize<-nrow(trainset)
  naivePrev<-sum(spdata$PresAbs)/nrow(spdata)
  
  # Save xy info of training and test
  trainset_xy = trainset
  testset_xy = testset
  
  # Remove coords and cell id before building the model
  trainset<-trainset[,which(!names(trainset) %in% c("x","y",paste0("gId",resolution)))]
  testset<-testset[,which(!names(testset) %in% c("x","y",paste0("gId",resolution)))]
  traintest = rbind(trainset, testset)
  
  # Write traintest files for each boot
  outPath_TrTe = paste0(svpath, "input/traintest/")
  if (!dir.exists(outPath_TrTe)){
    dir.create(outPath_TrTe)
  }
  write.csv(x = traintest, file = paste0(outPath_TrTe, spcd,"_",resolution,"_", gediyr, addgn,"_", useVIF,"_", useBal, "_i", iterNo, "_traintest.csv"), row.names = FALSE)
  
  
  ####
  #### 3. Fit Models
  ####
  cat("Part 3 - Fitting models \n")
  cat("\n")
  
  #remove from memory if they exist
  if(TRUE %in% grepl("rfom",ls())){	
    rm(list=c("rfom","svmm","boom","xgbm","mlpe","glmn","knn"));gc()
  }
  
  nc<-ncol(trainset)-2
  # Make model formulas
  fmlf<-paste("PresAbs_f~",paste(names(trainset[1:nc]),collapse="+"),sep="")
  fmln<-paste("PresAbs ~",paste(names(trainset[1:nc]),collapse="+"),sep="")
  cat(paste0("Model formula: ",fmlf),"\n")
  cat("\n")
  
  # Models to run
  # Run with rminer
  cat("Begin model fitting \n")
  cat("Fitting RF \n")
  rfsearch=list(smethod="grid",search=list(mtry=c(2,3,4,5,6,8,10),ntree=c(50,100,200,400,800,1000)),
                convex=0,metric="AUC",method=c("kfold",5)) #mtry is the number of params to vary in each perm; convex=0 means look for global min
  rfom<-try(fit(x = as.formula(fmlf), data = trainset, model = "randomForest", eval_metric = "auc", importance = TRUE,
                search = rfsearch, scale ="none", task = "prob", transform = "none", 
                fdebug=TRUE),silent=TRUE)
  cat("\n")
  
  cat("Fitting SVM \n")
  svmsearch=list(smethod="2L", search=list(C=c(1, 5, 10, 50, 100, 500, 1000), sigma=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)),
                 convex=0, metric="AUC", method=c("kfold",5))
  svmm<-try(fit(x = as.formula(fmlf), data = trainset, model = "ksvm", eval_metric = "auc", 
                search = svmsearch, scale ="none", task = "prob", transform = "none",
                fdebug=TRUE),silent=TRUE)
  cat("\n")
  
  cat("Fitting Boost \n")
  boostsearch=list(smethod="grid", search=list(mfinal=c(50,100,200,400), minsplit=c(1,2,4)), 
                   convex=0, metric="AUC") # Very slow with kfolds CV..
  boom<-try(fit(x = as.formula(fmlf), data = trainset, model = "boosting",cp=0.01, eval_metric="auc", 
                search=boostsearch, scale ="none", task = "prob", transform = "none",
                fdebug=TRUE),silent=TRUE)
  cat("\n")
  
  cat("Fitting mlpe \n")
  mlpesearch=list(smethod="grid", search=list(size=c(1,2,5,10,15,20), decay=c(0.9, 0.5, 0.1, 0.01, 0.001, 0.0001), maxit=c(1000)), 
                  convex=0, metric = "AUC", method=c("kfold",5))
  mlpe<-try(fit(x = as.formula(fmlf), data = trainset, model = "mlpe", eval_metric="auc", 
                search=mlpesearch, scale="none", task = "prob", transform = "none",
                fdebug=TRUE), silent=TRUE) #mlpe will scale by default but we already scaled data...
  cat("\n")
  
  cat("Fitting glmnet \n")
  glmnsearch=list(smethod="grid", search=list(alpha=seq(0.05,0.5,0.05)),convex=0,metric="AUC",method=c("kfold",5))
  glmn<-try(fit(x = as.formula(fmlf), data=trainset, model="cv.glmnet", family="binomial", 
                maxit=1000000, nlambda = 100, nfolds=5, type.measure="auc",
                search = glmnsearch, scale = "none", standardize = FALSE,
                task = "prob", transform = "none", fdebug=TRUE), silent=TRUE)
  cat("\n")
  
  cat("Fitting KNN \n")
  knnsearch=list(smethod="grid", search=list(k=seq(1,20,2), distance = seq(1,5,1)), convex=0, metric="AUC", method=c("kfold",5))
  knn<-try(fit(x = as.formula(fmlf), data = trainset, model = "kknn",
               search = knnsearch, scale = "none",
               task = "prob", transform = "none", fdebug=TRUE), silent=TRUE)
  cat("\n")
  
  # Custom fitting code needed (see function defined above) because of issues fitting with rminer
  cat("Fitting XG Boost \n")
  xgbm<-try(fitXGB(trainset=trainset, testset=testset, predgriddf=predgriddf, alldata=spdata),silent=TRUE)
  cat("\n")
  
  # Look for errors
  if((inherits(rfom,"try-error") || class(rfom@object)=="character") && (inherits(svmm,"try-error") || class(svmm@object)=="character") && 
     (inherits(boom,"try-error") || class(boom@object)=="character") && (inherits(xgbm,"try-error") || class(xgbm@object)=="character") && 
     (inherits(mlpe,"try-error") || class(mlpe@object)=="character") && (inherits(glmn,"try-error") || class(glmn@object)=="character") &&
     (inherits(knn,"try-error") || class(knn@object)=="character")){
    cat("None of the models attempted was able to converge and fit. Ending soon \n")
  }else{
    cat("Some or all models were fitted. Moving on \n")
    cat(" \n")
    
    
    ####
    #### 4. Model Variable Importance
    ####
    cat("Part 4 - Calculating variable importance \n")
    cat("\n")
    # Use rminer methods
    # 1D-SA first: fast but no interactions
    importance_RM1<-getVariableImportance(rfom,svmm,boom,xgbm,mlpe,glmn,knn,trainset,meth = "1D-SA")
    importance_RM1<-getVarMetaClass(df=importance_RM1)
    importance_RM1$Species<-spcd
    importance_RM1$Resolution<-resolution
    colnames(importance_RM1)<-c("Variable", "AbsImp_1DSA", "Model", "RelImp_1DSA", "VarType", "Species", "Resolution")
    
    # DSA next: a little slower but recommended and considers interactions
    importance_RM2<-getVariableImportance(rfom,svmm,boom,xgbm,mlpe,glmn,knn,trainset,meth = "DSA")
    importance_RM2<-getVarMetaClass(df=importance_RM2)
    importance_RM2$Species<-spcd
    importance_RM2$Resolution<-resolution
    colnames(importance_RM2)<-c("Variable", "AbsImp_DSA", "Model", "RelImp_DSA", "VarType", "Species", "Resolution")
    
    # Join rminer importances
    importance_RM = dplyr::full_join(x = importance_RM1, y = importance_RM2, by = c("Variable","Model","VarType", "Species", "Resolution"))
    
    # Use randomForest type1 and join with rminer estimaes
    importance_RF1<-as.data.frame(randomForest::importance(rfom@object, type=1))
    importance_RF1$Variable<-rownames(importance_RF1)
    importance_RF1$Model<-"RF"
    rownames(importance_RF1) <- c()
    colnames(importance_RF1)<-c("ImpRF1","Variable","Model")
    importance = dplyr::full_join(x = importance_RM, y = importance_RF1, by = c("Variable","Model"))
    
    nImp = nrow(importance)
    outimp = data.frame(species = rep(spcd, nImp), 
                        res = rep(resolution, nImp),
                        gediYr = rep(gediyr, nImp),
                        wGEDI = rep(addGEDI, nImp),
                        wVIF = rep(useVIF, nImp),
                        wBal = rep(useBal, nImp),
                        iter = rep(iterNo, nImp),
                        model = importance$Model,
                        var = importance$Variable,
                        varType = importance$VarType,
                        Imp_1DSA = importance$AbsImp_1DSA,
                        Imp_DSA = importance$AbsImp_DSA,
                        Imp_RF = importance$ImpRF1)
    
    # Save importance metrics
    cat("Saving variable importance \n")
    write.csv(x = outimp, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_modelResults_Optimized_imp.csv"), row.names = FALSE)
    cat("\n")
    
    
    ####
    #### 5. Model predictions
    ####
    cat("Part 5 - Model predictions \n")
    cat("\n")
    
    ## predicting to test and to grid 
    preds<-data.frame(cellId=as.integer(predgriddf[,paste0("gId",resolution)]))
    predgriddf<-predgriddf[,names(predgriddf)[which(names(predgriddf) %in% names(trainset))]]
    
    ## predict to test set and evaluate
    predres<-getPredicted(preds=preds,predgriddf=predgriddf,trainset=trainset,testset=testset,alldata=spdata,rfom=rfom,svmm=svmm,boom=boom,xgbm=xgbm,mlpe=mlpe,glmn=glmn,knn=knn)
    preds<-predres$preds
    trainres<-predres$trained
    testres<-predres$test
    alldata<-predres$alldata
    cat("Finished predictions \n")
    
    # Adjust predicted values exactly equal to 0 and 1 (since log(0) is undefined). 
    pCols = colnames(preds[2:ncol(preds)])
    
    preds[,pCols] = sapply(preds[,pCols],function(x) ifelse(x==0.0000,0.0001,
                                                            ifelse(x==1.0000,0.9999,x)))
    trainres[,pCols] = sapply(trainres[,pCols],function(x) ifelse(x==0.0000,0.0001,
                                                                  ifelse(x==1.0000,0.9999,x)))
    testres[,pCols] = sapply(testres[,pCols],function(x) ifelse(x==0.0000,0.0001,
                                                                ifelse(x==1.0000,0.9999,x)))
    alldata[,pCols] =  sapply(alldata[,pCols],function(x) ifelse(x==0.0000,0.0001,
                                                                 ifelse(x==1.0000,0.9999,x)))
    
    ## convert all predicted values to logits...
    #preds<-adply(.data=preds[,2:5],.margins=1,.fun=function(x)log(x)-log(1-x))	#Too slow!
    preds<-data.table(preds)
    if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){preds[,lgprfo:=log(prfo)-log(1-prfo),]}
    if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){preds[,lgpsvm:=log(psvm)-log(1-psvm),]}
    if(!inherits(boom,"try-error") && class(boom@object)!="character"){preds[,lgpboo:=log(pboo)-log(1-pboo),]}
    if(!inherits(xgbm,"try-error")){preds[,lgpxgb:=log(pxgb)-log(1-pxgb),]}
    if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){preds[,lgpmlp:=log(pmlp)-log(1-pmlp),]}
    if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){preds[,lgpglm:=log(pglm)-log(1-pglm),]}
    if(!inherits(knn,"try-error") && class(knn@object)!="character"){preds[,lgpknn:=log(pknn)-log(1-pknn),]}
    
    lpCols = colnames(preds[,(2+length(pCols)):ncol(preds)])
    
    predsSave = data.frame(
      cellId = preds$cellId,
      rfo = if("prfo" %in% pCols){preds$prfo} else{NA},
      svm = if("psvm" %in% pCols){preds$psvm} else{NA},
      boo = if("pboo" %in% pCols){preds$pboo} else{NA},
      xgb = if("pxgb" %in% pCols){preds$pxgb} else{NA},
      mlp = if("pmlp" %in% pCols){preds$pmlp} else{NA},
      glm = if("pglm" %in% pCols){preds$pglm} else{NA},
      knn = if("pknn" %in% pCols){preds$pknn} else{NA},
      lrfo = if("lgprfo" %in% lpCols){preds$lgprfo} else{NA},
      lsvm = if("lgpsvm" %in% lpCols){preds$lgpsvm} else{NA},
      lboo = if("lgpboo" %in% lpCols){preds$lgpboo} else{NA},
      lxgb = if("lgpxgb" %in% lpCols){preds$lgpxgb} else{NA},
      lmlp = if("lgpmlp" %in% lpCols){preds$lgpmlp} else{NA},
      lglm = if("lgpglm" %in% lpCols){preds$lgpglm} else{NA},
      lknn = if("lgpknn" %in% lpCols){preds$lgpknn} else{NA}
    )
    pColsAll = c("prfo", "psvm", "pboo", "pxgb", "pmlp", "pglm", "pknn")
    lColsAll = paste0("lg", pColsAll)
    # Set output column names to include species, iteration number, and model prediction
    colnames(predsSave) = c("cellId",paste0(spcd,gediyr,"_",pColsAll,"_i",iterNo),paste0(spcd,gediyr,"_",lColsAll,"_i",iterNo))
    
    # Save the untransformed predictions so we can access later if necessary. Good to do know in case the script fails later on (may happen if no models meet ensemble criteria)
    cat("Saving prediction values for each model \n")
    write.csv(x = predsSave, file = paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_probPresRawTab.csv"),row.names = FALSE)
    # Also save as RData
    save(predsSave, file = paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_probPresRawTab.RData"))
    cat("\n")
    
    
    ####
    #### 6. Goodness of fit evaluation
    ####
    cat("Part 6 - Goodness of Fit (GOF) \n")
    cat("\n")
    
    # get threshold for binary P/A based on sensitivity == specificity 
    # use combined train+test sets
    trte_res = rbind(trainres,testres)
    thresh<-findThreshold(df=trte_res)
    
    ## Evaluate GOF
    gofs<-getGOF(testres=testres,thresh=thresh)
    cat("Finished getting GOF \n")
    
    # Get the number of features used. May be less than number of predictor variables for GLMNet and XGBoost
    if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){
      fGLMN=as.data.frame(as.matrix(coef(glmn@object, glmn@object$lambda.1se)))
      colnames(fGLMN)=c("coef")
      fGLMN$val = fGLMN$coef != 0
      TFGLMN = as.vector(fGLMN$val)
      nfGLMN = length(TFGLMN[TFGLMN=="TRUE"])-1
    } else {
      cat("No GLMN model \n")
    }
    
    if(!inherits(xgbm,"try-error")){
      nfXGB = length(xgbm$varimp$Feature)
    } else {
      cat("No XGB model \n")
    }
    
    nfOth = ncol(trainset)-2
    gofs$nFeat = ifelse(gofs$Model=="XGB", nfXGB, 
                        ifelse(gofs$Model=="GLMN", nfGLMN, nfOth))
    
    # Write GOF
    nMod = nrow(as.data.frame(gofs))
    outgof = cbind(data.frame(species = rep(spcd, nMod), 
                              res = rep(resolution, nMod),
                              gediYr = rep(gediyr, nMod),
                              wGEDI = rep(addGEDI, nMod),
                              wVIF = rep(useVIF, nMod),
                              wBal = rep(useBal, nMod),
                              iter = rep(iterNo, nMod),
                              nP_tr = rep(numPres_Tr, nMod),
                              nA_tr = rep(numAbs_Tr, nMod))
                   , gofs)
    cat("Saving GOF \n")
    write.csv(x = outgof, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_modelResults_Optimized_gof.csv"), row.names = FALSE)
    cat("\n")
    
    
    ####
    #### 7. Model ensemble
    ####
    cat("Part 7 - Model ensemble for this iteration \n")
    cat("\n")
    
    ## convert alldata (AKA spdata) predicted values to logits...
    alldataLT<-data.table(alldata)
    if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){alldataLT[,lgprfo:=log(prfo)-log(1-prfo),]}
    if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){alldataLT[,lgpsvm:=log(psvm)-log(1-psvm),]}
    if(!inherits(boom,"try-error") && class(boom@object)!="character"){alldataLT[,lgpboo:=log(pboo)-log(1-pboo),]}
    if(!inherits(xgbm,"try-error")){alldataLT[,lgpxgb:=log(pxgb)-log(1-pxgb),]}
    if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){alldataLT[,lgpmlp:=log(pmlp)-log(1-pmlp),]}
    if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){alldataLT[,lgpglm:=log(pglm)-log(1-pglm),]}
    if(!inherits(knn,"try-error") && class(knn@object)!="character"){alldataLT[,lgpknn:=log(pknn)-log(1-pknn),]}
    
    ## convert train+test (AKA spdata) predicted values to logits...
    trte_res_LT<-data.table(trte_res)
    if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){trte_res_LT[,lgprfo:=log(prfo)-log(1-prfo),]}
    if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){trte_res_LT[,lgpsvm:=log(psvm)-log(1-psvm),]}
    if(!inherits(boom,"try-error") && class(boom@object)!="character"){trte_res_LT[,lgpboo:=log(pboo)-log(1-pboo),]}
    if(!inherits(xgbm,"try-error")){trte_res_LT[,lgpxgb:=log(pxgb)-log(1-pxgb),]}
    if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){trte_res_LT[,lgpmlp:=log(pmlp)-log(1-pmlp),]}
    if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){trte_res_LT[,lgpglm:=log(pglm)-log(1-pglm),]}
    if(!inherits(knn,"try-error") && class(knn@object)!="character"){trte_res_LT[,lgpknn:=log(pknn)-log(1-pknn),]}
    
    
    # Keep only "good" models before weighted average
    # "Good" model criteria: (1) AUC > 0.5, (2) truePos > 0 & trueNeg > 0, (3) number of features > 1
    gofs_filt = gofs %>% dplyr::filter(AUC_RM > 0.5 & truePos > 0 & trueNeg > 0 & nFeat > 1)
    gofs_filt$predName<-ifelse(gofs_filt$Model=="RF","prfo",
                               ifelse(gofs_filt$Model=="SVM","psvm",
                                      ifelse(gofs_filt$Model=="Boo", "pboo",
                                             ifelse(gofs_filt$Model=="XGB","pxgb", 
                                                    ifelse(gofs_filt$Model=="MLPE", "pmlp",
                                                           ifelse(gofs_filt$Model=="GLMN", "pglm", "pknn"))))))
    
    # Check to see whether or not any models meet criteria. Should use at least two mods for an ensemble
    if(nrow(gofs_filt)>=2){
      predsKeep = c(as.vector(gofs_filt$predName), paste0("lg",as.vector(gofs_filt$predName)))
      preds_filt = preds %>% dplyr::select(c("cellId", predsKeep))
      alldataLT_filt = alldataLT %>% dplyr::select(c("observed", predsKeep))
      trte_res_LT_filt = trte_res_LT %>% dplyr::select(c("observed", predsKeep))
      
      #Pick which metric to use for support (how to weight the average prediction value)
      # AUC
      sAUC<-sum(gofs_filt$AUC_RM,na.rm=TRUE)
      supp<-gofs_filt$AUC_RM/sAUC
      gofs_filt$Support<-supp
      # AUC adjusted (AUC minus 0.5) bc the model is no better than random when AUC = 0.5 or less
      gofs_filt$AUC_RM_adj = gofs_filt$AUC_RM - 0.5
      gofs_filt$AUC_RM_adj = ifelse(gofs_filt$AUC_RM_adj>0, gofs_filt$AUC_RM_adj, 0) 
      sAUCadj<-sum(gofs_filt$AUC_RM_adj,na.rm=TRUE)
      suppadj<-gofs_filt$AUC_RM_adj/sAUCadj
      gofs_filt$Supportadj<-suppadj
      
      # Write a table with model support values
      suppadjDF=data.frame(t(gofs_filt[,c("AUC_RM_adj")]))
      colnames(suppadjDF) = as.vector(gofs_filt$predName)
      rownames(suppadjDF)=NULL
      # Make a data frame. If the model did not meet performance criteria then adjusted support should be 0
      suppadjSave = data.frame(
        rfo = if("prfo" %in% as.vector(gofs_filt$predName)){suppadjDF$prfo} else{0},
        svm = if("psvm" %in% as.vector(gofs_filt$predName)){suppadjDF$psvm} else{0},
        boo = if("pboo" %in% as.vector(gofs_filt$predName)){suppadjDF$pboo} else{0},
        xgb = if("pxgb" %in% as.vector(gofs_filt$predName)){suppadjDF$pxgb} else{0},
        mlp = if("pmlp" %in% as.vector(gofs_filt$predName)){suppadjDF$pmlp} else{0},
        glm = if("pglm" %in% as.vector(gofs_filt$predName)){suppadjDF$pglm} else{0},
        knn = if("pknn" %in% as.vector(gofs_filt$predName)){suppadjDF$pknn} else{0}
      )
      
      # Set output column names to include species, iteration number, and model prediction
      colnames(suppadjSave) = c(paste0(spcd,gediyr,"_",as.vector(c("prfo", "psvm", "pboo", "pxgb", "pmlp", "pglm", "pknn")),"_i",iterNo))
      
      cat("Saving adjusted support \n")
      write.csv(x = suppadjSave, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_AUCsuppadj.csv"), row.names = FALSE)
      # Also save as RData
      save(suppadjSave, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_AUCsuppadj.RData"))
      cat("\n")
      
      # weighted average of log vals. Need some logic based on which models were fit and which had AUC > 0.5. 3=3; 5=4:5; 7=5:7 9=6:9
      ncp<-ncol(preds_filt)
      if (ncp < 5){
        cat("Fewer than 2 models meet performance criteria for ensemble. \n")
      } else {
        cat("At least 2 models with AUC > 0.5. Attempting weighted average for all grid cells using models that meet requirements \n")
        cat("\n")
        if(ncp==5)
        {ncprg<-4:5}
        else if(ncp==7)
        {ncprg<-5:7}
        else if(ncp==9)
        {ncprg<-6:9}
        else if(ncp==11)
        {ncprg<-7:11}
        else if(ncp==13)
        {ncprg<-8:13}
        else{ncprg<-9:15}
        preds_filt[,lgweighted:=apply(X=preds_filt,MARGIN=1,FUN=function(x,supp,ncprg){as.numeric(x[ncprg])%*%supp},supp=suppadj,ncprg=ncprg),]
        alldataLT_filt[,lgweighted:=apply(X=alldataLT_filt,MARGIN=1,FUN=function(x,supp,ncprg){as.numeric(x[ncprg])%*%supp},supp=suppadj,ncprg=ncprg),]
        trte_res_LT_filt[,lgweighted:=apply(X=trte_res_LT_filt,MARGIN=1,FUN=function(x,supp,ncprg){as.numeric(x[ncprg])%*%supp},supp=suppadj,ncprg=ncprg),]
        
        ## convert weighted back to probabilities
        preds_filt[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
        alldataLT_filt[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
        trte_res_LT_filt[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
        
      }
      
      # Save the results so we can access later if necessary
      #cat("Saving filtered prediction values for each model \n")
      #write.csv(x = preds_filt, file = paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_probPresFiltTab.csv"),row.names = FALSE)
      #cat("\n")
      
      # Find the weighted average threshold
      allD_WA = alldataLT_filt %>% dplyr::select(c("observed", "weighted"))
      trte_WA = trte_res_LT_filt %>% dplyr::select(c("observed", "weighted"))
      thresh_allD_WA = findThreshold(df = as.data.frame(allD_WA))
      thresh_trte_WA = findThreshold(df = as.data.frame(trte_WA))
      
      # Evaluate the accuracy/performance of the weighted average ensemble
      wTest<-data.table(testres)
      if(!inherits(rfom,"try-error") && class(rfom@object)!="character"){wTest[,lgprfo:=log(prfo)-log(1-prfo),]}
      if(!inherits(svmm,"try-error") && class(svmm@object)!="character"){wTest[,lgpsvm:=log(psvm)-log(1-psvm),]}
      if(!inherits(boom,"try-error") && class(boom@object)!="character"){wTest[,lgpboo:=log(pboo)-log(1-pboo),]}
      if(!inherits(xgbm,"try-error")){wTest[,lgpxgb:=log(pxgb)-log(1-pxgb),]}
      if(!inherits(mlpe,"try-error") && class(mlpe@object)!="character"){wTest[,lgpmlp:=log(pmlp)-log(1-pmlp),]}
      if(!inherits(glmn,"try-error") && class(glmn@object)!="character"){wTest[,lgpglm:=log(pglm)-log(1-pglm),]}
      if(!inherits(knn,"try-error") && class(knn@object)!="character"){wTest[,lgpknn:=log(pknn)-log(1-pknn),]}
      
      testKeep = c("observed", as.vector(gofs_filt$predName), paste0("lg",as.vector(gofs_filt$predName)))
      wTest = wTest %>% dplyr::select(testKeep)
      
      # weighted average of log vals. Need some logic based on which models were fit and which had AUC > 0.5. 3=3; 5=4:5; 7=5:7 9=6:9
      nct<-ncol(wTest)
      if (nct < 5){
        cat("Fewer than 2 models meet performance criteria for ensemble. \n")
      } else {
        cat("At least 2 models with AUC > 0.5. Attempting weighted average for test cells \n")
        cat("\n")
        if(nct==5)
        {ncprg<-4:5}
        else if(nct==7)
        {ncprg<-5:7}
        else if(nct==9)
        {ncprg<-6:9}
        else if(ncp==11)
        {ncprg<-7:11}
        else if(ncp==13)
        {ncprg<-8:13}
        else{ncprg<-9:15}
        
        wTest[,lgweighted:=apply(X=wTest,MARGIN=1,FUN=function(x,supp,ncprg){as.numeric(x[ncprg])%*%supp},supp=suppadj,ncprg=ncprg),]
        
        ## convert weighted back to probabilities
        wTest[,weighted:=exp(lgweighted)/(1+exp(lgweighted)),]
        
        # Get GOF metrics of the weighted average ensemble
        Test_WA = wTest %>% dplyr::select(c("observed", "weighted"))
        gofs_WA<-getGOF(testres=as.data.frame(Test_WA),thresh=thresh_trte_WA)
        gofs_WA$nFeat = NA
        
        # Combine weighted average with individual GOFs
        gofs_merged = rbind(gofs, gofs_WA)
        
        nMod = nrow(as.data.frame(gofs_merged))
        outgof = cbind(data.frame(species = rep(spcd, nMod), 
                                  res = rep(resolution, nMod),
                                  gediYr = rep(gediyr, nMod),
                                  wGEDI = rep(addGEDI, nMod),
                                  wVIF = rep(useVIF, nMod),
                                  wBal = rep(useBal, nMod),
                                  iter = rep(iterNo, nMod),
                                  nP_tr = rep(numPres_Tr, nMod),
                                  nA_tr = rep(numAbs_Tr, nMod))
                       , gofs_merged)
        
        cat("Re-Saving GOF to include Ensemble Weighted Average \n")
        write.csv(x = outgof, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_modelResults_Optimized_gof.csv"), row.names = FALSE)
        cat("\n")
      }
    } else {
      cat("Fewer than 2 models meet performance criteria for weighted average ensemble \n")
      # Make a data frame for the case where 0 or 1 models exceed 0.5 AUC
      gofs_filt$AUC_RM_adj = gofs_filt$AUC_RM - 0.5
      gofs_filt$AUC_RM_adj = ifelse(gofs_filt$AUC_RM_adj>0, gofs_filt$AUC_RM_adj, 0) 
      sAUCadj<-sum(gofs_filt$AUC_RM_adj,na.rm=TRUE)
      suppadj<-gofs_filt$AUC_RM_adj/sAUCadj
      gofs_filt$Supportadj<-suppadj
      # Write a table with model support values
      suppadjDF=data.frame(t(gofs_filt[,c("AUC_RM_adj")]))
      colnames(suppadjDF) = as.vector(gofs_filt$predName)
      rownames(suppadjDF)=NULL
      
      suppadjSave = data.frame(
        rfo = if("prfo" %in% as.vector(gofs_filt$predName)){suppadjDF$prfo} else{0},
        svm = if("psvm" %in% as.vector(gofs_filt$predName)){suppadjDF$psvm} else{0},
        boo = if("pboo" %in% as.vector(gofs_filt$predName)){suppadjDF$pboo} else{0},
        xgb = if("pxgb" %in% as.vector(gofs_filt$predName)){suppadjDF$pxgb} else{0},
        mlp = if("pmlp" %in% as.vector(gofs_filt$predName)){suppadjDF$pmlp} else{0},
        glm = if("pglm" %in% as.vector(gofs_filt$predName)){suppadjDF$pglm} else{0},
        knn = if("pknn" %in% as.vector(gofs_filt$predName)){suppadjDF$pknn} else{0}
      )
      
      # Set output column names to include species, iteration number, and model prediction
      colnames(suppadjSave) = c(paste0(spcd,gediyr,"_",as.vector(c("prfo", "psvm", "pboo", "pxgb", "pmlp", "pglm", "pknn")),"_i",iterNo))
      
      cat("Saving adjusted support \n")
      write.csv(x = suppadjSave, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_AUCsuppadj.csv"), row.names = FALSE)
      # Also save as RData
      save(suppadjSave, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_AUCsuppadj.RData"))
      cat("\n")
    }
    
    
    ####
    #### 8. Export maps
    ####
    cat("Part 8 - Saving maps \n")
    cat("\n")
    
    # Specify whether or not to export plota
    savePlots = TRUE
    
    if (savePlots){
      # Optional - save a plot of points used for model training and testing 
      trainset_xy_pts = sp::SpatialPointsDataFrame(data = trainset_xy, coords = trainset_xy[,xycols])
      crs(trainset_xy_pts) = crs(basegrid)
      testset_xy_pts = sp::SpatialPointsDataFrame(data = testset_xy, coords = testset_xy[,xycols])
      crs(testset_xy_pts) = crs(basegrid)
      trainset_xy_pts@data$col[trainset_xy_pts@data$PresAbs == 1] = "blue"
      trainset_xy_pts@data$col[trainset_xy_pts@data$PresAbs == 0] = "red"
      trainset_xy_pts@data$pch[trainset_xy_pts@data$PresAbs == 1] = 0
      trainset_xy_pts@data$pch[trainset_xy_pts@data$PresAbs == 0] = 0
      
      testset_xy_pts@data$col[testset_xy_pts@data$PresAbs == 1] = "blue"
      testset_xy_pts@data$col[testset_xy_pts@data$PresAbs == 0] = "red"
      testset_xy_pts@data$pch[testset_xy_pts@data$PresAbs == 1] = 3
      testset_xy_pts@data$pch[testset_xy_pts@data$PresAbs == 0] = 3
      
      # County shape
      sonomaCtyShp = paste0(pathToGit,"sdmTool/data/Ancillary/Sonoma_county_bounds.shp")
      sonomaSHP = rgdal::readOGR(dsn = sonomaCtyShp)
      sonomaSHP = spTransform(sonomaSHP, CRS = crs(basegrid))
      # Canopy height raster
      ALSvegHt_path = paste0(pathToGit,"sdmTool/data/Ancillary/Sonoma 2013 Vegetation Height_90m.tif")
      ALSvegHt = raster(ALSvegHt_path)
      
      cat("Saving PA map \n")
      png(paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PAmap.png"), width = 2000, height = 2000, units = "px", pointsize = 20)
      plot(sonomaSHP)
      pal = colorRampPalette(c("grey", "yellow","forestgreen", "darkgreen", "white"))
      plot(ALSvegHt , add=TRUE, col = pal(20))
      plot(sonomaSHP, add = TRUE)
      
      plot(trainset_xy_pts, add=TRUE, pch = trainset_xy_pts@data$pch, col = trainset_xy_pts@data$col, cex=1.5, lwd = 2.5)
      plot(testset_xy_pts, add=TRUE, pch = testset_xy_pts@data$pch, col = testset_xy_pts@data$col, cex=1.5, lwd =2.5)
      
      title(main = paste0(spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PAmap"))
      
      dev.off()
      cat("\n")
      
      if(nrow(gofs_filt)>=2){
        ## convert predictions to raster and plot...
        rastres<-basegrid
        preds<-merge(preds_filt,xydf,by="cellId",all.x=T)
        preds$cid<-cellFromXY(basegrid,preds[,c("x","y")])
        cid<-preds$cid;vals<-as.numeric(preds$weighted)
        rastres[cid]<-vals
        writeRaster(rastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PredMap_EWA.tif"),format="GTiff",overwrite=T)
        
        # Plot training and test data on weighted prediction map
        cat("Saving weighted prediction map \n")
        png(paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PredMap_EWA_wPA.png"), width = 2000, height = 2000, units = "px", pointsize = 20)
        plot(sonomaSHP)
        # Plot the predictions
        pal = colorRampPalette(c("black","white"))
        plot(rastres , add=TRUE, zlim=c(0,1), col = pal(30))
        
        plot(sonomaSHP, add = TRUE)
        
        plot(trainset_xy_pts, add=TRUE, pch = trainset_xy_pts@data$pch, col = trainset_xy_pts@data$col, cex=1.5, lwd = 2.5)
        plot(testset_xy_pts, add=TRUE, pch = testset_xy_pts@data$pch, col = testset_xy_pts@data$col, cex=1.5, lwd = 2.5)
        modsUsed = paste(as.vector(gofs_filt$Model), collapse = ", ")
        modWeights = paste(round(as.vector(gofs_filt$Supportadj),2), collapse = ", ")
        mtext(paste("Models used in weighted ensemble:", modsUsed, sep = " "), side = 1, line =0, cex=1.5)
        mtext(paste("                   Model weights:", modWeights, sep = " "), side = 1, line = 1, cex=1.5)
        title(main = paste0(spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PredcitionMap"))
        
        dev.off()
        cat("\n")
        
        ## let's hurdle it by the weighted prevalence...
        preds[,presence:=ifelse(weighted<=thresh_trte_WA,0,1),]
        trastres<-basegrid
        vals<-as.numeric(preds$presence)
        trastres[cid]<-vals
        ## write as geotiff
        cat("Saving hurdled prediction map")
        writeRaster(trastres,filename=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_PredMap_EWA_hurdle.png"),format="GTiff",overwrite=T)
        cat("\n")
      } else {
        cat("Not saving prediction map because fewer than 2 models had AUC > 0.5 \n")
        cat("\n")
      }
    } else {
      cat("savePlots = FALSE. No plots saved \n")
      cat("\n")
    }
    
    # Save training, test, and models as one .RData file. Takes up a lot of space. Commented out to save space
    #save(trainset,testset,predres,testres,preds,gofs,rfom,svmm,boom,xgbm,mlpe,glmn,knn,importance, file=paste0(svpth,resolution,"/",spcd,"_",resolution,"_",gediyr,addgn,"_",useVIF,"_",useBal,"_i",iterNo,"_modelResults_Optimized.RData"))
  }
  
  cat(paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn),"\n")
  res<-paste0("Done with ",spcd," at resolution ",resolution," and gedi year: ",gediyr,addgn)
  
  return(res)
  
}


