# TODO: We discussed adding to your code the ability to keep birds with NA calls, and then allow two analyses: 
# 
# Author: lsalas
###############################################################################

## This file tests all the pre-trained models vs the GV

libs<-c("ggplot2","plyr","RMySQL","data.table")
lapply(libs, require, character.only = TRUE)

pathToLocalGit<-ifelse(dir.exists("c:/users/salasle"),"c:/users/salasle/git/S2L_devel/GVanalyses/","c:/users/lsalas/git/S2L_devel/GVanalyses/")

load(file=paste0(pathToLocalGit,"/3models2outputs/data/chosenSpeciesModel.RData"))
spmodel$speciesmodel<-paste0(spmodel$birdcode,"::",spmodel$Model)

########################################
## Need these functions:
# This function retrieves all the data from all models and outputs for each wav file in the GV table
## Query sintax builder, but only for NEW set of models. If using old database, then PR.PredictionScore becomes PR.SoftMaxScore
makeQuery<-function(event, dbver){
	predscore<-ifelse(dbver=="s2l_test","PR.PredictionScore","PR.SoftMaxScore")
	event<-as.character(event)
	site<-substr(event,1,15); yearv<-as.integer(substr(event,19,20)); monthv<-as.integer(substr(event,22,23)); dayv<-as.integer(substr(event,25,26))
	hourv<-as.integer(substr(event,28,29)); minutev<-as.integer(substr(event,31,32))
	sqltxt<-paste0("SELECT PR.PredictionId, PR.Second, PR.SpeciesCode, PR.PredictionsDatasetId, ",predscore," AS PredictionScore, AF.AudiofileId, ",
			"CONCAT(AF.SiteName,'_20',AF.Year, '-', LPAD(AF.Month, 2, '0'), '-', LPAD(AF.Day, 2, '0'), '_', LPAD(AF.Hour, 2, '0'), '-', LPAD(AF.Minute, 2, '0')) AS SamplingEvent ",
			"FROM predictions AS PR INNER JOIN audiofiles AS AF ON (PR.audiofileid = AF.audiofileid) WHERE ",
			"EXISTS (SELECT 1 FROM audiofiles AS audiofiles1 WHERE (audiofiles1.SiteName = '",site,"') AND ",
			"(audiofiles1.Year = '",yearv,"') AND (audiofiles1.Month = '",monthv,"') AND (audiofiles1.Day = '",dayv,"') AND ",
			"(audiofiles1.Hour = '",hourv,"') AND (audiofiles1.Minute = '",minutev,"') AND (AF.audiofileid = audiofiles1.AudiofileId))")
	return(sqltxt)
}

# This function lists all the unique files in the GV dataset and queries the data for all of them
getGVpredictions<-function(events,speciesmodeled,dbver="s2l_test"){
	
	#dimension output
	tdf<-data.frame(SpeciesCode=rep("XXXX",354),PredictionsDatasetId=rep(9,354),PredictionScore=rep(9999999,354),AudiofileId=rep(123456,354),SamplingEvent=rep("s2lamXXX_100101_0000-00-00_00-00",354))
	reslst<-list();for(rr in events){reslst[[rr]]<-tdf}
	
	# make the connection and query
	con <- dbConnect(RMySQL::MySQL(), user = "lsalas", password = "&pBtt2G.", host = "localhost", port = 3306, dbname = dbver)
	
	for(rr in events){
		sqltxt<-makeQuery(rr, dbver)
		preddf<-dbGetQuery(con,sqltxt)
		if(nrow(preddf)>0){
			reslst[[rr]]<-preddf
		}
	}
	
	## Add information about the model...
	if(dbver=="s2l_test"){
		predinfo<-dbGetQuery(con,"select MD.ModelName, PD.PredictionsDatasetId, PD.PredictionScoreType from models as MD inner join predictions_datasets as PD on (MD.ModelId = PD.ModelId)")
		predinfo$modelVersion<-"Pre-trained 3 models"
	}else{
		predinfo<-dbGetQuery(con,"select CONCAT(MD.ModelName,'_',MD.Epochs,'_',MD.PreTrainData) as ModelName, PD.PredictionsDatasetId from models as MD inner join predictions_datasets as PD on (MD.ModelId = PD.ModelId)")	
		predinfo$PredictionScoreType<-"softmax"
		predinfo$modelVersion<-"No pre-train 6 models"
	}
	
	dbDisconnect(con)
	dbdf<-rbindlist(reslst)
	dbdf<-subset(dbdf,SpeciesCode != "XXXX")
	dbdf<-subset(dbdf,SpeciesCode %in% speciesmodeled)
	dbdf<-merge(dbdf,predinfo,by="PredictionsDatasetId",all.x=TRUE)
	
	return(dbdf)
}

## This function does the matches for all hurdle levels to be used for test and prod data
getModelHurdledMatches<-function(data,gvedf,hurdvals){
	res<-ldply(.data=unique(data$ModelName),.parallel=TRUE,.fun=function(mm,data,gvedf,hurdvals,getMatches,removeDoubleMatches){
				modelMatches<-data.frame()
				for(pp in unique(data$PredictionScoreType)){
					matches<-data.frame()
					datm<-subset(data,ModelName==mm & PredictionScoreType==pp)
					for(ss in unique(datm$SpeciesCode)){
						datms<-subset(datm,SpeciesCode==ss)
						for(hh in hurdvals){
							hv<-hh*(10^7)
							datmsh<-subset(datms,PredictionScore>=hv)
							for(ee in unique(gvedf$SamplingEvent)){  
								rdf<-subset(datmsh,SamplingEvent==ee)
								if(nrow(rdf)>0){rdf<-rdf[order(rdf$Second),]}
								gdf<-subset(gvedf,species==ss & SamplingEvent==ee)
								if(nrow(gdf)>0){gdf<-gdf[order(gdf$secs),]}
								match<-getMatches(rdf=rdf,gdf=gdf,removeDoubleMatches=removeDoubleMatches)
								if(nrow(match)>0){
									match$SamplingEvent<-ee
									match$hurdle<-hh
									matches<-rbind(matches,match)
								}
							}
							
						}
					}
					if(nrow(matches)>0){
						matches$ModelName<-mm
						matches$PredictionScoreType<-pp
						matches$modelVersion<-unique(datm$modelVersion)
						modelMatches<-rbind(modelMatches,matches)
					}
					
				}
				
				return(modelMatches)
				
			},data=data,gvedf=gvedf,hurdvals=hurdvals,getMatches=getMatches,removeDoubleMatches=removeDoubleMatches)
	return(res)
}

## Match against the GV for each output and threshold, generate table of 0/1, save
## Need these functions:
# This matches each file's cnn detections to the GVs and returns a data frame with second, cnnSpecies. gvspecies - this matching is minute-by-minute
# rdf is the cnn predictions
# gdf is the GV detections
getMatches<-function(rdf,gdf,removeDoubleMatches){	#Processing one recording at the time
	
	## ADD call type to the match
	## MAKE SURE all predictions are reported on
	
	#both dfs contain data for the same file, so it must be a match second by second first, then explore +/-1, 2 and 3. Keep the first species found and report that species if no match
	#consider first the special cases of 0 predictions or 0 gv detections
	if(nrow(rdf)==0){	#this should never happen, but...
		if(nrow(gdf)==0){  #No data in either, nothing to match
			madf<-data.frame()
		}else{
			# All records in gdf are false negatives
			madf<-data.frame(PredictionId=rep(NA,nrow(gdf)), Second=gdf$secs, GVsecond=gdf$secs, SpeciesCode=gdf$species, GVspeciesCode=gdf$species, PredictionScore=rep(0,nrow(gdf)), match="FALSENEG", matchDelta=rep(8,nrow(gdf)))
		}
	}else if(nrow(gdf)==0){	#It may be the case that no species' calls were detected in this recording
		# All records in rdg are false positives
		madf<-data.frame(PredictionId=rdf$PredictionId, Second=rdf$Second, GVsecond=rep(-9,nrow(rdf)), SpeciesCode=rdf$SpeciesCode, GVspeciesCode=rep("NA",nrow(rdf)), PredictionScore=rdf$PredictionScore, match="FALSEPOS", matchDelta=rep(9,nrow(rdf)))
	}else{
		# First add all the false negatives for species found in gdf and not in rdf
		spg<-unique(gdf$species); spr<-unique(rdf$SpeciesCode)
		fnsp<-spg[which(!spg %in% spr)]
		if(NROW(fnsp)>0){
			gfn<-subset(gdf,species %in% fnsp)
			mafn<-data.frame(PredictionId=rep(NA,nrow(gfn)), Second=gfn$secs, GVsecond=gfn$secs, SpeciesCode=gfn$species, GVspeciesCode=gfn$species, PredictionScore=rep(0,nrow(gfn)), match="FALSENEG", matchDelta=rep(8,nrow(gfn)))
		}
		
		# Now do the matches by clip
		madf<-ldply(rdf$Second,function(ss,rdf,gdf){
					srdf<-subset(rdf,Second==ss); spcd<-srdf$SpeciesCode
					# Symmetric match for first second, NOT symmetric for 2 or 3 seconds off, so that the expert might have marked the starting second of the bird call, but the machine predicted for a section 1-3 seconds later
					# symmetric for the first second in case gv timing of model and expert are a bit off.
					ssq1u<-c(ss-1); ssq1u<-subset(ssq1u,ssq1u >= 0 & ssq1u <= 60) #breaking into up and down so we can track the matching gv second to avoid double-dipping: two predictions on the same gv detection
					ssq1d<-c(ss+1); ssq1d<-subset(ssq1d,ssq1d >= 0 & ssq1d <= 60)
					ssq2<-c(ss-2); ssq2<-subset(ssq2,ssq2 >= 0 & ssq2 <= 60) #,ss+2 
					ssq3<-c(ss-3); ssq3<-subset(ssq3,ssq3 >= 0 & ssq3 <= 60) #,ss+3
					sgdf<-subset(gdf,secs==ss); flg<-0
					if(nrow(sgdf)>0){	#match on the second
						gspcd<-sgdf$species; tempmatch<-gspcd[1]
						if(spcd %in% gspcd){
							tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=0)
							flg<-1
						}
					}
					if(flg==0 & NROW(ssq1u)>0){	#No match at 0, try 1u
						sgdf<-subset(gdf,secs %in% ssq1u)  ##OR.. should we only use the case where gdf secs < rdf secs??
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-1, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=1)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq1d)>0){	#No match at 0, try 1d
						sgdf<-subset(gdf,secs %in% ssq1d)  ##OR.. should we only use the case where gdf secs < rdf secs??
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss+1, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=1)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq2)>0){	#No match at 1, try 2
						sgdf<-subset(gdf,secs %in% ssq2)
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-2, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=2)
								flg<-1
							}
						}
					}
					if(flg==0 & NROW(ssq3)>0){	#No match at 2, try 3
						sgdf<-subset(gdf,secs %in% ssq3)
						if(nrow(sgdf)>0){
							gspcd<-sgdf$species
							if(spcd %in% gspcd){
								tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=ss, GVsecond=ss-3, SpeciesCode=spcd, GVspeciesCode=gspcd, PredictionScore=srdf$PredictionScore, match="TRUEPOS", matchDelta=3)
								flg<-1
							}
						}
					}
					if(flg==0){	#no match at 3, no match so rdf value is a false positive
						tmdf<-data.frame(PredictionId=srdf$PredictionId, Second=srdf$Second, GVsecond=-9, SpeciesCode=spcd, GVspeciesCode="NA", PredictionScore=srdf$PredictionScore, match="FALSEPOS", matchDelta=9)
					}
					return(tmdf)
				}, rdf=rdf, gdf=gdf)
		if(NROW(fnsp)>0){
			madf<-rbind(madf,mafn)
		}
	}
	#Before returning madf, need to scoop out and delete the double-dips
	if(sum(madf$match=="TRUEPOS")>0){madf<-removeDoubleMatches(madf)}
	
	return(madf)
}

# This function relables matches to ensure two model predictions do not match the same GV detection
# So it applies only to TRUEPOS matches, and it is called by the getMatches function
# madf is madf from the getMatches function
removeDoubleMatches<-function(madf){
	dfp<-subset(madf,match=="TRUEPOS")
	dfp<-dfp[order(dfp$Second),]
	fixrows<-integer()
	for(rr in 1:nrow(dfp)){
		##########################
		## Allowing for max 2 detections per GV detection, so two consecutive predictions can be for the same GV, due to imprecision in the timing of GV detections
		#if(rr+1 <= nrow(dfp)){
		#	if(dfp[rr,"GVsecond"]==dfp[rr+1,"GVsecond"]){
		#		remrows<-c(remrows,rr+2)
		#	}
		#}
		
		if(rr+2 <= nrow(dfp)){
			if(dfp[rr,"GVsecond"]==dfp[rr+2,"GVsecond"]){
				fixrows<-c(fixrows,rr+1)
			}
		}
		
		if(rr+3 <= nrow(dfp)){
			if(dfp[rr,"GVsecond"]==dfp[rr+3,"GVsecond"]){
				fixrows<-c(fixrows,rr+1)
			}
		}
		
	}
	if(NROW(fixrows)>0){
		madf[fixrows,"GVsecond"]<- -9
		madf[fixrows,"match"]<-"FALSEPOS"
		madf[fixrows,"matchDelta"]<- 9
	}
	
	return(madf)
}

## Need a function to summarize matches to file level
# matches is the df of matches: filename, second, cnnSpecies, gvMatch, model, hurdle
# return: cnnSpecies, TP, FP, RegistriName, model, hurdle, metrics
# Metrics: 
# 	F1: 2TP/(2TP + FP + FN)
# 	Sensitivity: TP/(TP+FN)
# 	Precision: TP/(TP+FP)
# 	Miss rate: FN/(FN+TP)
summarizeToFile<-function(matches){
	sumToFile<-ldply(unique(matches$model),function(mm,matches){
				modeldf<-subset(matches,model==mm)
				regdf<-ldply(unique(modeldf$RegistryName),function(rr,modeldf){
							rdf<-subset(modeldf,RegistryName==rr)
							specdf<-ldply(unique(rdf$SpeciesCode),function(ss,rdf){
										sdf<-subset(rdf,SpeciesCode==ss)
										d0val<-sum(sdf$matchDelta==0); d1val<-sum(sdf$matchDelta==1); d2val<-sum(sdf$matchDelta==2); d3val<-sum(sdf$matchDelta==3)
										trval<-sum(d0val,d1val,d2val,d3val)
										fnval<-sum(sdf$matchDelta==8)
										fpval<-sum(sdf$matchDelta==9)
										f1val<-trval/((trval*2)+fnval+fpval)
										sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
										prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
										miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
										fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
										mdf<-data.frame(SpeciesCode=ss,count=nrow(sdf),d0count=d0val, d1count=d1val, d2count=d2val, d3count=d3val,
												truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
										return(mdf)
									},rdf=rdf)
							specdf$RegistryName<-rr
							return(specdf)
						}, modeldf=modeldf)
				regdf$model<-mm
				return(regdf)
			},matches=matches)
	return(sumToFile)
	
}

## Need a function for overall summary
summarizeToSample<-function(matches){
	sumhurdlst<-list()
	for(hh in unique(matches$hurdle)){
		hv<-as.character(hh)
		hmatches<-subset(matches, hurdle==hv)
		sumToSample<-ldply(unique(hmatches$model),function(mm,hmatches){
					modeldf<-subset(hmatches,model==mm)
					specdf<-ldply(unique(modeldf$SpeciesCode),function(ss,modeldf){
								sdf<-subset(modeldf,SpeciesCode==ss)
								d0val<-sum(sdf$matchDelta==0); d1val<-sum(sdf$matchDelta==1); d2val<-sum(sdf$matchDelta==2); d3val<-sum(sdf$matchDelta==3)
								trval<-sum(d0val,d1val,d2val,d3val)
								fnval<-sum(sdf$matchDelta==8)
								fpval<-sum(sdf$matchDelta==9)
								f1val<-trval/((trval*2)+fnval+fpval)
								sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
								prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
								miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
								fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
								mdf<-data.frame(SpeciesCode=ss,count=nrow(sdf),d0count=d0val, d1count=d1val, d2count=d2val, d3count=d3val,
										truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
								return(mdf)
							},modeldf=modeldf)
					specdf$model<-mm
					return(specdf)
				},hmatches=hmatches)
		sumToSample$hurdle<-hv
		sumhurdlst[[hv]]<-sumToSample
	}
	hurdsample<-rbindlist(sumhurdlst)
	return(hurdsample)
}

## Need a function for overall summary all species combined
summarizeToSampleAllSpecies<-function(matches){
	sumhurdlst<-list()
	for(hh in unique(matches$hurdle)){
		hv<-as.character(hh)
		hmatches<-subset(matches, hurdle==hv)
		sumToSample<-ldply(unique(hmatches$Model),function(mm,hmatches){
					mdf<-subset(hmatches,Model==mm)
					d0val<-sum(mdf$matchDelta==0); d1val<-sum(mdf$matchDelta==1); d2val<-sum(mdf$matchDelta==2); d3val<-sum(mdf$matchDelta==3)
					trval<-sum(d0val,d1val,d2val,d3val)
					fnval<-sum(mdf$matchDelta==8)
					fpval<-sum(mdf$matchDelta==9)
					f1val<-trval/((trval*2)+fnval+fpval)
					sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
					prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
					miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
					fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
					modeldf<-data.frame(model=mm,count=nrow(mdf),d0count=d0val, d1count=d1val, d2count=d2val, d3count=d3val,
							truePos=trval,falseNeg=fnval,falsePos=fpval,F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper)
					return(modeldf)
				},hmatches=hmatches)
		sumToSample$hurdle<-hv
		sumhurdlst[[hv]]<-sumToSample
	}
	hurdsample<-rbindlist(sumhurdlst)
	return(hurdsample)
}

## This function subsets the data by the species and model
subsetBySpeciesModel<-function(df,spmodel,spfield="birdcode",modelfield="Model"){
	df$speciesmodel<-paste0(df[,spfield],"::",df[,modelfield])
	dff<-subset(df,speciesmodel %in% spmodel$speciesmodel)
	dff<-dff[,which(names(dff)!="speciesmodel")]
	return(dff)
}


########################################
## We load the GV data from matchGVtoPreds_part1.R
load(paste0(pathToLocalGit,"data/gvData.RData"))

# Remove all species/records with NA calls
# Remove all species' records that are for calls not modeled for the species - do this species by species, use the roi dataset for this purpose
# Remove all species without enough data to evaluate - see below
# Remove: any species from model data that are not in gv AFTER filtering for NA calls or calls not modeled for the species 
gvedf<-subset(gvedf,!is.na(call)); nrow(gvedf)  

gvedf$spcall<-paste0(toupper(gvedf$species),"::",toupper(gvedf$call))
## Now we need the species and calls selected for training:
roidf<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_201109.csv"), stringsAsFactors=FALSE)
roicalls<-unique(roidf[,c("birdcode","songtype")])
roicalls$spcall<-paste0(toupper(roicalls$birdcode),"::",toupper(roicalls$songtype))
gvedf<-subset(gvedf,spcall %in% roicalls$spcall)

## do we have enough GV data to assess the models?
enoughLim<-40
numdf<-aggregate(secs~species,gvedf,NROW); names(numdf)<-c("species","count")
limspdf<-subset(numdf,count >= enoughLim)$species  
## For now not doing this...
#speciesmodeled<-limspdf$species
##instead
speciesmodeled<-numdf$species
NROW(speciesmodeled)
gvedf<-subset(gvedf,species %in% speciesmodeled)
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)
events<-unique(gvedf$SamplingEvent)

########################################
## get the s2l_test data
tst<-getGVpredictions(events=events,speciesmodeled=speciesmodeled,dbver="s2l_test")
save(tst,file=paste0(pathToLocalGit,"/data/modelsDataAll/pretrained_040122.RData"))
#load(file=paste0(pathToLocalGit,"data/modelsDataAll/pretrained.RData"))

## get the s2l data - ignore this as it is the 45-species model
#prod<-getGVpredictions(events=events,speciesmodeled=speciesmodeled,dbver="s2l")
#save(prod,file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/nopretrain.RData")
#load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/nopretrain.RData")

#########################################
##Subset by model&outType, then by species, then, one event at the time...
## ...hurdle, order by second, subset gvedf by registry name too and order by second
#hurdvals<-c(0.85,0.90,0.95,0.98,0.99,0.999,0.9999,0.99999)
hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)

library(doParallel)
nodes<-detectCores()
cl<-makeCluster(nodes)
registerDoParallel(cl)

tm<-Sys.time()
tstmatches<-getModelHurdledMatches(data=tst,gvedf=gvedf,hurdvals=hurdvals)
#prodmatches<-getModelHurdledMatches(data=prod,gvedf=gvedf,hurdvals=hurdvals) NO NEED, it's the old 45 species model
#allmatches<-rbind(tstmatches,prodmatches)
tstmatches<-subset(tstmatches,PredictionScoreType=="sigmoid")
mdf<-data.frame(ModelName=unique(tstmatches$ModelName),Model=c("Resnet50::sigmoid","Resnet101::sigmoid","MobileNet::sigmoid"))
tstmatches<-merge(tstmatches,mdf,by="ModelName",all.x=T)
allmatches<-subsetBySpeciesModel(df=tstmatches,spmodel=spmodel,spfield="SpeciesCode",modelfield="Model")
allmatches$match<-as.character(allmatches$match)
unique(allmatches$match)
#all match NAs are FALSEPOS, matchDelta 9, so...
allmatches$match<-ifelse(is.na(allmatches$match),"FALSEPOS",allmatches$match)
Sys.time()-tm

save(tstmatches,allmatches,file=paste0(pathToLocalGit,"data/modelsDataAll/allmaches_65to99999_040122.RData"))



##########################################################################
## Make table of metrics per species: kappa, total acc, AUC
metrics<-summarizeToSample(matches=allmatches)
			
modelmeta<-ldply(unique(metrics$model),function(mm){
			cuts<-as.numeric(gregexpr("::",mm)[[1]])
			mvc<-min(cuts); otc<-max(cuts)
			mver<-substr(mm,1,mvc-1); mnam<-substr(mm,mvc+2,otc-1); otyp<-substr(mm,otc+2,nchar(mm))
			tdf<-data.frame(model=mm, modelVersion=mver, ModelName=mnam, PredictionScoreType=otyp)
			return(tdf)
		})

metricsdf<-merge(metrics,modelmeta,by="model",all.x=TRUE)
metricsdf$hurdle<-as.numeric(as.character(metricsdf$hurdle))

Modelnames<-ldply(unique(metricsdf$model),function(mm){
			aa<-as.numeric(gregexpr("::",mm)[[1]])
			mnv<-substr(mm,aa[1]+2,aa[2]-1)
			tdf<-data.frame(model=mm,Model=mnv)
		})
Modelnames$model<-as.character(Modelnames$model);Modelnames$Model<-as.character(Modelnames$Model)
Modelnames[7,2]<-paste0(Modelnames[7,2],"_sig");Modelnames[8,2]<-paste0(Modelnames[8,2],"_soft")
Modelnames[9,2]<-paste0(Modelnames[9,2],"_sig");Modelnames[10,2]<-paste0(Modelnames[10,2],"_soft")
Modelnames[11,2]<-paste0(Modelnames[11,2],"_sig");Modelnames[12,2]<-paste0(Modelnames[12,2],"_soft")
metricsdf<-merge(metricsdf,Modelnames,by="model", all.x=T)

modelorder<-data.frame(Model=unique(metricsdf$Model),morder=c(1:6,10,7,11,8,12,9),mtype=c(rep("45spp",6),"sig","soft","sig","soft","sig","soft"))
metricsdf<-merge(metricsdf,modelorder,by="Model", all.x=T)
metricsdf$Model<-as.factor(metricsdf$Model)
metricsdf$Model<-reorder(metricsdf$Model,metricsdf$morder)

## let's bun the miss rate so it's easy to see in the plot

## Plots by aes: species, model (facet), threshold (x), FN (color). Response: Precision (y)
for(ss in unique(metricsdf$SpeciesCode)){
	pdf<-subset(metricsdf, SpeciesCode==ss)
	pdf<-subset(pdf,grepl("cols_",Model))
	pdf$modselval<-pdf$Prec+pdf$Sens
	msv<-subset(pdf,modselval==max(pdf$modselval))
	msvPrec<-msv$Prec[1]; msvSens<-msv$Sens[1]; msvHurd<-msv$hurdle[1]; msvMod<-msv$Model[1]
	ann_text_p1 <- data.frame(hurdle = msvHurd,Prec = msvPrec,lab = "X",
			Model = factor(msvMod,levels = unique(pdf$Model)))
	ann_text_p2 <- data.frame(Sens = msvSens,Prec = msvPrec,lab = "X",
			Model = factor(msvMod,levels = unique(pdf$Model)))
	p1<-ggplot(pdf,aes(x=hurdle, y=Prec)) + facet_wrap(~Model,ncol=3) + 
			scale_color_gradient2(low = "#001c49", mid="white", high = "#f04911") + 
			geom_rect(mapping = aes(fill = mtype), alpha = 0.1,
					xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=Inf, show.legend = FALSE) + geom_line(aes(color=Miss),size=2) +
			scale_fill_brewer() + geom_text(data = ann_text_p1,label = "X",color="black") +
			labs(x="Hurdle", y="Precision",color="Miss rate")
	p2<-ggplot(pdf,aes(x=Sens, y=Prec)) + facet_wrap(~Model,ncol=3) + 
			scale_color_gradient2(low = "#001c49", mid="white", high = "#f04911") + 
			geom_rect(mapping = aes(fill = mtype), alpha = 0.1,
					xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=Inf, show.legend = FALSE) + geom_line(aes(color=hurdle),size=2) +
			scale_fill_brewer() + geom_text(data = ann_text_p2,label = "X",color="black") +
			labs(x="Recall", y="Precision",color="Hurdle")
	print(p1)
	dev.new();print(p2)
	dev.new();print(p2+geom_text(data = ann_text,label = "X",color="black"))
}

###############################################################################
## Make table of global metrics, all species combined
metrics<-summarizeToSampleAllSpecies(matches=allmatches)

modelmeta<-ldply(unique(metrics$model),function(mm){
			mm<-as.character(mm)
			cuts<-as.numeric(gregexpr("::",mm)[[1]])
			mvc<-min(cuts); otc<-max(cuts)
			mver<-substr(mm,1,mvc-1); mnam<-substr(mm,mvc+2,otc-1); otyp<-substr(mm,otc+2,nchar(mm))
			tdf<-data.frame(model=mm, modelVersion=mver, ModelName=mnam, PredictionScoreType=otyp)
			return(tdf)
		})

metricsdf<-merge(metrics,modelmeta,by="model",all.x=TRUE)
metricsdf$hurdle<-as.numeric(as.character(metricsdf$hurdle))

Modelnames<-ldply(unique(metricsdf$model),function(mm){
			aa<-as.numeric(gregexpr("::",mm)[[1]])
			mnv<-substr(mm,aa[1]+2,aa[2]-1)
			tdf<-data.frame(model=mm,Model=mnv)
		})
Modelnames$model<-as.character(Modelnames$model);Modelnames$Model<-as.character(Modelnames$Model)
Modelnames<-Modelnames[order(Modelnames$model),]
Modelnames[7,2]<-paste(Modelnames[7,2],"_sig");Modelnames[8,2]<-paste(Modelnames[8,2],"_soft")
Modelnames[9,2]<-paste(Modelnames[9,2],"_sig");Modelnames[10,2]<-paste(Modelnames[10,2],"_soft")
Modelnames[11,2]<-paste(Modelnames[11,2],"_sig");Modelnames[12,2]<-paste(Modelnames[12,2],"_soft")
metricsdf<-merge(metricsdf,Modelnames,by="model", all.x=T)

modelorder<-data.frame(Model=unique(metricsdf$Model),morder=c(1:6,10,7,11,8,12,9),mtype=c(rep("45spp",6),"sig","soft","sig","soft","sig","soft"))
metricsdf<-merge(metricsdf,modelorder,by="Model", all.x=T)
metricsdf$Model<-as.factor(metricsdf$Model)
metricsdf$Model<-reorder(metricsdf$Model,metricsdf$morder)

## Plot it
p<-ggplot(metricsdf,aes(x=hurdle, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "#001c49", mid="white", high = "#f04911") + 
		geom_rect(mapping = aes(fill = mtype), alpha = 0.1,
				xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=Inf, show.legend = FALSE) + geom_line(aes(color=Miss),size=2) +
		scale_fill_brewer()

bb<-subset(metricsdf,modelVersion=="Pre-trained 3 models" & PredictionScoreType=="sigmoid")
pp<-ggplot(bb,aes(x=hurdle, y=Prec)) + facet_wrap(~Model,ncol=3) + 
		scale_color_gradient2(low = "#001c49", mid="white", high = "#f04911") + 
		geom_line(aes(color=Miss),size=2) + scale_fill_brewer()

load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/chosenSpeciesModel.RData")
spmodel$speciesmodel<-paste0(spmodel$birdcode,"::",spmodel$Model)




pdf<-aggregate(Prec~Model+mtype+hurdle,subset(metricsdf,hurdle %in% c(0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.99,0.999)),mean)
mdf<-aggregate(Miss~Model+mtype+hurdle,subset(metricsdf,hurdle %in% c(0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.99,0.999)),mean)
plotdf<-merge(pdf,mdf,by=c("Model","mtype","hurdle"),all.x=T)
plotdf$hurdle<-as.character(plotdf$hurdle)
bb<-ggplot(plotdf,aes(x=Model,y=Prec)) + geom_bar(aes(fill=Miss),stat="identity") + geom_hline(yintercept=0.75)
		coord_flip() + facet_wrap(~hurdle) + theme_bw() 

### ROC curve plot
h65<-subset(allmatches, hurdle==0.65)
h65<-subset(h65,!is.na(match))
h65<-h65[,c("PredictionScore","SpeciesCode","match","model")]
Modelnames<-ldply(unique(h65$model),function(mm){
			aa<-as.numeric(gregexpr("::",mm)[[1]])
			mnv<-substr(mm,aa[1]+2,aa[2]-1)
			tdf<-data.frame(model=mm,Model=mnv)
		})
Modelnames$model<-as.character(Modelnames$model);Modelnames$Model<-as.character(Modelnames$Model)
Modelnames<-Modelnames[order(Modelnames$model),]
Modelnames[7,2]<-paste(Modelnames[7,2],"_sig");Modelnames[8,2]<-paste(Modelnames[8,2],"_soft")
Modelnames[9,2]<-paste(Modelnames[9,2],"_sig");Modelnames[10,2]<-paste(Modelnames[10,2],"_soft")
Modelnames[11,2]<-paste(Modelnames[11,2],"_sig");Modelnames[12,2]<-paste(Modelnames[12,2],"_soft")
h65<-merge(h65,Modelnames,by="model",all.x=T)
h65<-h65[,-1]
h65df<-ldply(1:nrow(h65),function(rr,h65){
			match<-h65[rr,"match"]
			pred<-h65[rr,"PredictionScore"]/(10^7)
			obs<-ifelse(match %in% c("TRUEPOS","FALSENEG"),1,0)
			if(match=="TRUEPOS"){
				Pred<-"T";Obs<-"T"
			}else if(match=="FALSEPOS"){
				Pred<-"T";Obs<-"F"
			}else{
				 Pred<-"F";Obs<-"T"
			}
			tdf<-data.frame(predicted=pred,observed=obs,Predicted=Pred,Observed=Obs,Model=h65[rr,"Model"])
			return(tdf)
		},h65=h65)
ggplot(h65df, aes(m = predicted, d = observed)) + geom_roc(aes(color=Model), labels=FALSE) + theme_bw()


h999<-subset(allmatches, hurdle==0.999)
h999<-subset(h999,!is.na(match))
h999<-h999[,c("PredictionScore","SpeciesCode","match","model")]
Modelnames<-ldply(unique(h999$model),function(mm){
			aa<-as.numeric(gregexpr("::",mm)[[1]])
			mnv<-substr(mm,aa[1]+2,aa[2]-1)
			tdf<-data.frame(model=mm,Model=mnv)
		})
Modelnames$model<-as.character(Modelnames$model);Modelnames$Model<-as.character(Modelnames$Model)
Modelnames<-Modelnames[order(Modelnames$model),]
Modelnames[7,2]<-paste(Modelnames[7,2],"_sig");Modelnames[8,2]<-paste(Modelnames[8,2],"_soft")
Modelnames[9,2]<-paste(Modelnames[9,2],"_sig");Modelnames[10,2]<-paste(Modelnames[10,2],"_soft")
Modelnames[11,2]<-paste(Modelnames[11,2],"_sig");Modelnames[12,2]<-paste(Modelnames[12,2],"_soft")
h999<-merge(h999,Modelnames,by="model",all.x=T)
h999<-h999[,-1]
h999df<-ldply(1:nrow(h999),function(rr,h999){
			match<-h65[rr,"match"]
			pred<-h65[rr,"PredictionScore"]/(10^7)
			obs<-ifelse(match %in% c("TRUEPOS","FALSENEG"),1,0)
			if(match=="TRUEPOS"){
				Pred<-"T";Obs<-"T"
			}else if(match=="FALSEPOS"){
				Pred<-"T";Obs<-"F"
			}else{
				Pred<-"F";Obs<-"T"
			}
			tdf<-data.frame(predicted=pred,observed=obs,Predicted=Pred,Observed=Obs,Model=h65[rr,"Model"])
			return(tdf)
		},h999=h999)
dev.new()
ggplot(h999df, aes(m = predicted, d = observed)) + geom_roc(aes(color=Model), labels=FALSE) + theme_bw()

#####################################################################################################################################################################################
#####################################################################################################################################################################################
## Matt asked:
## 1) for target birds but without a GV call identified, does the CNN pick up those bird species correctly? 
## We load the GV data from matchGVtoPreds_part1.R but now we work with the data for birds modeled, but not the call modeled
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/pretrained.RData")
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/nopretrain.RData")

load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/gvData.RData")

gvedf<-subset(gvedf,is.na(call)); nrow(gvedf)

gvedf$species<-toupper(gvedf$species)
## Now we need the species selected for training:
roidf<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/data/pattern_matching_ROIs_201109.csv", stringsAsFactors=FALSE)
roidf$birdcode<-toupper(roidf$birdcode)
gvedf<-subset(gvedf,species %in% roidf$birdcode)

## do we have enough GV data to assess the models?
enoughLim<-40
numdf<-aggregate(secs~species,gvedf,NROW); names(numdf)<-c("species","count")
speciesmodeled<-subset(numdf,count >= enoughLim)$species
NROW(speciesmodeled)
gvedf<-subset(gvedf,species %in% speciesmodeled)
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)
events<-unique(gvedf$SamplingEvent)

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)
tm<-Sys.time()
tstmatches<-getModelHurdledMatches(data=tst,gvedf=gvedf,hurdvals=hurdvals)
prodmatches<-getModelHurdledMatches(data=prod,gvedf=gvedf,hurdvals=hurdvals)
allmatches<-rbind(tstmatches,prodmatches)
allmatches$model<-paste0(allmatches$modelVersion,"::",allmatches$ModelName,"::",allmatches$PredictionScoreType)

save(allmatches,file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/allmaches_CallNotTrained_65to99999.RData")
Sys.time()-tm

## Then plot as above



#####################################################################################################################################################################################
#####################################################################################################################################################################################
## Matt asked:
## 2) for non-target birds, does the CNN confuse these with our target birds (after penalization)?  (And if it does confuse, then we could look at the calls to see if the confused species are similar.)
## We load the GV data from matchGVtoPreds_part1.R but now we work with the data for birds modeled, but not the call modeled

## CAUTION: add the species matched to getMatches function

load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/pretrained.RData")
load(file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/nopretrain.RData")

load("c:/users/lsalas/git/S2L_devel/GVanalyses/3models2outputs/data/gvData.RData")

gvedf<-subset(gvedf,is.na(call)); nrow(gvedf)

gvedf$species<-toupper(gvedf$species)
## Now we need the species selected for training:
roidf<-read.csv("c:/users/lsalas/git/S2L_devel/GVanalyses/data/pattern_matching_ROIs_201109.csv", stringsAsFactors=FALSE)
roidf$birdcode<-toupper(roidf$birdcode)
gvedf<-subset(gvedf,!species %in% roidf$birdcode)
## do we have enough GV data to assess the models?
enoughLim<-40
numdf<-aggregate(secs~species,gvedf,NROW); names(numdf)<-c("species","count")
speciesmodeled<-subset(numdf,count >= enoughLim)$species
NROW(speciesmodeled)
gvedf<-subset(gvedf,species %in% speciesmodeled)
gvedf$SamplingEvent<-substr(gvedf$RegistryName,1,32)
events<-unique(gvedf$SamplingEvent)

hurdvals<-c(seq(0.65,0.99,0.01),0.999,0.9999,0.99999)
tm<-Sys.time()
tstmatches<-getModelHurdledMatches(data=tst,gvedf=gvedf,hurdvals=hurdvals)
prodmatches<-getModelHurdledMatches(data=prod,gvedf=gvedf,hurdvals=hurdvals)
allmatches<-rbind(tstmatches,prodmatches)
allmatches$model<-paste0(allmatches$modelVersion,"::",allmatches$ModelName,"::",allmatches$PredictionScoreType)

save(allmatches,file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/modelsDataAll/allmaches_SpeciesNotTrained_65to99999.RData")
Sys.time()-tm

## Then evaluate the matches!!







