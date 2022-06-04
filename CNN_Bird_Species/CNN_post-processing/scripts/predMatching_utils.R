# TODO: Add comment
# In pickled records, the second reported is the starting second: use the floor(x1)-1 to ceiling(x2)-1 to match with the ROI
# In databased records, the second reported is the middle second: use the floor(x1) to ceiling(x2) to match with the ROI 
# In pickled records, the second reported is the starting second: use the secs-1 to secs to match with the GV
# In databased records, the second reported is the middle second: use the secs to secs+1 to match with the GV 
# Author: lsalas
###############################################################################


## These are the utility matching functions used to process all the data for predictions from AI models


#####
# This function checks that nams in inut tables are correct
# predsdf is the table of predictions, must have: PredictionId, ModelName, SpeciesCode, PredictionScore, SamplingEvent, and Second
# labsdf is the table of labels, must have for GV: species, secs, SamplingEvent 
# 								must have for ROI: event, x1, x2, birdcode, vote
# labelSource is either "ROI" or "GV"
checkNames<-function(predsdf,labsdf,labelSource){
	for(nn in c("PredictionId","ModelName","SpeciesCode","PredictionScore","SamplingEvent","Second")){
		if(!nn %in% names(predsdf))print(paste("Predictions:",nn,"not found"))
	}
	
	if(labelSource=="GV"){
		for(nn in c("species","secs","SamplingEvent")){
			if(!nn %in% names(labsdf))print(paste("Labels:",nn,"not found"))
		}
	}else{ #ROI
		for(nn in c("event","x1","x2","birdcode","vote")){
			if(!nn %in% names(labsdf))print(paste("Labels:",nn,"not found"))
		}
	}
}

#####
## This function is a high-level cycling function seeking the matches for all hurdle levels for each model against the golden validations
# data is the predictions data.frame
# gvedf is the data frame of GV data, from the script makeGVdf.R. Must have: species, secs, SamplingEvent 
# hurdvals is the vactor of hurdles to be used to penalize the data
# dataSource is a text flag to indicate if the predictions source is database or pickle, because the second of the prediction is different in each
# gvGetMatches is a function called within this function
# removeDoubleGVMatches is a function called within this function
## REQUIREMENTS: data must have fields PredictionId, ModelName, SpeciesCode, PredictionScore, SamplingEvent, and Second
## REQUIREMENTS: hurdle is a value between 0 and 1, but PredictionScore is a number between 0 and 9999999
## REQUIREMENTS: gvedf must have fields species, secs, SamplingEvent
getModelHurdledGVMatches<-function(data,gvedf,hurdvals,dataSource="database",gvGetMatches,removeDoubleGVMatches){
	
	library(doParallel)
	nodes<-detectCores()
	cl<-makeCluster(nodes)
	registerDoParallel(cl)
	
	modelList<-list()
	for(mm in unique(data$ModelName)){
		datm<-subset(data,ModelName==mm)
		hurdleList<-list()
		for(hh in hurdvals){
			hv<-hh*(10^7)
			datmh<-subset(datm,PredictionScore>=hv)
			hbelow<-subset(datm, PredictionScore<hv)
			## We start by finding a match to all GV records...
			hurdledf<-ldply(.data=unique(gvedf$SamplingEvent), .parallel=TRUE, .fun=function(ee,datmh,hbelow,gvedf,dataSource,getGVMatches,removeDoubleGVMatches){
						rdf<-subset(datmh,SamplingEvent==ee)
						if(nrow(rdf)>0){rdf<-rdf[order(rdf$Second),]}
						gdf<-subset(gvedf,SamplingEvent==ee)
						gdf<-gdf[order(gdf$secs),]
						
						match<-getGVMatches(rdf=rdf,gdf=gdf,dataSource=dataSource,removeDoubleGVMatches=removeDoubleGVMatches)
						if(nrow(match)>0){
							match$SamplingEvent<-ee
						}
						
						# Get TRUEPOS below the hurdle - these are FALSENEG
						hbdf<-subset(hbelow,SamplingEvent==ee)
						hbmatch<-data.frame()
						if(nrow(hbdf)>0){
							hbmatch<-getGVMatches(rdf=hbdf,gdf=gdf,dataSource=dataSource,removeDoubleGVMatches=removeDoubleGVMatches)
							hbmatch$SamplingEvent<-ee
							hbmatch<-subset(hbmatch,match=="TRUEPOS")
							if(nrow(hbmatch)>0){
								hbmatch$match<-"FALSENEG";hbmatch$matchDelta<-8
							}
						}
												
						match<-rbind(match,hbmatch)
						return(match)
					},datmh=datmh,hbelow=hbelow,gvedf=gvedf,dataSource=dataSource,getGVMatches=getGVMatches,removeDoubleGVMatches=removeDoubleGVMatches)
			if(nrow(hurdledf)>0){
				hurdledf$hurdle<-hh
			}
			
			hn<-paste0("Hurdle_",hh)
			hurdleList[[hn]]<-hurdledf
			
		}
		hurdres<-rbindlist(hurdleList)
		hurdres$ModelName<-mm
		
		modelList[[mm]]<-hurdres
		
	}
	
	res<-rbindlist(modelList)
	return(res)
}


######
# This is the workhorse matching function for GVs - this matching is minute-by-minute
# rdf is the predictions data.frame called from getModelHurdledGVMatches
# gdf is the GV detections single-event data.frame also called from getModelHurdledMatches
# dataSource is the source of predictions
# removeDoubleGVMatches is a function called within this function
getGVMatches<-function(rdf,gdf,dataSource,removeDoubleGVMatches){	#Processing one recording at the time
	
	## This needs to happen each GV second by GV second, because the model should predict only where there is a GV record
	# For each GV label: is there a prediction?
	# A GV record reports the starting second of every second of a species call
	eventdf<-data.frame()
	eventfn<-data.frame()
	for(ss in unique(gdf$secs)){
		gvsec<-subset(gdf,secs==ss)
		
		## There can be more than one label in a second
		for(kk in 1:nrow(gvsec)){
			spp<-as.character(gvsec[kk,"species"])
			
			# In pickled records, the second reported is the starting second: use the secs-1 to secs to match with the GV
			# In databased records, the second reported is the middle second: use the secs to secs+1 to match with the GV 
			if(dataSource=="database"){
				secsearch<-seq(ss-1,ss+2)		#ss,ss+1
			}else{
				secsearch<-seq(ss-2,ss+1)		#ss-1,ss
			}
			trdf<-subset(rdf,Second %in% secsearch)
			
			# There is a gv record and possibly also a prediction
			evt<-gvsec$SamplingEvent[1]
			
			if(nrow(trdf)==0){	
				# no predictions overlap the GV record, so the GV record is a FN
				# careful here, because the same records will be repeated for species labeled in the same second
				efn<-data.frame(PredictionId=0, SamplingEvent=evt, Second=NA, GVsecond=ss, SpeciesCode=spp, 
						GVspeciesCode=spp, PredictionScore=9999999, match="FALSENEG", matchDelta=8)
				eventfn<-rbind(eventfn,efn)
				madf<-data.frame()
			}else{
				# there are predictions overlapping the GV record
				# all predictions matching the species are given TRUEPOS
				strdf<-subset(trdf,SpeciesCode==spp)
				madftp<-data.frame(); madffp<-data.frame()
				if(nrow(strdf)>0){
					madftp<-data.frame(PredictionId=strdf$PredictionId, SamplingEvent=strdf$SamplingEvent, Second=strdf$Second, GVsecond=rep(ss,nrow(strdf)), SpeciesCode=strdf$SpeciesCode, 
							GVspeciesCode=strdf$SpeciesCode, PredictionScore=strdf$PredictionScore, match=rep("TRUEPOS",nrow(strdf)), matchDelta=rep(1,nrow(strdf)))
				}
				
				# all predictions not matching the species are given FALSEPOS (but filtered out later if they end up being TRUEPOS for another GV second)
				nstrdf<-subset(trdf,SpeciesCode!=spp)
				if(nrow(nstrdf)>0){
					madffp<-data.frame(PredictionId=nstrdf$PredictionId, SamplingEvent=nstrdf$SamplingEvent, Second=nstrdf$Second, GVsecond=rep(ss,nrow(nstrdf)), SpeciesCode=nstrdf$SpeciesCode, 
							GVspeciesCode=rep(spp,nrow(nstrdf)), PredictionScore=nstrdf$PredictionScore, match=rep("FALSEPOS",nrow(nstrdf)), matchDelta=rep(9,nrow(nstrdf)))
				}
				madf<-rbind(madftp,madffp)
			}
			eventdf<-rbind(eventdf,madf)
		}
	}
	
	## We add the false negatives
	eventfn<-unique(eventfn)
	eventdf<-rbind(eventdf,eventfn)
	
	# Must now ensure all FALSEPOS are not TRUEPOS for a different GV record...
	eventTPR<-subset(eventdf,match=="TRUEPOS")$PredictionId
	eventdf<-subset(eventdf, (match %in% c("TRUEPOS","FALSENEG")) | (match=="FALSEPOS" & !PredictionId %in% eventTPR))
	
	# It is also posible that the same TRUEPOS prediction is found in two consecutive GV records of the same species, so...  THIS IS OK1
	#if(nrow(eventdf)>0){
	#	eventdf<-removeDoubleGVMatches(edf=eventdf)
	#}
	
	## We then add all predictions not in eventdf at all (by PredictionId)
	drdf<-subset(rdf,!PredictionId %in% eventdf$PredictionId)
	if(nrow(drdf)>0){
		dmadf<-data.frame(PredictionId=drdf$PredictionId, SamplingEvent=drdf$SamplingEvent, Second=drdf$Second, GVsecond=rep(NA,nrow(drdf)), SpeciesCode=drdf$SpeciesCode, 
				GVspeciesCode=rep(NA,nrow(drdf)), PredictionScore=drdf$PredictionScore, match=rep("FALSEPOS",nrow(drdf)), matchDelta=rep(9,nrow(drdf)))
		eventdf<-rbind(eventdf,dmadf)
	}
	
	# Done
	return(eventdf)
}


#####
# Since getMatches can assign the same TRUEPOS or FALSEPOS prediction to two different and consecutive GV records (because a prediction spans 2 seconds and a GV label only 1)
# This function removes matches to ensure the same prediction does not match two consecutive GV records and thus is entered as a TRUEPOS twice
# This function is called by the getMatches function
# edf is the data frame sent from the getMatches function
removeDoubleGVMatches<-function(edf){
	edf$eventRecId<-1:nrow(edf)
	eventTPR<-subset(edf,match=="TRUEPOS")$PredictionId
	if(NROW(eventTPR)>NROW(unique(eventTPR))){
		remTPR<-numeric()
		for(pp in unique(eventTPR)){
			terem<-subset(edf,match=="TRUEPOS" & PredictionId==pp)$eventRecId
			if(NROW(terem)>1){remTPR<-c(remTPR,terem[-1])}
		}
		edf<-subset(edf,!eventRecId %in% c(remTPR))
	}
	
	eventFPR<-subset(edf,match=="FALSEPOS")$PredictionId
	if(NROW(eventFPR)>NROW(unique(eventFPR))){
		remFPR<-numeric()
		for(pp in unique(eventFPR)){
			ferem<-subset(edf,match=="FALSEPOS" & PredictionId==pp)$eventRecId
			if(NROW(ferem)>1){remFPR<-c(remFPR,ferem[-1])}
		}
		edf<-subset(edf,!eventRecId %in% c(remFPR))
	}
	
	edf<-edf[,which(names(edf)!="eventRecId")]
	return(edf)
}


#####
# This is the high-level function to loop through all models and hurdle levels for ROI prediction matches
# data is the data.frame of predictions for the ROI events in the test set, by model
# roidf is the list of ROIs in the test set
# hurdvals is the vector of hurdles
# dataSource is a text value that indicates if the set of predictions came from a database or pickle, to define correctly the overlapping span
# getROIMatches is the workhorse matching function
# removeDoubleROIMatches is a function called by this one to remove dupes
## REQUIREMENTS - data must have: PredictionScore, ModelName, SamplingEvent, SpeciesCode, Second, PredictionId
## REQUIREMENTS - roidf must have: event, x1, x2, birdcode, vote
getModelHurdledROIMatches<-function(data,roidf,hurdvals,dataSource="database",getROIMatches,removeDoubleROIMatches){
	## Must use the test set as presences, and the full set for vote="not present" for absences of the species and only of the species
	## At hurdle 0.65 the "not present" set will also generate the set for logistic regression
	library(doParallel)
	nodes<-detectCores()
	cl<-makeCluster(nodes)
	registerDoParallel(cl)
	
	modelList<-list()
	for(mm in unique(data$ModelName)){
		datm<-subset(data,ModelName==mm)
		hurdleList<-list()
		for(hh in hurdvals){
			hv<-hh*(10^7)
			datmh<-subset(datm,PredictionScore>=hv)
			
			## We start by finding a match to all GV records...
			hurdledf<-ldply(.data=unique(roidf$event), .parallel=TRUE, .fun=function(ee,datmh,roidf,dataSource,getROIMatches,removeDoubleROIMatches){
						preddf<-subset(datmh,SamplingEvent==ee)
						if(nrow(preddf)>0){preddf<-preddf[order(preddf$Second),]}
						labeldf<-subset(roidf,event==ee)
						labeldf<-labeldf[order(labeldf$roicenter),]
						match<-getROIMatches(preddf=preddf,labeldf=labeldf,dataSource=dataSource,removeDoubleROIMatches=removeDoubleROIMatches)
						
						return(match)
					
					},datmh=datmh,roidf=roidf,dataSource=dataSource,getROIMatches=getROIMatches,removeDoubleROIMatches=removeDoubleROIMatches)
			if(nrow(hurdledf)>0){
				hurdledf$hurdle<-hh
			}
			
			hn<-paste0("Hurdle_",hh)
			hurdleList[[hn]]<-hurdledf
			
		}
		hurdres<-rbindlist(hurdleList)
		hurdres$ModelName<-mm
		
		modelList[[mm]]<-hurdres
		
	}
	
	res<-rbindlist(modelList)
	return(res)
	
}


######
# This is the workhorse matching function for ROIs - this matching is une label record and model at the time to be able to parallelize
# labeldf is the data.frame of roi centers
# preddf is the hurdled data frame of predictions for a model in the labeled data
getROIMatches<-function(labeldf, preddf,dataSource,removeDoubleROIMatches){  
	
	eventdf<-data.frame()
	
	# looping through each label in the event...
	for(rr in 1:nrow(labeldf)){
		ldf<-labeldf[rr,]
		spp<-as.character(ldf$birdcode)
		evt<-ldf$event
		sec<-round(ldf$roicenter)
		
		# In pickled records, the second reported is the starting second: use the floor(x1)-1 to ceiling(x2)-1 to match with the ROI
		# In databased records, the second reported is the middle second: use the floor(x1) to ceiling(x2) to match with the ROI 
		if(dataSource=="database"){
			secsearch<-seq(floor(ldf$x1),ceiling(ldf$x2))
		}else{
			secsearch<-seq(floor(ldf$x1)-1,ceiling(ldf$x2)-1)
		}
		
		# Find predictions in the span
		trdf<-subset(preddf,Second %in% secsearch)
		
		## Given the species, event, and second in the label, find the matching predictions within the appropriate span
		##	1) Any prediction that is a species match in the span --> TP
		##  2) No prediction to match the label species in the span means that the label becomes a FN
		##  Can do NOTHING else
		
		if(nrow(trdf)==0){ #if no predictions in the span....
			# the label is a FALSENEG
			madf<-data.frame(PredictionId=0, SamplingEvent=evt, Second=NA, ROIsecond=sec, SpeciesCode=spp, ROIspeciesCode=spp, PredictionScore=9999999, match="FALSENEG", matchDelta=8)
		}else{	#predictions in the span
			strdf<-subset(trdf,SpeciesCode==spp)
			if(nrow(strdf)>0){
			# any predictions that are of the label species are true positives
				madf<-data.frame(PredictionId=strdf$PredictionId, SamplingEvent=strdf$SamplingEvent, Second=strdf$Second, ROIsecond=rep(sec,nrow(strdf)), SpeciesCode=strdf$SpeciesCode, 
								ROIspeciesCode=rep(spp,nrow(strdf)), PredictionScore=strdf$PredictionScore, match=rep("TRUEPOS",nrow(strdf)), matchDelta=rep(1,nrow(strdf)))
			}else{
				madf<-data.frame(PredictionId=trdf$PredictionId, SamplingEvent=trdf$SamplingEvent, Second=trdf$Second, ROIsecond=rep(sec,nrow(trdf)), SpeciesCode=trdf$SpeciesCode, 
						ROIspeciesCode=rep(spp,nrow(trdf)), PredictionScore=trdf$PredictionScore, match=rep("FALSEPOS",nrow(trdf)), matchDelta=rep(9,nrow(trdf)))
			}
		}
		
		
		eventdf<-rbind(eventdf,madf)
	}
	
	if(nrow(eventdf)>0){
		eventdf<-removeDoubleROIMatches(edf=eventdf)
	}
	
	
	return(eventdf)
}


#####
# This function removes duplicate TRUEPOS in the event, called from getROIMatch
# edf is the eventdf data.frame result from the matching function
removeDoubleROIMatches<-function(edf){
	## A TP duplicate is by definition the same prediction counting more than once as a TP 
	edf$eventRecId<-1:nrow(edf)
	eventTPR<-subset(edf,match=="TRUEPOS")$PredictionId
	if(NROW(eventTPR)>NROW(unique(eventTPR))){  #have dupes
		remTPR<-numeric()
		for(pp in unique(eventTPR)){
			terem<-subset(edf,match=="TRUEPOS" & PredictionId==pp)$eventRecId
			if(NROW(terem)>1){remTPR<-c(remTPR,terem[-1])}
		}
		edf<-subset(edf,!eventRecId %in% c(remTPR))
	}
	
		
	## ... or a duplicate false positive
	eventFPR<-subset(edf,match=="FALSEPOS")$PredictionId
	if(NROW(eventFPR)>NROW(unique(eventFPR))){  #have dupes
		remFPR<-numeric()
		for(pp in unique(eventFPR)){
			terem<-subset(edf,match=="FALSEPOS" & PredictionId==pp)$eventRecId
			if(NROW(terem)>1){remFPR<-c(remFPR,terem[-1])}
		}
		edf<-subset(edf,!eventRecId %in% c(remFPR))
	}
	
	edf<-edf[,which(names(edf)!="eventRecId")]
	
	return(edf)
	
}


#####
# This function collects the FALSEPOS for ROI data
# It does so by using "not present" ROIs -- those whose prediction is the species "not present" are confirmed FALSEPOS predictions
# There is no need to loop through hurdles here, as we report the PredictionScore of the FP
# Separately, we add the FP record for each hurdle level in the ROI matches
# allnproidata is the data.frame of the "not present" labels
# roipreds is the list of predictions of events in allnproidata, one data frame per model
# getROIfalseposMatch is the matching function called within this one
# removeDoubleFP is a function called by the matching function to remove FP dupes by PredictionId
## REQUIREMENTS - data must have: PredictionScore, ModelName, SamplingEvent, SpeciesCode, Second, PredictionId
## REQUIREMENTS - roidf must have: event, x1, x2, birdcode, vote
## REQUIREMENTS - roipreds are all predictions only for the events in allnproidata
getROIfalsepos<-function(allnproidata,roipreds,dataSource,getROIfpMatch,removeDoubleFP){
	#cycle through every label record and find a prediction within the span
	#library(doParallel)
	#nodes<-detectCores()
	#cl<-makeCluster(nodes)
	#registerDoParallel(cl)
	
	modelList<-list()
	for(mm in unique(roipreds$ModelName)){
		datm<-subset(roipreds,ModelName==mm)
		eventdf<-ldply(.data=unique(datm$SamplingEvent), .parallel=FALSE, .fun=function(ee,datm,allnproidata,dataSource,getROIfpMatch,removeDoubleFP){
					preddf<-subset(datm,SamplingEvent==ee)
					preddf<-preddf[order(preddf$Second),]
					labeldf<-subset(allnproidata,event==ee)
					labeldf<-labeldf[order(labeldf$roicenter),]
					match<-getROIfpMatch(preddf=preddf,labeldf=labeldf,dataSource=dataSource)
					if(nrow(match)>0){
						match$SamplingEvent<-ee
						
						## Because we center on the roi label, not on the prediction, a dupe is the same prediction considered as a FP for two consecutive labels
						match<-removeDoubleFP(match)
					}
					return(match)
				},datm=datm,allnproidata=allnproidata,dataSource=dataSource,getROIfpMatch=getROIfpMatch,removeDoubleFP=removeDoubleFP)
		
		if(nrow(eventdf)>0){
			eventdf$ModelName<-mm
		}else{
			eventdf<-data.frame()
		}
		
		modelList[[mm]]<-eventdf
	}
	
	return(modelList)
}


#####
# This is the workhorse function to match the false positive ROIs
# It is called by getROIfalsepos
# preddf is the data.frame of predictions for a survey event
# labeldf is the data.frame of "not present" labels in that event
# dataSource is a string that indicates the source of predictions to know how to generate the matching time span
getROIfpMatch<-function(preddf,labeldf,dataSource){
	
	fpres<-data.frame()
	
	for(rr in 1:nrow(labeldf)){
		ldf<-labeldf[rr,]
		spp<-ldf$birdcode
		sec<-round(ldf$roicenter)
		
		# In pickled records, the second reported is the starting second: use the floor(x1)-1 to ceiling(x2)-1 to match with the ROI
		# In databased records, the second reported is the middle second: use the floor(x1) to ceiling(x2) to match with the ROI 
		if(dataSource=="database"){
			secsearch<-seq(floor(ldf$x1),ceiling(ldf$x2))
		}else{
			secsearch<-seq(floor(ldf$x1)-1,ceiling(ldf$x2)-1)
		}
		
		# Find predictions in the span for the label species
		# We only care for species matches, nothing else
		trdf<-subset(preddf,Second %in% secsearch & SpeciesCode==spp)
		
		if(nrow(trdf)>0){
			# There are predictions in the span, all False Positives
			madf<-data.frame(PredictionId=trdf$PredictionId, SamplingEvent=trdf$SamplingEvent, Second=trdf$Second, ROIsecond=rep(sec,nrow(trdf)), SpeciesCode=trdf$SpeciesCode, 
					ROIspeciesCode=rep(spp,nrow(trdf)), PredictionScore=trdf$PredictionScore, match=rep("FALSEPOS",nrow(trdf)), matchDelta=rep(9,nrow(trdf)))
			fpres<-rbind(fpres,madf)
		}
	}
	return(fpres)
}


#####
# This function ensures that there are no duplicate FP assignments by ensuring that each record has a unique PredictionId
# A dupe is the same prediction on two consecutive roi labels
# edf is the event's false positives data.frame
removeDoubleFP<-function(edf){
	
	edf$eventRecId<-1:nrow(edf)
	eventFPR<-edf$PredictionId
	if(nrow(edf)>NROW(unique(edf$PredictionId))){  #have dupes
		remTPR<-numeric()
		for(pp in unique(eventFPR)){
			terem<-subset(edf,PredictionId==pp)$eventRecId
			if(NROW(terem)>1){remTPR<-c(remTPR,terem[-1])}
		}
		edf<-subset(edf,!eventRecId %in% c(remTPR))
	}
	
	edf<-edf[,which(names(edf)!="eventRecId")]
	return(edf)
	
}


#####
# This function collects the TRUEPOS for ROI data
# It does so by using "present" ROIs -- those whose prediction is the species "not present" are confirmed FALSEPOS predictions
# There is no need to loop through hurdles here, as we report the PredictionScore of the FP
# Separately, we add the FP record for each hurdle level in the ROI matches
# allnproidata is the data.frame of the "present" labels
# roipreds is the list of predictions of events in allnproidata, one data frame per model
# getROItrueposMatch is the matching function called within this one
# removeDoubleFP is a function called by the matching function to remove FP dupes by PredictionId
## REQUIREMENTS - data must have: PredictionScore, ModelName, SamplingEvent, SpeciesCode, Second, PredictionId
## REQUIREMENTS - roidf must have: event, x1, x2, birdcode, vote
## REQUIREMENTS - roipreds are all predictions only for the events in allnproidata
getROItruepos<-function(allnproidata,roipreds,dataSource,getROItpMatch,removeDoubleFP){
	#cycle through every label record and find a prediction within the span
	#library(doParallel)
	#nodes<-detectCores()
	#cl<-makeCluster(nodes)
	#registerDoParallel(cl)
	
	modelList<-list()
	for(mm in unique(roipreds$ModelName)){
		datm<-subset(roipreds,ModelName==mm)
		eventdf<-ldply(.data=unique(datm$SamplingEvent), .parallel=FALSE, .fun=function(ee,datm,allnproidata,dataSource,getROItpMatch,removeDoubleFP){
					preddf<-subset(datm,SamplingEvent==ee)
					preddf<-preddf[order(preddf$Second),]
					labeldf<-subset(allnproidata,event==ee)
					labeldf<-labeldf[order(labeldf$roicenter),]
					match<-getROItpMatch(preddf=preddf,labeldf=labeldf,dataSource=dataSource)
					if(nrow(match)>0){
						match$SamplingEvent<-ee
						
						## Because we center on the roi label, not on the prediction, a dupe is the same prediction considered as a FP for two consecutive labels
						match<-removeDoubleFP(match)
					}
					return(match)
				},datm=datm,allnproidata=allnproidata,dataSource=dataSource,getROItpMatch=getROItpMatch,removeDoubleFP=removeDoubleFP)
		
		if(nrow(eventdf)>0){
			eventdf$ModelName<-mm
		}else{
			eventdf<-data.frame()
		}
		
		modelList[[mm]]<-eventdf
	}
	
	return(modelList)
}


#####
# This is the workhorse function to match the false positive ROIs
# It is called by getROItruepos
# preddf is the data.frame of predictions for a survey event
# labeldf is the data.frame of "not present" labels in that event
# dataSource is a string that indicates the source of predictions to know how to generate the matching time span
getROItpMatch<-function(preddf,labeldf,dataSource){
	
	fpres<-data.frame()
	
	for(rr in 1:nrow(labeldf)){
		ldf<-labeldf[rr,]
		spp<-ldf$birdcode
		sec<-round(ldf$roicenter)
		
		# In pickled records, the second reported is the starting second: use the floor(x1)-1 to ceiling(x2)-1 to match with the ROI
		# In databased records, the second reported is the middle second: use the floor(x1) to ceiling(x2) to match with the ROI 
		if(dataSource=="database"){
			secsearch<-seq(floor(ldf$x1),ceiling(ldf$x2))
		}else{
			secsearch<-seq(floor(ldf$x1)-1,ceiling(ldf$x2)-1)
		}
		
		# Find predictions in the span for the label species
		# We only care for species matches, nothing else
		trdf<-subset(preddf,Second %in% secsearch & SpeciesCode==spp)
		
		if(nrow(trdf)>0){
			# There are predictions in the span, all TRUE Positives
			madf<-data.frame(PredictionId=trdf$PredictionId, SamplingEvent=trdf$SamplingEvent, Second=trdf$Second, ROIsecond=rep(sec,nrow(trdf)), SpeciesCode=trdf$SpeciesCode, 
					ROIspeciesCode=rep(spp,nrow(trdf)), PredictionScore=trdf$PredictionScore, match=rep("TRUEPOS",nrow(trdf)), matchDelta=rep(9,nrow(trdf)))
			fpres<-rbind(fpres,madf)
		}
	}
	return(fpres)
}


##### 
# Need a function to summarize matches to sample level by species - calls to this function are to be put on a loop of hurdle values, called by summarizeByHurdle
# matches is the data frame of matches, from getMatches
# returns: cnnSpecies, TP, FP, RegistriName, model, hurdle, metrics
# Metrics: 
# 	Sensitivity: TP/(TP+FN)
# 	Precision: TP/(TP+FP)
# 	F1: (2*Sensitivity*Precision)/(Sensitivity+Precision)
# 	Miss rate: FN/(FN+TP)
#	FPper: FP/(TP+FP)
#   Fbeta: (1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
summarizeToSample<-function(matches,beta){
	sumToModel<-ldply(unique(matches$ModelName),function(mm,matches){
				mdf<-subset(matches,ModelName==mm)
				trval<-sum(mdf$match=="TRUEPOS")
				fnval<-sum(mdf$match=="FALSENEG")
				fpval<-sum(mdf$match=="FALSEPOS")
				sens<-ifelse(trval+fnval==0,0,trval/(trval+fnval))
				prec<-ifelse(trval+fpval==0,0,trval/(trval+fpval))
				f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
				miss<-ifelse(trval+fnval==0,0,fnval/(trval+fnval))
				fpper<-ifelse(trval+fpval==0,0,fpval/(trval+fpval))
				fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
				modeldf<-data.frame(count=nrow(mdf),truePos=trval,falseNeg=fnval,falsePos=fpval,
						F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
				modeldf$ModelName<-mm
				return(modeldf)
			},matches=matches)
	return(sumToModel)
}

summarizeToEvent<-function(matches,bySpecies,beta,addEvent){
	#split by model
	sumToModel<-ldply(unique(matches$ModelName),function(mm,matches,bySpecies,beta,addEvent){
				mdf<-subset(matches,ModelName==mm)
				#asign TP, FP, and FN by species+event, then add up by model or by species and model
				eventspeciesdf<-ldply(unique(mdf$SpeciesCode),function(ss,mdf,addEvent){
							smatches<-subset(mdf,SpeciesCode==ss)
							eventsdf<-ldply(unique(smatches$SamplingEvent),function(ee,smatches){
										edf<-subset(smatches,SamplingEvent==ee)
										if(TRUE %in% grepl("TRUEPOS",edf$match)){
											trv<-1;fnv<-0;fpv<-0
										}else{
											trv<-0
											fnv<-ifelse(TRUE %in% grepl("FALSENEG",edf$match),1,0)
											fpv<-ifelse(TRUE %in% grepl("FALSEPOS",edf$match),1,0)
										}
										redf<-data.frame(trv=trv,fnv=fnv,fpv=fpv)
										if(addEvent==TRUE){redf$event<-ee}
										return(redf)
									},smatches=smatches)
							eventsdf$SpeciesCode<-ss
							return(eventsdf)
						},mdf=mdf,addEvent=addEvent)
				
				if(addEvent==TRUE){ #we aggregate at the species level for each event
					mdldf<-eventspeciesdf %>% group_by(SpeciesCode,event) %>% dplyr::summarise(trval=sum(trv),fnval=sum(fnv),fpval=sum(fpv),count=NROW(trv))
					modeldf<-ldply(unique(mdldf$event),function(ee,mdldf,beta){
								evmdldf<-subset(mdldf,event==ee)
								if(bySpecies=="yes"){
									spmdldf<-ldply(unique(evmdldf$SpeciesCode),function(ss,evmdldf,beta){
												sdf<-subset(evmdldf,SpeciesCode==ss) #there should be just one value per species+model+hurdle
												sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
												prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
												f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
												miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
												fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
												fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
												resdf<-data.frame(SpeciesCode=ss,Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
														F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
											},evmdldf=evmdldf,beta=beta)
													
								}else{
									#Before calculating the statistics, aggregate to the event-model level
									sdf<-evmdldf %>% dplyr::summarise(trval=sum(trval),fnval=sum(fnval),fpval=sum(fpval),count=sum(count))
									#now there should be just one row per model
									sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
									prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
									f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
									miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
									fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
									fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
									spmdldf<-data.frame(Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
											F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
									
								}
								spmdldf$event<-ee
								return(spmdldf)
							},mdldf=mdldf,beta=beta)

				}else{# or we aggregate at the species level across events
					mdldf<-eventspeciesdf %>% group_by(SpeciesCode) %>% dplyr::summarise(trval=sum(trv),fnval=sum(fnv),fpval=sum(fpv),count=NROW(trv))
					
					if(bySpecies=="yes"){
						modeldf<-ldply(unique(mdldf$SpeciesCode),function(ss,mdldf,beta){
									sdf<-subset(mdldf,SpeciesCode==ss) #there should be just one value per species+model+hurdle
									sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
									prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
									f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
									miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
									fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
									fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
									resdf<-data.frame(SpeciesCode=ss,Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
											F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
								},mdldf=mdldf,beta=beta)
						
					}else{
						#Before calculating the statistics, aggregate to the model level
						sdf<-mdldf %>% dplyr::summarise(trval=sum(trval),fnval=sum(fnval),fpval=sum(fpval),count=sum(count))
						#now there should be just one row per model
						sens<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$trval/(sdf$trval+sdf$fnval))
						prec<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$trval/(sdf$trval+sdf$fpval))
						f1val<-ifelse(sens+prec==0,0,(2*sens*prec)/(sens+prec))
						miss<-ifelse(sdf$trval+sdf$fnval==0,0,sdf$fnval/(sdf$trval+sdf$fnval))
						fpper<-ifelse(sdf$trval+sdf$fpval==0,0,sdf$fpval/(sdf$trval+sdf$fpval))
						fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
						modeldf<-data.frame(Count=sdf$count,truePos=sdf$trval,falseNeg=sdf$fnval,falsePos=sdf$fpval,
								F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
					}
					
				}
				
				modeldf$ModelName<-mm
				return(modeldf)
				
			},matches=matches,bySpecies=bySpecies,beta=beta,addEvent=addEvent)
	
			return(sumToModel)
}
#####
# This function provides metrics by hurdle value, using summarizeToSample to estimate by species and model
# allmatches is the data frame of all the matches at all hurdle levels for all species and models
summarizeByHurdle<-function(allmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE){
	hurdvals<-unique(allmatches$hurdle)
	sumByHurdle<-ldply(hurdvals,function(hh,allmatches,summarizeToSample,summarizeToEvent,bySpecies,sumLevel,beta,addEvent){
				hmatches<-subset(allmatches,hurdle==hh)
				if(bySpecies=="no"){
					if(sumLevel=="minute"){
						sumMatches<-summarizeToEvent(matches=hmatches,beta=beta,bySpecies=bySpecies,addEvent=addEvent)
					}else{
						sumMatches<-summarizeToSample(matches=hmatches,beta=beta)
					}
				}else{
					if(sumLevel=="minute"){
						sumMatches<-summarizeToEvent(matches=hmatches,beta=beta,bySpecies=bySpecies,addEvent=addEvent)
					}else{
						sumMatches<-ldply(unique(hmatches$SpeciesCode),function(ss,hmatches,summarizeToSample,beta){
									shmatches<-subset(hmatches,SpeciesCode==ss)
									ssmatches<-summarizeToSample(matches=shmatches,beta=beta)
									ssmatches$SpeciesCode<-ss
									return(ssmatches)
								},hmatches=hmatches,summarizeToSample=summarizeToSample,beta=beta)
					}
					
				}
				sumMatches$hurdle<-hh
				return(sumMatches)
			},allmatches=allmatches,summarizeToSample=summarizeToSample,summarizeToEvent=summarizeToEvent,bySpecies=bySpecies,sumLevel=sumLevel,beta=beta,addEvent=addEvent)
	return(sumByHurdle)
}


#####
# This function calculates the F score weighted by a beta value
getFvalbeta<-function(df,beta){
	fv<-(1+(beta^2))*(df$Prec*df$Sens)/(((beta^2)*df$Prec) + df$Sens)
	return(fv)
}


#####
## This function retrieves the number of FP and predictions per species, model and hurdle value
# hurdvals is the vector of hurdles
# species is the vector of species to review
# fpMatchesList is the list of data.frames, one per model, containing the false positives match results
# labelPredsList is the list of data.frames, one per model, containing the original predictions (need this to obtain the number of tests)
getFPnumsSp<-function(hurdvals,species,fpMatchesList,labelPredsList){
	fpRatesList<-list()
	for(mm in 1:3){
		fpmdf<-fpMatchesList[[mm]]
		lpdf<-labelPredsList[[mm]]
		modName<-unique(as.character(fpmdf$ModelName))
		modeldf<-ldply(species,function(ss,hurdvals,fpmdf,lpdf){
					fpmsdf<-subset(fpmdf,SpeciesCode==ss)
					lpsdf<-subset(lpdf,SpeciesCode==ss)
					speciesdf<-ldply(hurdvals,function(hh,fpmsdf,lpsdf){
								hv<-hh*(10^7)
								if(nrow(fpmsdf)>0){
									nfp<-nrow(subset(fpmsdf,PredictionScore>=hv))
								}else{
									nfp<-nrow(subset(fpmsdf,PredictionScore>=hv))
								}
								if(nrow(lpsdf)>0){
									npreds<-nrow(subset(lpsdf,PredictionScore>=hv))
								}else{
									npreds<-0
								}
								
								tdf<-data.frame(hurdle=hh,numFPPreds=npreds,numFP=nfp)
								return(tdf)
							},fpmsdf=fpmsdf,lpsdf=lpsdf)
					
					speciesdf$SpeciesCode<-ss
					return(speciesdf)
				},hurdvals=hurdvals,lpdf=lpdf,fpmdf=fpmdf)
		modeldf$ModelName<-modName
		fpRatesList[[modName]]<-modeldf
	}
	return(fpRatesList)
}

## Same as above, but for the model in general, all species combined
getFPnums<-function(hurdvals,fpMatchesList,labelPredsList){
	fpRatesList<-list()
	for(mm in 1:3){
		fpmdf<-fpMatchesList[[mm]]
		lpdf<-labelPredsList[[mm]]
		modName<-unique(as.character(fpmdf$ModelName))
		modeldf<-ldply(hurdvals,function(hh,fpmdf,lpdf){
					hv<-hh*(10^7)
					if(nrow(fpmdf)>0){
						nfp<-nrow(subset(fpmdf,PredictionScore>=hv))
					}else{
						nfp<-nrow(subset(fpmdf,PredictionScore>=hv))
					}
					if(nrow(lpdf)>0){
						npreds<-nrow(subset(lpdf,PredictionScore>=hv))
					}else{
						npreds<-0
					}
					
					tdf<-data.frame(hurdle=hh,numFPPreds=npreds,numFP=nfp)
					return(tdf)
				},fpmdf=fpmdf,lpdf=lpdf)
		
		modeldf$ModelName<-modName
		fpRatesList[[modName]]<-modeldf
	}
	return(fpRatesList)
}


#####
## This function retrieves the number of TP and FN per species, model, and hurdle value
# hurdvals is the vector of hurdle values
# species is the vecor of species
# roimatches is the data.frame of results of ROI matches
getTP_FNnumsSp<-function(hurdvals,species,roimatches){
	tpfnRatesList<-list()
	for(mm in unique(roimatches$ModelName)){
		mdf<-subset(roimatches,ModelName==mm)
		modeldf<-ldply(species,function(ss,mdf,hurdvals){
					sdf<-subset(mdf,SpeciesCode==ss)
					speciesdf<-ldply(hurdvals,function(hh,sdf){
								hv<-hh*(10^7)
								hdf<-subset(mdf,PredictionScore >= hv)
								hdfbh<-subset(mdf,PredictionScore < hv)
								if(nrow(hdf)>0){
									numTP<-sum(hdf$match=="TRUEPOS")
									numFNh<-sum(hdf$match=="FALSENEG")
									numFNbh<-sum(hdfbh$match=="TRUEPOS")		#the TP that are now FN because of being below hurdle
									numFN<-numFNh + numFNbh
									numROIpreds<- nrow(hdf) + numFNbh
								}else{
									numTP<-0;numFN<-0;numROIpreds<-0
								}
								tdf<-data.frame(hurdle=hh,numTPPreds=numTP,numFNpreds=numFN, numTPFNpreds=numROIpreds)
								return(tdf)
							},sdf=sdf)
					speciesdf$SpeciesCode<-ss
					return(speciesdf)
				},mdf=mdf,hurdvals=hurdvals)
		modeldf$ModelName<-mm
		tpfnRatesList[[mm]]<-modeldf
	}
	return(tpfnRatesList)
}

## Same as the above function but for the model in general, all species combined
getTP_FNnums<-function(hurdvals,roimatches){
	tpfnRatesList<-list()
	for(mm in unique(roimatches$ModelName)){
		mdf<-subset(roimatches,ModelName==mm)
		modeldf<-ldply(hurdvals,function(hh,mdf){
					hv<-hh*(10^7)
					hdf<-subset(mdf,PredictionScore >= hv)
					hdfbh<-subset(mdf,PredictionScore < hv)
					numTP<-sum(hdf$match=="TRUEPOS")
					numFNh<-sum(hdf$match=="FALSENEG")
					numFNbh<-sum(hdfbh$match=="TRUEPOS")	#the TP that due to hurdle are now FN
					numFN<-numFNh+numFNbh
					numROIpreds<-numTP+numFN
					tdf<-data.frame(hurdle=hh,numTPPreds=numTP,numFNpreds=numFN, numTPFNpreds=numROIpreds)
					return(tdf)
				},mdf=mdf)
		modeldf$ModelName<-mm
		tpfnRatesList[[mm]]<-modeldf
	}
	return(tpfnRatesList)
}


#####
## A function that calculates the adjusted rates by species, model and hurdle, and then the metrics
# Metrics: 
# 	Sensitivity: TP/(TP+FN)
# 	Precision: TP/(TP+FP)
# 	F1: (2*Sensitivity*Precision)/(Sensitivity+Precision)
# 	Miss rate: FN/(FN+TP)
#	FPper: FP/(TP+FP)
#   Fbeta: (1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
# rdf is the data.frame of TP, FP, and FN rates from which to calculate the metrics
# hurdvals is as always the vector of hurdles to apply penalizations
# species is the vector of species
# beta is the value to calculate the modified F-value
getRateMetrics_bySpecies<-function(rdf,hurdvals,species,beta){
	modeldf<-ldply(species,function(ss,hurdvals,rdf){
				sdf<-subset(rdf,SpeciesCode==ss)
				speciesdf<-ldply(hurdvals,function(hh,sdf){
							hdf<-subset(sdf,hurdle==hh)
							if(nrow(hdf)>0){
								trv<-sum(hdf$tpRate+hdf$fpRate+hdf$fnRate)
								hdf$tpSpRate<-hdf$tpRate/trv
								hdf$fpSpRate<-hdf$fpRate/trv
								hdf$fnSpRate<-hdf$fnRate/trv
								sens<-hdf$tpRate/(hdf$tpRate+hdf$fnRate)
								prec<-hdf$tpRate/(hdf$tpRate+hdf$fpRate)
								f1val<-(2*sens*prec)/(sens+prec)
								miss<-hdf$fnRate/(hdf$tpRate+hdf$fnRate)
								fpper<-hdf$fpRate/(hdf$tpRate+hdf$fpRate)
								fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
								hurdledf<-data.frame(ModelName=hdf$ModelName,SpeciesCode=hdf$SpeciesCode,hurdle=hdf$hurdle, TP=hdf$tpSpRate, FP=hdf$fpSpRate, FN=hdf$fnSpRate,
										F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
							}else{
								hurdledf<-data.frame()
							}
							return(hurdledf)
						},sdf=sdf)
				return(speciesdf)
			},hurdvals=hurdvals,rdf=rdf)
	return(modeldf)	
}

## Same as above, but for the model in general, all species combined
getRateMetrics<-function(rdf,hurdvals,beta){
	modeldf<-ldply(hurdvals,function(hh,sdf){
				hdf<-subset(rdf,hurdle==hh)
				if(nrow(hdf)>0){
					trv<-sum(hdf$tpRate+hdf$fpRate+hdf$fnRate)
					hdf$tpRate<-hdf$tpRate/trv
					hdf$fpRate<-hdf$fpRate/trv
					hdf$fnRate<-hdf$fnRate/trv
					sens<-hdf$tpRate/(hdf$tpRate+hdf$fnRate)
					prec<-hdf$tpRate/(hdf$tpRate+hdf$fpRate)
					f1val<-(2*sens*prec)/(sens+prec)
					miss<-hdf$fnRate/(hdf$tpRate+hdf$fnRate)
					fpper<-hdf$fpRate/(hdf$tpRate+hdf$fpRate)
					fbeta<-(1+(beta^2))*((prec*sens)/(((beta^2)*prec) + sens))
					hurdledf<-data.frame(ModelName=hdf$ModelName,hurdle=hdf$hurdle, TP=hdf$tpRate, FP=hdf$fpRate, FN=hdf$fnRate,
							F1val=f1val,Sens=sens,Prec=prec,Miss=miss,FPper=fpper,Fbeta=fbeta,beta=beta)
				}else{
					hurdledf<-data.frame()
				}
				return(hurdledf)
			},sdf=sdf)
	return(modeldf)	
}


#####
## This function calculates the metrics by species or in general, given the FP, TP, and FN counts
# FPmdlList is the list of data.frames, one for each model, with the counts and number of tests for FP
# TPFNmdlList is the list of data.frames, one for each model, with the counts and number of tests for TP and FN
# hurdvals is the vector of hurdle values
# species is either NA (i.e., no summarization by species) or the vector of species to summarize by
# beta is the value to calculate the modified F-value, defaults to beta=0.5
getROImetrics<-function(FPmdlList,TPFNmdlList,hurdvals,species=NA,beta=0.5){
	resList<-llply(names(FPmdlList),function(mm,FPmdlList,TPFNmdlList,hurdvals,species,beta){
				fpm<-FPmdlList[[mm]]
				fpm$fpRate<-as.numeric(sapply(1:nrow(fpm),function(rr,fpm){
									npreds<-fpm[rr,"numFPPreds"]; nfp<-fpm[rr,"numFP"]
									fpspr<-ifelse(npreds>0,nfp/npreds,NA)
									return(fpspr)
								},fpm=fpm))
				
				tpfnm<-TPFNmdlList[[mm]]
				tpfnm$tpRate<-sapply(1:nrow(tpfnm),function(rr,tpfnm){
							npreds<-tpfnm[rr,"numTPFNpreds"]; ntp<-tpfnm[rr,"numTPPreds"]
							tpspr<-ifelse(npreds>0,ntp/npreds,NA)
							return(tpspr)
						},tpfnm=tpfnm)
				tpfnm$fnRate<-sapply(1:nrow(tpfnm),function(rr,tpfnm){
							npreds<-tpfnm[rr,"numTPFNpreds"]; nfn<-tpfnm[rr,"numFNpreds"]
							fnspr<-ifelse(npreds>0,nfn/npreds,NA)
							return(fnspr)
						},tpfnm=tpfnm)
				
				if(is.na(species[1])){
					rdf<-merge(fpm[,c("hurdle","ModelName","fpRate")],
							tpfnm[,c("hurdle","ModelName","tpRate","fnRate")],by=c("hurdle","ModelName"))
					modelres<-getRateMetrics(rdf=rdf,hurdvals=hurdvals,beta=beta)
				}else{
					rdf<-merge(fpm[,c("hurdle","SpeciesCode","ModelName","fpRate")],
							tpfnm[,c("hurdle","SpeciesCode","ModelName","tpRate","fnRate")],by=c("hurdle","SpeciesCode","ModelName"))
					modelres<-getRateMetrics_bySpecies(rdf=rdf,hurdvals=hurdvals,species=species,beta=beta)
				}
				
				return(modelres)
			},FPmdlList=FPmdlList,TPFNmdlList=TPFNmdlList,hurdvals=hurdvals,species=species,beta=beta)
	return(resList)
}


