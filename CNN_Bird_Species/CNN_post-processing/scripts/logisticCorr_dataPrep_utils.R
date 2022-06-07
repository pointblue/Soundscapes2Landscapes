# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This function gets the AudiofileIds for all events in the table of matches submitted, plus the 3 prior and posterior events
## It identifies the event  of the ROI with isevent<-1
## returns a table with all the events to consider for data given the event of interest, plus their AudiofileIds
# lgevents is the list of events to find AudiofileId
# con is the RMySQL connection
getAudiofileIds<-function(lgevents,con){
	eventIddf<-ldply(lgevents,function(ee,con){
				site<-substr(ee,1,15)
				yr<-substr(ee,19,20)
				mn<-substr(ee,22,23)
				dy<-substr(ee,25,26)
				hr<-substr(ee,28,29)
				mt<-substr(ee,31,32)
				dt<-paste0("20",yr,"-",mn,"-",dy," ",hr,":",mt,":00")
				afq<-paste0("select AudiofileId from audiofiles where SiteName='",site,"' AND ",
						"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') <= DATE_ADD('",dt,"', INTERVAL 30 MINUTE) AND ",
						"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') >= DATE_SUB('",dt,"', INTERVAL 30 MINUTE)")
				afid<-dbGetQuery(con,afq)
				afe<-paste0("select AudiofileId from audiofiles where SiteName='",site,"' and Year=",yr," and Month=",mn," and Day=",dy," and Hour=",hr," and Minute=",mt)
				afeid<-dbGetQuery(con,afe)
				#covert into data.frame
				afid$SamplingEvent<-ee;afid$isevent<-ifelse(afid$AudiofileId==afeid$AudiofileId,1,0);afid$eventAfid<-afeid$AudiofileId
				return(afid)
			},con=con)
	return(eventIddf)
}

## This function retrieves the counts of the target species in the file and nearby files, and the counts of all other species
# targetdf is the target data.frame to fit corrections models to
# con is a connection to the MySQL database
# eventIddf is the table of AudiofileIds for all events of interest and for files +/- 30 minutes
# sppdf is a data.frame with a single column listing all species codes
# filter the predictions to consider as valid for counts
getLogCorrVars<-function(targetdf,con,eventIddf,sppdf,hurdval=0.65){
	logistdf<-ldply(1:nrow(targetdf),function(dd,targetdf,con,eventIddf,sppdf,hurdval){
				hv<-hurdval*10^7
				afv<-as.numeric(targetdf[dd,"AudiofileId"])
				pdid<-as.numeric(targetdf[dd,"PredictionsDatasetId"])
				spcd<-as.character(targetdf[dd,"SpeciesCode"])
				rdf<-targetdf[dd,c("PredictionId","Second","PredictionScore","SpeciesCode","SamplingEvent","AudiofileId","ModelName","PredictionsDatasetId","matchval")]
				
				# how many predictions in the same file
				afidvals<-subset(eventIddf,eventAfid==afv)$AudiofileId;afidvcs<-paste(afidvals,collapse=",")
				cdf<-dbGetQuery(con,paste0("select count(*) as count from predictions where AudiofileId = ",afv," and PredictionScore >= ",hv," and PredictionsDatasetId = ",pdid," and SpeciesCode = '",spcd,"'"))
				rdf$count<-as.numeric(cdf$count[1])
				
				# how many predictions in nearby files
				qdf<-dbGetQuery(con,paste0("select SpeciesCode,count(*) as Count from predictions where AudiofileId in (",afidvcs,") and PredictionScore >= ",hv," and PredictionsDatasetId = ",pdid," GROUP BY SpeciesCode"))
				sdf<-merge(sppdf,qdf,by="SpeciesCode",all.x=TRUE)
				sdf$Count<-ifelse(is.na(sdf$Count),0,sdf$Count)
				tsdf<-as.data.frame(t(sdf$Count));names(tsdf)<-sdf$SpeciesCode
				tsdf$rich<-sum(sdf$Count>0)
				tsdf$nearCount<-tsdf[1,spcd]
				resdf<-cbind(rdf,tsdf)
				return(resdf)
			},targetdf=targetdf,sppdf=sppdf,con=con,hurdval=hurdval,eventIddf=eventIddf)
	return(logistdf)
	
}

## This does the same as above but in chunks, thus not including the exact second of the prediction in the report
# No need to include prediction score or hurdle, because the data have already been hurdled
getLogCorrVarsChunk<-function(targetdf,con,eventIddf,sppdf){
	logistdf<-ldply(1:nrow(targetdf),function(dd,targetdf,con,eventIddf,sppdf){
				afv<-as.numeric(targetdf[dd,"AudiofileId"])
				pdid<-as.numeric(targetdf[dd,"PredictionsDatasetId"])
				spcd<-as.character(targetdf[dd,"SpeciesCode"])
				rdf<-targetdf[dd,c("SpeciesCode","SamplingEvent","AudiofileId","ModelName","PredictionsDatasetId","matchval")]
				
				# how many predictions in the same file
				afidvals<-subset(eventIddf,eventAfid==afv)$AudiofileId;afidvcs<-paste(afidvals,collapse=",")
				cdf<-dbGetQuery(con,paste0("select count(*) as count from predictions where AudiofileId = ",afv,
								" and PredictionsDatasetId = ",pdid," and SpeciesCode = '",spcd,"'"))
				rdf$count<-as.numeric(cdf$count[1])
				
				# how many predictions in nearby files
				qdf<-dbGetQuery(con,paste0("select SpeciesCode,count(*) as Count from predictions where AudiofileId in (",afidvcs,")",
								" and PredictionsDatasetId = ",pdid," GROUP BY SpeciesCode"))
				sdf<-merge(sppdf,qdf,by="SpeciesCode",all.x=TRUE)
				sdf$Count<-ifelse(is.na(sdf$Count),0,sdf$Count)
				tsdf<-as.data.frame(t(sdf$Count));names(tsdf)<-sdf$SpeciesCode
				tsdf$rich<-sum(sdf$Count>0)
				tsdf$nearCount<-tsdf[1,spcd]
				resdf<-cbind(rdf,tsdf)
				return(resdf)
			},targetdf=targetdf,sppdf=sppdf,con=con,eventIddf=eventIddf)
	return(logistdf)
	
}


## Function to request all parameters in Doug's code from table audiofiles given AudiofileId
# afid is the AudiofileId
getAudiofilesFromAfid<-function(con,afid){
	adifdf<-dbGetQuery(con,paste("select * from audiofiles where AudiofileId =",afid))
	dt<-paste0("20",adifdf$Year[1],"-",adifdf$Month[1],"-",adifdf$Day[1]," ",adifdf$Hour[1],":",adifdf$Minute[1],":00")
	afq<-paste0("select AudiofileId from audiofiles where SiteName='",adifdf$SiteName[1],"' AND ",
			"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') <= DATE_ADD('",dt,"', INTERVAL 30 MINUTE) AND ",
			"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') >= DATE_SUB('",dt,"', INTERVAL 30 MINUTE)")
	resafid<-dbGetQuery(con,afq)
	afe<-paste0("select AudiofileId from audiofiles where SiteName='",adifdf$SiteName[1],"' and Year=",adifdf$Year[1]," and Month=",adifdf$Month[1],
			" and Day=",adifdf$Day[1]," and Hour=",adifdf$Hour[1]," and Minute=",adifdf$Minute[1])
	afeid<-dbGetQuery(con,afe)
	#covert into data.frame
	resafid$SamplingEvent<-paste0(adifdf$SiteName[1],"_","20",adifdf$Year[1],"-",adifdf$Month[1],"-",adifdf$Day[1],"_",adifdf$Hour[1],"-",adifdf$Minute[1])
	resafid$isevent<-ifelse(resafid$AudiofileId==afeid$AudiofileId,1,0);resafid$eventAfid<-afeid$AudiofileId
	return(resafid)
}

## This does the same as above but for a vector of afids
getAudiofilesFromAfidVector<-function(con,afidv){
	
	afidt<-paste(afidv,collapse=",")
	adifdf<-dbGetQuery(con,paste0("select * from audiofiles where AudiofileId in (",afidt,")"))
	adifdf$Hour<-ifelse(adifdf$Hour==0,"00",adifdf$Hour)
	adifdf$Minute<-ifelse(adifdf$Minute==0,"00",adifdf$Minute)
	adifdf$dt<-paste0("20",adifdf$Year,"-",adifdf$Month,"-",adifdf$Day," ",adifdf$Hour,":",adifdf$Minute,":00")
	adifdf$afq<-paste0("select AudiofileId from audiofiles where SiteName='",adifdf$SiteName,"' AND ",
			"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') <= DATE_ADD('",adifdf$dt,"', INTERVAL 30 MINUTE) AND ",
			"CONCAT('20',Year, '-', LPAD(Month, 2, '0'), '-', LPAD(Day, 2, '0'), ' ', LPAD(Hour, 2, '0'), ':', LPAD(Minute, 2, '0'), ':00') >= DATE_SUB('",adifdf$dt,"', INTERVAL 30 MINUTE)")
	
	resafiddf<-ldply(1:nrow(adifdf),function(rr,adifdf,con){	
				afqv<-adifdf$afq[rr]
				resafid<-dbGetQuery(con,afqv)
				afe<-paste0("select AudiofileId from audiofiles where SiteName='",adifdf$SiteName[rr],"' and Year=",adifdf$Year[rr],
						" and Month=",adifdf$Month[rr]," and Day=",adifdf$Day[rr],
						" and Hour=",as.numeric(adifdf$Hour[rr])," and Minute=",as.numeric(adifdf$Minute[rr]))
				afeid<-dbGetQuery(con,afe)
				#covert into data.frame
				resafid$SamplingEvent<-paste0(adifdf$SiteName[rr],"_","20",adifdf$Year[rr],"-",adifdf$Month[rr],"-",adifdf$Day[rr],
						"_",adifdf$Hour[rr],"-",adifdf$Minute[rr])
				resafid$isevent<-ifelse(resafid$AudiofileId==afeid$AudiofileId,1,0);resafid$eventAfid<-afeid$AudiofileId
				return(resafid)
	},adifdf=adifdf,con=con)
	return(resafiddf)
}
