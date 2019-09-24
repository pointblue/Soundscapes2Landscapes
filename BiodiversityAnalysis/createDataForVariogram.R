# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## the purpose of this file is to generate indices of diversity for each survey in the Sonoma dataset 2006-2015
## With attribution to land cover class and lon/lat

## Dependencies:
libs<-c("raster","rgdal","rgeos","sp","plyr","ggplot2","raster")
lapply(libs,require,character.only = TRUE)

## Functions we will need:
reprojectPoints<-function(df,lonfield,latfield,fromProj,toProj){
	#Cannot have missing coordinates
	df<-subset(df,!is.na(df[,lonfield]) & !is.na(df[,latfield]))
	coordinates(df)<-c(lonfield,latfield)
	proj4string(df)<-CRS(fromProj)
	
	projdf<-spTransform(df,CRS(toProj))
	projdf<-as.data.frame(projdf)
	names(projdf)<-gsub(lonfield,"Easting",names(projdf))
	names(projdf)<-gsub(latfield,"Northing",names(projdf))
	return(projdf)
	
}

attributeVegClass<-function(df,vegmap){
	coordinates(df)<-c("Easting","Northing")
	proj4string(df)<-CRS(projection(vegmap))
	df$VegClass<-over(df,vegmap)
	dfattr<-as.data.frame(df)
	names(dfattr)<-gsub("Reclass","VegClass",names(dfattr))
	return(dfattr)
}

getDiversityIndices<-function(df){
	
	sum(eobs$ObservationCount,na.rm=TRUE)==nrow(eobs)
	
	#richness
	RichnessVal<-NROW(unique(df$SpeciesCode))
	
	## Prep data for other indices
	if(sum(eobs$ObservationCount,na.rm=TRUE)==nrow(eobs)){
		#do nothing, no abundance data
		ShannonVal<-NA; SimpsonVal<-NA
	}else if(sum(is.na(df$ObservationCount))==nrow(df)){
		#do nothing, also no abundance data
		ShannonVal<-NA; SimpsonVal<-NA
	}else{
		abund<-aggregate(ObservationCount~SpeciesCode,data=df,FUN=sum, na.rm=TRUE)
		if(nrow(abund)>0){
			tabund<-sum(abund$ObservationCount)
			
			#Shannon
			abund$pv<-abund$ObservationCount/tabund
			abund$Shannon<-(-1)*abund$pv*log(abund$pv)
			ShannonVal<-round(exp(sum(abund$Shannon)),2)
			
			#Simpson's evenness
			abund$pvsq<-abund$pv^2
			SimpsonVal<-round(1/sum(abund$pvsq),2)
		}else{
			ShannonVal<-NA; SimpsonVal<-NA
		}
		
	}
	
	
	divinds<-c(RichnessVal,ShannonVal,SimpsonVal)
	return(divinds)
}


## Load shapefile with land cover polygons
dsn<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/BiodiversityAnalysis/sonoma_veg_map_stratification_lifeforms_dissolved_190918"
vegmap<-readOGR(dsn,layer="sonoma_veg_map_stratification_lifeforms_dissolved_190918")
vegproj<-projection(vegmap)

## Reading the people survey data, filtering, reprojecting:
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/2006_2015_unmergedData.Rdata")
effort<-as.data.frame(effort)
effortLocs<-subset(effort,JulianDay > 73 & JulianDay< 181 & !ProtocolCode %in% c("Historical","eBird - Casual Observation"),
		select=c("ProjectCode","ProtocolCode","SamplingUnitId","JulianDay","DecimalLatitude","DecimalLongitude"))
survproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
effortLocsUTM<-reprojectPoints(df=effortLocs,lonfield="DecimalLongitude",latfield="DecimalLatitude",fromProj=survproj,toProj=vegproj)
effortdf<-attributeVegClass(df=effortLocsUTM,vegmap=vegmap)

## Loop through each event, find the matching set of observations and calculate diversity indices
obsdata$SpeciesCode<-as.character(obsdata$SpeciesCode)	#But using only identified taxa...
obsdata<-subset(obsdata,!grepl("XX",SpeciesCode,fixed=T))
obsdata<-subset(obsdata,!grepl("UN",SpeciesCode,fixed=T))

effortdf$ProjectCode<-as.character(effortdf$ProjectCode)
effortdf$ProtocolCode<-as.character(effortdf$ProtocolCode)
effortdf$SamplingUnitId<-as.character(effortdf$SamplingUnitId)

obsdata$ProjectCode<-as.character(obsdata$ProjectCode)
obsdata$ProtocolCode<-as.character(obsdata$ProtocolCode)
obsdata$SamplingUnitId<-as.character(obsdata$SamplingUnitId)
obsdata$ObservationCount<-as.numeric(as.character(obsdata$ObservationCount))

divdf<-data.frame()
for(i in 1:nrow(effortdf)){
	prjc<-effortdf[i,"ProjectCode"]; ptlc<-effortdf[i,"ProtocolCode"]; suid<-effortdf[i,"SamplingUnitId"]; jday<-effortdf[i,"JulianDay"]
	eutm<-effortdf[i,"Easting"]; nutm<-effortdf[i,"Northing"]; vgcl<-effortdf[i,"VegClass"]
	eobs<-subset(obsdata,ProjectCode==prjc & ProtocolCode==ptlc & SamplingUnitId==suid & JulianDay==jday)
	if(nrow(eobs)==0){
		#do nothing - no data
	}else{
		divind<-getDiversityIndices(eobs[,c("SpeciesCode","ObservationCount")])
		resdf<-data.frame(Easting=eutm,Northing=nutm,CoverClass=vgcl,Richness=divind[1],ExpShannon=divind[2],Simpson=divind[3])
		divdf<-rbind(divdf,resdf)
	}
}


nrow(effortdf)
nrow(divdf)

write.csv(divdf,file="c:/users/lsalas/git/Soundscapes2Landscapes/BiodiversityAnalysis/DiversityIndices.csv")


