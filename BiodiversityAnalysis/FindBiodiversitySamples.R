# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## The purpose of this file is to find the people survey events that can be associated with each soundscape survey point 
## Once identified, these people surveys can be used to attribute the soundscape survey points with values of biodiversity indices
## We are doing this so as to then relate bioacoustic indices of sound diversity to biodiversity estimates from the people surves

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

getBinnedSurveyDistances<-function(pid,ssdf,sedf,vclass){
	pea<-subset(ssdf,PointID==pid)$Easting; pno<-subset(ssdf,PointID==pid)$Northing
	sedf$distToSoundPoint<-round(sqrt(((sedf$Easting-pea)^2)+((sedf$Northing-pno)^2)))
	b250<-sum(sedf$distToSoundPoint<251)
	b500<-sum(sedf$distToSoundPoint>250 & sedf$distToSoundPoint<501)
	b1000<-sum(sedf$distToSoundPoint>500 & sedf$distToSoundPoint<1001)
	b2500<-sum(sedf$distToSoundPoint>1000 & sedf$distToSoundPoint<2501)
	b5000<-sum(sedf$distToSoundPoint>2500 & sedf$distToSoundPoint<5001)
	bOver<-sum(sedf$distToSoundPoint>5000)
	resdf<-data.frame(PointID=rep(pid,times=6),VegClass=rep(vclass,times=6),
			DistBin=c("UpTo250","UpTo500","UpTo1000","UpTo2500","UpTo5000","Over5K"),
			BinValue=c(b250,b500,b1000,b2500,b5000,bOver))
	return(resdf)
}

###################
## STEP 1: Profiling distances of people survey events to sound survey points
## The goal is to find a cut-off distance such that each sound survey point has at least 5 related nearest people survey events
## Pseudo-code:
## Attribute the soundscape points with values of category of landscape cover
## Attribute people survey locations with values of category of landscape cover
## For each point in each category, bin the distances of all people survey events in that category

## Load shapefile with land cover polygons
dsn<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/BiodiversityAnalysis/sonoma_veg_map_stratification_lifeforms_dissolved_190918"
vegmap<-readOGR(dsn,layer="sonoma_veg_map_stratification_lifeforms_dissolved_190918")
vegproj<-projection(vegmap)

## Reading survey point locations, reprojecting:
soundLocs<-read.csv("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/SoundscapeSurveyLocations_190904.csv")
soundLocs<-soundLocs[,c("PointID","Latitude","Longitude")]
survproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
soundLocsUTM<-reprojectPoints(df=soundLocs,lonfield="Longitude",latfield="Latitude",fromProj=survproj,toProj=vegproj) 
sounddf<-attributeVegClass(df=soundLocsUTM,vegmap=vegmap)

## Reading the people survey data, filtering, reprojecting:
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/2006_2015_unmergedData.Rdata")
effort<-as.data.frame(effort)
effortLocs<-subset(effort,JulianDay > 73 & JulianDay< 181 & !ProtocolCode %in% c("Historical","eBird - Casual Observation"),
		select=c("ProjectCode","ProtocolCode","SamplingUnitId","JulianDay","DecimalLatitude","DecimalLongitude"))
effortLocsUTM<-reprojectPoints(df=effortLocs,lonfield="DecimalLongitude",latfield="DecimalLatitude",fromProj=survproj,toProj=vegproj)
effortdf<-attributeVegClass(df=effortLocsUTM,vegmap=vegmap)

## Ready to do the matching...

soundmatch<-ldply(.data=as.character(unique(sounddf$VegClass)),.fun=function(vc,sdf,edf){
			ssdf<-subset(sdf,VegClass==vc);sedf<-subset(edf,VegClass==vc)
			# loop through each sound survey point and calculate distance (binned) to each people survey - put results as data.frame
			# rbind these into a larger data.frame to return from the ldply
			pdf<-data.frame()
			for(pid in unique(ssdf$PointID)){
				tdf<-getBinnedSurveyDistances(pid=pid,ssdf=ssdf,sedf=sedf,vclass=vc)
				pdf<-rbind(pdf,tdf)
			}
			
			return(pdf)
		},sdf=sounddf,edf=effortdf)

## Visualizing...
for(vc in unique(soundmatch$VegClass)){
	tdf<-subset(soundmatch,VegClass==vc & ((DistBin=="UpTo250" & BinValue>0) | (DistBin=="UpTo500" & BinValue>0) | (DistBin=="UpTo1000" & BinValue>0)))
	if(nrow(tdf)>0){
		totPoints<-NROW(unique(subset(soundmatch,VegClass==vc)$PointID))
		selPoints<-NROW(unique(tdf$PointID))
		p<-ggplot(data=tdf,	aes(x=PointID,y=BinValue)) + geom_jitter(width = 0.1,size=1.5, aes(color=DistBin)) + 
			labs(x="PointID",y="Number of matching surveys",title=paste0("Class: ",vc," Total: ",totPoints,"  Selected: ",selPoints)) + coord_flip() 
		dev.new();print(p)
	}
}


