# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file takes the harvested data and does two things with it:
## 1) Reattributes with cellId for 250, 500, and 1000m grids - we really don't need these, but just in case
## 3) Formats the data and saves individual udf formats for each of the target species

library("raster");library("unmarked");library("data.table");library(XLConnect)

## This function retrieves the cellId for the cell within which each observation was made, for a given raster
# df is the table of records
# rast is the raster from which to obtain cellId values
# rez is a string indicating the resolution of the raster, and thus names the raster itself. Possible values are 200, 500, and 1000
getCellId<-function(df,rast,rez){
	gdf<-df[,c("DecimalLongitude","DecimalLatitude"),with=F]
	coordinates(gdf)<-c("DecimalLongitude","DecimalLatitude")
	proj4string(gdf) <- CRS("+proj=longlat +datum=WGS84") 
	gdfp<-spTransform(gdf,CRS=crs(rast))
	
	cid<-extract(rast,gdfp,cellnumbers=T,df=T)
	cidnam<-paste0("gId",rez)
	df[,cidnam]<-cid$cells
	
	return(df)
}

## This function completes the 0's for surveys where the species was not detected
# df is the table of records
# ss is the species code
# comname is the common name of the species
fillBlanks<-function(df,ss,comname){
	df$CommonName<-comname
	df$SpeciesCode<-ss
	df$obsCount<-ifelse(is.na(df$obsCount),0,df$obsCount)
	return(df)
}

## data here: effort and obsdata
load(file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/unmergedData.Rdata")

## load the 250, 500 and 1000 grids - remove cellId from the effort table and add these grids' cellIds
effort<-effort[,cellId:=NULL]
obsdata<-obsdata[,c("cellId","JulianDay","DecimalLatitude","DecimalLongitude","county","ObservationCount"):=NULL]; 
# grids here
g250<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM_Rescaled/250M/dem_clip_250m.tif")
g500<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM_Rescaled/500M/dem_clip_500m.tif")
g1000<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM_Rescaled/1000M/dem_clip_1000m.tif")

## 1) reattribute with cellIds
effort<-getCellId(df=effort,rast=g250,rez="250")
effort<-getCellId(df=effort,rast=g500,rez="500")
effort<-getCellId(df=effort,rast=g1000,rez="1000")

## 2) Make the udfs
spdf<-try(readWorksheetFromFile("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/S2L_Sonoma_only_Species_DLadds.xlsx",sheet="Final"))
species<-spdf$SpeciesCode; 
seIDflds<-c("ProjectCode","ProtocolCode","SamplingUnitId","YearCollected","MonthCollected","DayCollected")
# loop through each resolution and each species to construct the udf HERE

rezz<-c("gId250","gId500","gId1000")

# need to filter each species for breeding range
for(ss in species){
	comname<-subset(spdf,SpeciesCode==ss)$CommonName
	sttmo<-subset(spdf,SpeciesCode==ss)$sttmo; endmo<-subset(spdf,SpeciesCode==ss)$endmo;
	speff<-subset(effort,MonthCollected>=sttmo & MonthCollected<=endmo)
	spobs<-subset(obsdata, SpeciesCode==ss & MonthCollected>=sttmo & MonthCollected<=endmo)
	spobs$SpeciesCode<-as.character(spobs$SpeciesCode)
	spdat<-merge(speff,spobs,by=seIDflds,all.x=T)
	spdat<-fillBlanks(df=spdat,ss=ss,comname=comname)
	spdat<-spdat[,presence:=ifelse(obsCount>0,1,0)]; spdat$presence<-as.integer(as.numeric(spdat$presence))
	datalst<-list()
	for(zz in rezz){
		dfa<-aggregate(as.formula(paste0("presence~SpeciesCode+",zz)),data=spdat,FUN=function(x){
					y<-ifelse(sum(x)>0,1,0);return(y)
				})
		dfa$Resolution<-paste0(substr(zz,4,7),"M")
		names(dfa)<-gsub(zz,"CellId",names(dfa))
		dfb<-aggregate(as.formula(paste0("presence~",zz)),data=spdat,FUN=sum,na.rm=T)
		names(dfb)<-gsub(zz,"CellId",names(dfb))
		names(dfb)<-gsub("presence","NumDet",names(dfb))
		df<-merge(dfa,dfb,by="CellId",all.x=T)
		datalst[[zz]]<-df
	}
	svfile<-paste0("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/Birds/UDF/",ss,".RData")
	#svfile<-paste0("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/birdData/UDF/",ss,".RData")
	save(datalst,file=svfile)
	print(paste("Done with",ss))
	
	##
	#datalst<-list()
	#rezz<-c("gId250","gId500","gId1000")
	#for(zz in rezz){
	#	cid<-unique(spdat[,zz,with=FALSE])
	#	flatlst<-list(); i<-0
	#	for(nc in 1:nrow(cid)){
	#		cc<-cid[nc]
	#		if(zz=="gId250"){tdf<-spdat[gId250==cc]}else if(zz=="gId500"){tdf<-spdat[gId500==cc]}else{tdf<-spdat[gId1000==cc]}
	#		tdf<-tdf[,Resolution:=zz]
	#		if(nrow(tdf)>1){
	#			tdf<-tdf[1:10,];tdf$SpeciesCode<-ss
	#			tdf<-tdf[,visit:=1:10]
	#			presshp<-reshape(tdf[,c(zz,"SpeciesCode","Resolution","visit","presence"),with=F],idvar=c(zz,"SpeciesCode","Resolution"),timevar="visit",direction="w")
	#			presshp<-subset(presshp,!is.na(Resolution))
	#			names(presshp)<-gsub("presence.","survey_",names(presshp))
	#			names(presshp)<-gsub(zz,"cellId",names(presshp))
	#			jdayshp<-reshape(tdf[,c(zz,"Resolution","visit","JulianDay"),with=F],idvar=c(zz,"Resolution"),timevar="visit",direction="w")
	#			jdayshp<-subset(jdayshp,!is.na(Resolution))
	#			names(jdayshp)<-gsub("presence.","jday_",names(jdayshp))
	#			names(jdayshp)<-gsub(zz,"cellId",names(jdayshp))
	#			tmp<-merge(presshp,jdayshp,by=c("cellId","Resolution"))
	#			i<-i+1;flatlst[[i]]<-tmp
	#		}
	#	}
	#	flatdf<-rbindlist(flatlst)
	#	presfls<-subset(names(flatdf),grepl("survey_",names(flatdf)));jdayfls<-subset(names(flatdf),grepl("JulianDay.",names(flatdf)))
	#	udf<-unmarkedFrameOccu(y=as.data.frame(flatdf[,presfls,with=F]),siteCovs=as.data.frame(flatdf[,c("cellId","Resolution","SpeciesCode"),with=F]),obsCovs=list(jday=as.data.frame(flatdf[,jdayfls,with=F])))
	#	datalst[[zz]]<-udf
	#}
	#svfile<-paste0("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/birdData/UDF/",ss,".RData")
	#save(datalst,file=svfile)
	#print(paste("Done with",ss))
}


