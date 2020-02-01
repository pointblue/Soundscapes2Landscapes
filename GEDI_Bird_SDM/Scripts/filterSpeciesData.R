# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file takes the harvested data and does two things with it:
## 1) Reattributes with cellId for 250, 500, and 1000m grids - we really don't need these, but just in case
## 3) Formats the data and saves individual udf formats for each of the target species

## The results are found in the GitHub repository here: https://github.com/leosalas/Soundscapes2Landscapes/tree/master/GEDI_Bird_SDM/Data/Birds/UDF

libs<-c("raster","unmarked","data.table","XLConnect","plyr")
lapply(libs,require,character.only = TRUE)
svpth<-"C:/temp/data/Birds/UDF/"

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
	df$ObservationCount<-ifelse(is.na(df$ObservationCount),0,df$ObservationCount)
	return(df)
}

## Must first download the data from the GitHub repository
# The data file is here: https://github.com/leosalas/Soundscapes2Landscapes/tree/master/GEDI_Bird_SDM/Data/2006_2015_unmergedData.Rdata
load(file="c:/temp/2006_2015_unmergedData.Rdata")
effort<-effort[,cellId:=NULL]


## Download and then read the grids
# The grids are found here: https://github.com/leosalas/Soundscapes2Landscapes/tree/master/GEDI_Bird_SDM/Data/DEM
# you must download the entire folder of files for each resolution, even though the code below points to just one file. The raster package needs the other files too.
g250<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM/250M/dem_250m.tif")
g500<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM/500M/dem_500m.tif")
g1000<-raster("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/DEM/1000M/dem_1000m.tif")

## 1) reattribute with cellIds
effort<-getCellId(df=effort,rast=g250,rez="250")
effort<-getCellId(df=effort,rast=g500,rez="500")
effort<-getCellId(df=effort,rast=g1000,rez="1000")

## 2) Make the udfs 
# First must download the file S2L_Sonoma_BirdSpecies.xlsx. It is found here: https://github.com/leosalas/Soundscapes2Landscapes/tree/master/GEDI_Bird_SDM/Data/S2L_Sonoma_BirdSpecies.xlsx
spdf<-try(readWorksheetFromFile("c:/temp/S2L_Sonoma_BirdSpecies.xlsx",sheet="Final"))
species<-spdf$SpeciesCode; 
seIDflds<-c("ProjectCode","ProtocolCode","SamplingUnitId","YearCollected","MonthCollected","DayCollected")
spflds<-c("CommonName","SpeciesCode","ObservationCount")

# loop through each resolution and each species to construct the udf 
rezz<-c("gId250","gId500","gId1000")

lapply(species,FUN=function(ss,spdf,effort,obsdata,seIDflds,rezz){
			comname<-subset(spdf,SpeciesCode==ss)$CommonName
			sttmo<-subset(spdf,SpeciesCode==ss)$sttmo; endmo<-subset(spdf,SpeciesCode==ss)$endmo;
			speff<-subset(effort,MonthCollected>=sttmo & MonthCollected<=endmo)
			spobs<-subset(obsdata, SpeciesCode==ss)	#& MonthCollected>=sttmo & MonthCollected<=endmo
			spobs$SpeciesCode<-as.character(spobs$SpeciesCode)
			spdat<-merge(speff,spobs[,c(seIDflds,spflds),with=FALSE],by=seIDflds,all.x=T)
			spdat<-fillBlanks(df=spdat,ss=ss,comname=comname)
			spdat<-spdat[,presence:=ifelse(ObservationCount>0,1,0)]; spdat$presence<-as.integer(as.numeric(spdat$presence))
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
			svfile<-paste0(svpth,ss,"_2006_2015.RData")
			save(datalst,file=svfile)
			print(paste("Done with",ss))
		},spdf=spdf,effort=effort,obsdata=obsdata,seIDflds=seIDflds,rezz=rezz)


##########################################################################


## Checks... Do not run
fls<-list.files(svpth,pattern="2006_2015")
spp<-substr(fls,1,4)
res<-data.frame()
for(ss in spp){
	load(paste0(svpth,ss,".RData"))
	old250<-nrow(datalst[["gId250"]])
	old500<-nrow(datalst[["gId500"]])
	old1000<-nrow(datalst[["gId1000"]])
	load(paste0(svpth,ss,"_2013_2015.RData"))
	new250<-nrow(datalst[["gId250"]])
	new500<-nrow(datalst[["gId500"]])
	new1000<-nrow(datalst[["gId1000"]])
	tdf<-data.frame(species=ss,old250=old250,new250=new250,old500=old500,new500=new500,old1000=old1000,new1000=new1000)
	res<-rbind(res,tdf)
}

det<-data.frame()
for(ss in spp){
	load(paste0(svpth,ss,".RData"))
	old250<-sum(datalst[["gId250"]]$presence>0)
	old500<-sum(datalst[["gId500"]]$presence>0)
	old1000<-sum(datalst[["gId1000"]]$presence>0)
	load(paste0(svpth,ss,"_2013_2015.RData"))
	new250<-sum(datalst[["gId250"]]$presence>0)
	new500<-sum(datalst[["gId500"]]$presence>0)
	new1000<-sum(datalst[["gId1000"]]$presence>0)
	tdf<-data.frame(species=ss,old250=old250,new250=new250,old500=old500,new500=new500,old1000=old1000,new1000=new1000)
	det<-rbind(det,tdf)
}






