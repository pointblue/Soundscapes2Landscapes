# TODO: Add comment
# 
# Author: lsalas
###############################################################################

libs<-c("RODBC","raster","plyr","data.table","rgdal")
lapply(libs, require, character.only = TRUE)
source("C:/Users/lsalas/git/sparklemotion/AKN/dataRequests/getDataUtils.R")

#using: +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
#ext<-c(-124.0229,-122.3157,38.09592,40.01872); names(ext)<-c("xmin","xmax","ymin","ymax") #sonoma, lake, mendo
ext<-c(-123.6325,-122.3475,38.07326,38.85292); names(ext)<-c("xmin","xmax","ymin","ymax") #sonoma only

conn<-odbcConnect("ravian_wh")
pcd<-getSQLforDataFromWH(tablename="ravianpointcountlevel3_v1",spcodes=NA,projectcodes=NA,years=c(2013,2014,2015),extent=ext,con=conn)
asd<-getSQLforDataFromWH(tablename="ravianareasearchlevel3_v1",spcodes=NA,projectcodes=NA,years=c(2013,2014,2015),extent=ext,con=conn)
bbd<-getSQLforDataFromWH(tablename="ravianbbsbase_v1",spcodes=NA,projectcodes=NA,years=c(2013,2014,2015),extent=ext,con=conn)
odbcClose(conn)

#Make it Oct2014 to Sep2015 - Spring and Summer
conn<-odbcConnect("ebird")
#ebd<-getSQLforEBirdData(tablename="ebd_usaonly",commname=NA,states="California",counties=c("Sonoma","Mendocino","Lake"),years=c(2014,2015),con=conn)	
ebd<-getSQLforEBirdData(tablename="ebd_usaonly",commname=NA,states="California",counties=c("Sonoma"),years=c(2013,2014,2015),con=conn)	
odbcClose(conn)

save(pcd,asd,bbd,ebd,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/rawData2013_15.Rdata")

#filter eBird for appropriate protocols and remove dupes
ebdc<-filterEBird(ebd,distlim=0.1, protocols=NA,approv=1,rev=0,allsp=1)

#################################################
#homogenize columns:
#generate effort table
efffields<-c("ProjectCode","ProtocolCode","SamplingUnitId","YearCollected","MonthCollected","DayCollected","JulianDay","DecimalLatitude","DecimalLongitude")
effebd<-c("PROJECT_CODE","PROTOCOL_TYPE","LOCALITY_ID","YearCollected","MonthCollected","DayCollected","JulianDay","LATITUDE","LONGITUDE")
effpc<-unique(pcd[,efffields,with=F]);effas<-unique(asd[,efffields,with=F]);effbb<-unique(bbd[,efffields,with=F])		#,with=F
effeb<-unique(ebdc[,effebd,with=F]);names(effeb)<-efffields
effort<-rbind(effpc,effas);effort<-rbind(effort,effbb);effort<-rbind(effort,effeb)

#attribute effort table with cell values
baseg<-raster("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/MODISrast/baserastDD.tif")
gdf<-effort[,c("DecimalLongitude","DecimalLatitude"),with=F];gdf$recid<-1:(nrow(effort))
coordinates(gdf)<-c("DecimalLongitude","DecimalLatitude")
proj4string(gdf) <- CRS("+proj=longlat +datum=WGS84") 
gdfp<-spTransform(gdf,CRS=projection(baseg))
cid<-extract(baseg,gdfp,cellnumbers=T,df=T)
effort[,cellId:=cid$cells,]

#attribute effort with county and filter
county<-readOGR("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/MODISrast/sonoma_lake_mendo_utm_wgs84","sonoma_lake_mendo_utm_wgs84")
gdfp<-spTransform(gdf,CRS=projection(county))
cnam<-over(gdfp,county)
effort[,county:=cnam$NAME,]
effort<-subset(effort,!is.na(county))
#remove the waterbird data
effort<-subset(effort,ProtocolCode!="WATERBIRD_COUNT")

#generate obs table
obsfields<-c(efffields,"CommonName","SpeciesCode","ObservationCount")
obsebd<-c(effebd,"COMMON_NAME","OBSERVATION_COUNT")
obspc<-pcd[,obsfields,with=F];obsas<-asd[,obsfields,with=F];obsbb<-bbd[,obsfields,with=F]
obs<-rbind(obspc,obsas);obs<-rbind(obs,obsbb);obs<-subset(obs,!is.na(SpeciesCode))
obseb<-ebdc[,obsebd,with=F];names(obseb)<-c(efffields,"CommonName","ObservationCount")

#add species code to obsebd - query from fieldbird_v2!
spp<-unique(obseb$CommonName)
spp<-subset(spp,grepl(" sp.",spp)==FALSE & grepl("/",spp)==FALSE & grepl(" spp.",spp)==FALSE  & grepl("Domestic",spp)==FALSE  & grepl(" x ",spp)==FALSE)
conn<-odbcConnect("prbodb")
spcddf<-getSpeciesCodes(spp=spp,con=conn)
odbcClose(conn)
NROW(spp)==nrow(spcddf)	#Not equal, but that's OK
tdf<-data.frame(CommonName=c("Canada Goose","Ridgway's Rail"),SpeciesCode=c("CANG","RIRA"))
spcddf<-rbind(spcddf,tdf)
obseb<-merge(obseb,spcddf,by="CommonName",all.x=T)
obseb<-subset(obseb,!is.na(SpeciesCode));obseb<-obseb[,names(obs),with=F]
obsdata<-rbind(obs,obseb)

#attribute obs with county and filter
gdf<-obsdata[,c("DecimalLongitude","DecimalLatitude"),with=F]
coordinates(gdf)<-c("DecimalLongitude","DecimalLatitude")
proj4string(gdf) <- CRS("+proj=longlat +datum=WGS84") 
gdfp<-spTransform(gdf,CRS=projection(county))
cnam<-over(gdfp,county)
obsdata[,county:=cnam$NAME,]

#attribute obs with cellId too, for summaries below
gdfp<-spTransform(gdf,CRS=projection(baseg))
cid<-extract(baseg,gdfp,cellnumbers=T,df=T)
obsdata[,cellId:=cid$cells,]
obsdata<-subset(obsdata,!is.na(county))
obsdata[,obsCount:=as.integer(as.character(ObservationCount)),]
obsdata[,obsCount:=ifelse(is.na(obsCount),1,obsCount),]
obsdata<-subset(obsdata,ProtocolCode %in% (unique(effort$ProtocolCode)))

#save(obsdata,effort,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/SonoMendoLake/unmergedData.Rdata")
save(obsdata,effort,file="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/S2Ldata/Sonoma/unmergedData.Rdata")

##Then what?
#report abundance by species??
dets<-aggregate(obsCount~SpeciesCode,data=obsdata,FUN=NROW);names(dets)<-c("SpeciesCode","NumDetections")
cnts<-aggregate(obsCount~SpeciesCode,data=obsdata,FUN=sum);names(cnts)<-c("SpeciesCode","TotalCount")
clls<-ldply(.data=unique(obsdata$SpeciesCode),.fun=function(x,df){
			scc<-NROW(unique(subset(df,SpeciesCode==x)$cellId));
			tdf<-data.frame(SpeciesCode=x,numCells=scc);
			return(tdf)
		},df=obsdata)
report<-merge(dets,cnts,by="SpeciesCode",all.x=T); report<-merge(report,clls,by="SpeciesCode",all.x=T)
report<-merge(report,spcddf,by="SpeciesCode",all.x=T)
