# TODO: Figure out how to use googlesheets
# 
# Author: lsalas
###############################################################################


library(googlesheets)
#register a googlesheet
gdat<-gs_url("https://docs.google.com/spreadsheets/d/1PBktjnO1whUUEWYdUUv-a80SaKR7WOg3S91iIc3kYS8/edit?usp=sharing")
#read its contents
gs_read(gdat)

#################################################################################
# Dependencies
library(unmarked)
library(raster)
library(rgdal)
library(sp)
library(rgeos)

lcpth<-"V:/Data/vegetation/SonomaCountyVegHabitat"
dsn<-"Sonoma_Veg_Map_5_1"
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

# custom functions  #############################################################
#counts the number of recordings at this site that were made during the dawn chorus
# x is the vector from which to count - using the "aggregate" function, so a vector of hours records for a site+year+... combo
# hrfld is a string naming the hours field in x, defaulting to "hour"
# sth and edh are the start and end hours
countDawnRecs<-function(x,sth=5,edh=9){
	ndc<-sum(x %in% c(sth:edh))
	return(ndc)
}

dat<-try(read.csv(paste(pth,"weme_all_recordings_(9-3-17).csv",sep=""))); if(inherits(dat,"try-error"))stop("Could not read data")
geo<-try(read.csv(paste(pth,"s2l_points_metadata_170925.csv",sep=""))); if(inherits(geo,"try-error"))stop("Could not read geodata")
geo<-geo[,c("guid","Lat","Long","UTM_Easting","UTM_Northing")];names(geo)<-c("site","Lat","Long","Easting","Northing")
geo$site<-trimws(geo$site)


ddat<-aggregate(as.formula("presence~site+year+month+day"),data=dat,FUN=max)
ndat<-aggregate(as.formula("presence~site+year+month+day"),data=dat,FUN=NROW);names(ndat)<-c("site","year","month","day","nrec")
cdat<-aggregate(as.formula("hour~site+year+month+day"),data=dat,FUN=countDawnRecs);names(cdat)<-c("site","year","month","day","dawn")
tdf<-merge(ddat,ndat,by=c("site","year","month","day"));tdf<-merge(tdf,cdat,by=c("site","year","month","day"))
tdf$site<-gsub("_a","",tdf$site)
tdf<-subset(tdf,!grepl("longterm",site))

tdf$date<-paste(tdf$year,tdf$month,ifelse(tdf$day<10,paste("0",tdf$day,sep=""),tdf$day),sep="")
#generate events by site
data<-data.frame()
for(ss in unique(tdf$site)){
	sdat<-subset(tdf,site==ss)
	sdat<-sdat[order(sdat$date),]
	sdat$event<-1:nrow(sdat)
	data<-rbind(data,sdat)
}

obs<-reshape(data[,c("presence","site","event")],idvar="site",timevar="event",direction="wide");names(obs)<-c("site",paste("event",1:(ncol(obs)-1),sep=":"))
nrecs<-reshape(data[,c("nrec","site","event")],idvar="site",timevar="event",direction="wide");names(nrecs)<-c("site",paste("nrecs",1:(ncol(obs)-1),sep=":"))
months<-reshape(data[,c("month","site","event")],idvar="site",timevar="event",direction="wide");names(months)<-c("site",paste("month",1:(ncol(obs)-1),sep=":"))
ndawn<-reshape(data[,c("dawn","site","event")],idvar="site",timevar="event",direction="wide");names(ndawn)<-c("site",paste("ndawn",1:(ncol(obs)-1),sep=":"))

#The data has 8 events, but not all sites were surveyed 8 days. The following code shows how many sites were surveyed N days:
apply(obs,2,function(x){y<-sum(!is.na(x));return(y)})
#All sites were surveyed at least 4 days. Only 50 (about half) were surveyed 5 days. Then 6 sites surveyed 6 days. So, omitting survey events 6-8
obs<-obs[,c("site",paste("event",1:5,sep=":"))]
nrecs<-nrecs[,c("site",paste("nrecs",1:5,sep=":"))]
months<-months[,c("site",paste("month",1:5,sep=":"))]
ndawn<-ndawn[,c("site",paste("ndawn",1:5,sep=":"))]


#For now: create the UDF and do an int-only occu + det model
alldata<-merge(obs,nrecs,by="site");alldata<-merge(alldata,months,by="site");alldata<-merge(alldata,ndawn,by="site")
y<-alldata[,paste("event",1:5,sep=":")]
scov<-data.frame(site=alldata$site)
ocovs<-list(
		nrecs=alldata[,paste("nrecs",1:5,sep=":")],
		months=alldata[,paste("month",1:5,sep=":")],
		ndawn=alldata[,paste("ndawn",1:5,sep=":")]
)

udf<-unmarkedFrameOccu(y=y,siteCovs=scov, obsCovs=ocovs)  #det.dat

mdl<-occu(formula=as.formula("~nrecs+months+ndawn ~1"),data=udf)
summary(mdl)
re<-ranef(mdl)
preds<-bup(re,"mean")

#attribute with land cover type
vegshp<-readOGR(lcpth,dsn)

