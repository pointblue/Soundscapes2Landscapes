# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(googlesheets)
#register a googlesheet
gdat<-gs_url("https://docs.google.com/spreadsheets/d/1PBktjnO1whUUEWYdUUv-a80SaKR7WOg3S91iIc3kYS8/edit?usp=sharing")
#read its contents
gs_read(gdat)

library(XLConnect)
library(unmarked)
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"
dat<-try(readWorksheetFromFile(paste(pth,"WEME_classifications.xlsx",sep=""),sheet="WEME_04"))
dat<-dat[,-1]

ddat<-aggregate(as.formula("presence~site+year+month+day"),data=dat,FUN=max)
ndat<-aggregate(as.formula("presence~site+year+month+day"),data=dat,FUN=NROW);names(ndat)<-c("site","year","month","day","nrec")
tdf<-merge(ddat,ndat,by=c("site","year","month","day"))

tdf$date<-paste(tdf$year,tdf$month,ifelse(tdf$day<10,paste("0",tdf$day,sep=""),tdf$day),sep="")
#generate events by site
data<-data.frame()
for(ss in unique(tdf$site)){
	sdat<-subset(tdf,site==ss)
	sdat<-sdat[order(sdat$date),]
	sdat$event<-1:nrow(sdat)
	data<-rbind(data,sdat)
}

obs<-reshape(data[,c("presence","site","event")],idvar="site",timevar="event",direction="wide")
nrecs<-reshape(data[,c("nrec","site","event")],idvar="site",timevar="event",direction="wide")
months<-reshape(data[,c("month","site","event")],idvar="site",timevar="event",direction="wide")
#Way too many events per site. What to do? Ask Marconi...
