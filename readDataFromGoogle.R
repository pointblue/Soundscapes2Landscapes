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
dat$event<-paste(dat$year,dat$month,ifelse(dat$day<10,paste("0",dat$day,sep=""),dat$day),"::",
		ifelse(dat$hour<10,paste("0",dat$hour,sep=""),dat$hour),":",ifelse(dat$minute<10,paste("0",dat$minute,sep=""),dat$minute),sep="")

obs<-reshape(dat[,c("presence","site","event")],idvar="site",timevar="event",direction="wide")

#Way too many events per site. What to do? Ask Marconi...
