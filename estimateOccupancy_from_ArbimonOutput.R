# TODO: Figure out how to use googlesheets
# 
# Author: lsalas
###############################################################################


#library(googlesheets)
#register a googlesheet
#gdat<-gs_url("https://docs.google.com/spreadsheets/d/1PBktjnO1whUUEWYdUUv-a80SaKR7WOg3S91iIc3kYS8/edit?usp=sharing")
#read its contents
#gs_read(gdat)

#################################################################################
# Dependencies
libs<-c("raster","rgdal","sp","rgeos","unmarked")
lapply(libs, require, character.only = TRUE)


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

udf<-unmarkedFrameOccuFP(y=y,siteCovs=scov, obsCovs=ocovs,type=c(0,5,0))  #det.dat
df<-cbind(scov,y)
df$max<-apply(df[,c(2:6)],1,max,na.rm=T)
df$sum<-apply(df[,c(2:6)],1,sum,na.rm=T)
df$nev<-apply(df[,c(2:6)],1,FUN=function(x){y<-sum(!is.na(x));return(y)})
df$presence<-df$sum/df$nev

#estimate prevalence
prev<-numeric()
for (ss in unique(dat$site)){
	tdf<-subset(dat,site==ss)
	nr<-nrow(tdf);nc<-sum(tdf$presence)
	prev<-c(prev,nc/nr)
}
mean(prev)

mdl1<-occuFP(detformula=as.formula("~as.factor(months)+ndawn"), FPformula=as.formula("~nrecs"), data=udf);summary(mdl1)
#mdl2<-occuFP(detformula=as.formula("~as.factor(months)+ndawn+I(ndawn^2)"), FPformula=as.formula("~nrecs"), data=udf);summary(mdl2)
#mdl3<-occuFP(detformula=as.formula("~as.factor(months)+ndawn"), FPformula=as.formula("~nrecs+I(nrecs^2)"), data=udf);summary(mdl3)
#mdl4<-occuFP(detformula=as.formula("~as.factor(months)+ndawn+I(ndawn^2)"), FPformula=as.formula("~nrecs+I(nrecs^2)"), data=udf);summary(mdl4)
#mdl1 is best
#mdl1a<-occuFP(detformula=as.formula("~as.factor(months)+ndawn+nrecs"), data=udf);summary(mdl1a)
#mdl1b<-occuFP(detformula=as.formula("~as.factor(months)+nrecs"), FPformula=as.formula("~ndawn"), data=udf);summary(mdl1b)
#mdl1c<-occuFP(detformula=as.formula("~as.factor(months)+ndawn+nrecs"), FPformula=as.formula("~nrecs+ndawn"), data=udf);summary(mdl1c)	#possibly competing
#mdl1 still best - different iterations of the below did not work
#mdl1d<-occuFP(detformula=as.formula("~ndawn+nrecs"), FPformula=as.formula("~as.factor(months)"), data=udf);summary(mdl1d)
#try monthly slopes...
#mdl1e<-occuFP(detformula=as.formula("~as.factor(months)*ndawn"), FPformula=as.formula("~nrecs"), data=udf);summary(mdl1e)
#mdl1 rules!!

##predict to month=5, and the mean ndawn and nrecs per site - TEST
cvals<-coef(mdl1)
#to predict:
#estimate occu as est.occu * (1-FP) and pdet is not needed because the coefficient for PSI is already corrected for pdet
#est.occu: psi 
estOc<-exp(cvals[1])/(1+exp(cvals[1])) 
#FP = (intpf+ mean(nrecs)*cFP)
lgfp<-(cvals[7] + (110*cvals[8]))
fp<-exp(lgfp)/(1+exp(lgfp))
nfp<-1-fp
op<-estOc * nfp 

#vectorizing
dfcovs<-alldata[,c(1,7:11)]
dfcovs$mean_nrecs<-apply(dfcovs[,c(2:6)],1,mean,na.rm=T)
dfcovs$lgfp<-cvals[7] + (dfcovs$mean_nrecs*cvals[8])
dfcovs$fp<-exp(dfcovs$lgfp)/(1+exp(dfcovs$lgfp))
dfcovs$est<-estOc*(1-dfcovs$fp)
dfcovs$lgt_est<-log(dfcovs$est)/(1-log(dfcovs$est))

results<-dfcovs[,c("site","fp","est","lgt_est")]
#Need to change the guid of 9 sites
results$site<-ifelse(results$site=="s2l01_170509_028390028_1432","s2l01_170509_028390028_1423",
		ifelse(results$site=="s2l03_170415_028150053_1423","s2l03_170415_028150053_1432",
			ifelse(results$site=="s2l03_170527_028320008_500","s2l03_170527_028380008_500",
				ifelse(results$site=="s2l06_170527_028320008_1300","s2l06_170527_028380008_1300",
					ifelse(results$site=="s2l07_170401_Pepperwood_1700","s2l07_170401_pepperwood_1700",
						ifelse(results$site=="s2l10_170620_060060059_2300","s2l01_170620_060060059_2300",results$site))))))

results<-merge(results,geo,by="site",all.x=T)
save(results,file=paste(pth,"testResults.RData",sep=""))

########################################################
##The below does not work for occuFP
re<-ranef(mdl)
preds<-bup(re,"mean")

#attribute with land cover type
vegshp<-readOGR(lcpth,dsn)

