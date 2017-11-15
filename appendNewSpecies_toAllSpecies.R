# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#load allspecies
#load the current species' data
#modify/correct sideIds
#merge
#write out

pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"
newspf<-"wren_allrecs_sitesummary.reviewed_WS.csv"
spcd<-"WREN"

allsp<-read.csv(paste(pth,"allspecies_allrecs_site.summaries_geo.csv",sep=""),as.is=T)

#Site	Code	Strata	Lat	Long	Easting	Northing	WREN

newsp<-read.csv(paste(pth,newspf,sep=""))
newsp<-newsp[,c("Site",spcd)]

newsp$Site<-ifelse(newsp$Site=="s2l01_170509_028390028_1432","s2l01_170509_028390028_1423",
		ifelse(newsp$Site=="s2l03_170415_028150053_1423","s2l03_170415_028150053_1432",
				ifelse(newsp$Site=="s2l03_170527_028320008_500","s2l03_170527_028380008_500",
						ifelse(newsp$Site=="s2l06_170527_028320008_1300","s2l06_170527_028380008_1300",
								ifelse(newsp$Site=="s2l07_170401_Pepperwood_1700","s2l07_170401_pepperwood_1700",
										ifelse(newsp$Site=="s2l10_170620_060060059_2300","s2l01_170620_060060059_2300",newsp$Site))))))

allsp<-merge(allsp,newsp,by="Site",all.x=T)
#INSPECT!

write.csv(allsp,paste(pth,"allspecies_allrecs_site.summaries_geo.csv",sep=""))
