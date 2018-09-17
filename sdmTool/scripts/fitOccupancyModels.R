# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## STOP!!
## go back to filterSpeciesData.R and attribute cells with some unique continuous covariate that can be used to predict to cell level

library(unmarked); library(plyr);library(XLConnect)

datapth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/birdData/UDF/"
spdf<-try(readWorksheetFromFile("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/S2L_Sonoma_only_Species_DLadds.xlsx",sheet="Final"))
species<-spdf$SpeciesCode; 
rezz<-c("gId250","gId500","gId1000")

ss<-"WESJ"
zz<-rezz[1]
load(paste0(datapth,ss,".RData"))
udf<-datalst[[zz]]
mdl1<-occu(as.formula("~1 ~cellId"),udf,se=T,engine="C")
mdl2<-occu(as.formula("~jday ~cellId"),udf,se=T,engine="C")
top<-ifelse( mdl1@AIC< mdl2@AIC, mdl1,mdl2)
#predict and append to the raw data
