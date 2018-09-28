# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#list the files .RData
#select only those for the 8 species of interest
#Get the RMSE, plot orderd by habitat

library(ggplot2)

pathToData<-"c:/temp/s2l/250M/"

resfiles<-list.files(pathToData,pattern=".RData")
resfiles<-subset(resfiles,!grepl("topVariables",resfiles))

species<-data.frame(spcd=c("ACWO","CALT","WESJ","MODO","NOMO","OATI","AMGO","LEGO","SPTO","DEJU", "HOFI","BLPH","RWBL","SOSP", "WCSP"),
		habitat=c("Oak","Oak","Oak","Oak","Oak","Oak","Grass","Grass","Conn","Conn","Urb","Rip","Rip","Rip","Scrub"),
		plotorder=c(1:15))

species<-subset(species, spcd %in% c("ACWO","WESJ","BLPH","RWBL","AMGO","LEGO","DEJU","SPTO"))

rmsedf<-data.frame()
for(ff in resfiles){
	load(paste0(pathToData,ff))
	tdf<-data.frame(model=names(supp),support=as.numeric(supp),Species=substr(ff,1,4))
	rmsedf<-rbind(rmsedf,tdf)
}
rmsedf$RMSE<-1-rmsedf$support

rmsedf<-subset(rmsedf,Species %in% species$spcd)
rmsedf<-merge(rmsedf,species,by.x="Species",by.y="spcd",all.x=T)
rmsedf$Species<-reorder(rmsedf$Species,rmsedf$plotorder)
rmsedf$Model<-ifelse(rmsedf$model=="pboo","AdaBoost",
		ifelse(rmsedf$model=="prfo","RandomForests",
				ifelse(rmsedf$model=="psvm","SupportVector","XGBoost")))
meanRMSE<-mean(rmsedf$RMSE)

p<-ggplot(rmsedf,aes(x=Model,y=RMSE)) + 
		geom_hline(yintercept=meanRMSE,color="black", linetype="dashed") +
		geom_point(aes(color=Species),size=2) +
		scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,by=0.1)) +
		labs(x="Model")

jpeg(file="c:/users/lsalas/desktop/GEDI_RMSE_250M.jpeg",quality=100, width=400, height=200)
print(p);dev.off()

