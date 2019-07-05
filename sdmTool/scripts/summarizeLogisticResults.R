# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(plyr); library(ggplot2); library(data.table)

#only the species listed in the paper, but there are others
spp<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
		"CBCH", "SOSP", "MODO", "ACWO", "AMGO", "WEBL", "NOFL", "BUSH",
		"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "BHGR")
ht<-c("Oak","Urban","Shrub","Riparian","Conifer","Shrub","Oak","Urban","Riparian","Grass",
		"Conifer","Riparian","Urban","Oak","Grass","Grass","Conifer","Oak",
		"Conifer","Oak","Oak","Shrub","Vairable","Conifer","Variable")

indf<-data.frame(species=rep(spp,9),habtype=rep(ht,9),rez=c(rep("250M",75),rep("500M",75),rep("1000M",75)),
		gediyrs=rep(c(rep("1yr",25),rep("2yr",25),rep("3yr",25)),3))

datapth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/"

lrtrez<-list()
jj<-0
for(ii in 1:nrow(indf)){
	zz<-indf[ii,"rez"]
	ss<-indf[ii,"species"]
	gg<-indf[ii,"gediyrs"]
	hh<-indf[ii,"habtype"]
	filena<-paste0(datapth,zz,"/",ss,"_",zz,"_",gg,"__balanced_logisticModelResults.RData")
	if(file.exists(filena)){
		load(filena)
		for(nn in 1:NROW(res)){
			jj<-jj+1
			rdf<-res[[nn]]
			tdfa<-subset(rdf$resdf, Model != "WithGEDIoptimized")
			tdfa$species<-ss;tdfa$rez<-zz;tdfa$gediyr<-gg;tdfa$habtype<-hh
			tdfa<-data.table(tdfa)
			lrtrez[[jj]]<-tdfa
		}
	}
}

logdata<-rbindlist(lrtrez)

## A plot of the distribution of chisq values for the general test of GEDI importance, by species and resolution, one plot per year
globy1<-subset(logdata,Model=="WithGEDI" & gediyr=="1yr" & LRTchisq < 50)
globy1$rezorder<-ifelse(globy1$rez=="250M",1,ifelse(globy1$rez=="500M",2,3))
globy1$rez<-reorder(globy1$rez,globy1$rezorder)
globy1$haborder<-ifelse(globy1$habtype=="Conifer",1,ifelse(globy1$habtype=="Oak",2,
				ifelse(globy1$habtype=="Shrub",3,ifelse(globy1$habtype=="Riparian",4,
								ifelse(globy1$habtype=="Grass",5,ifelse(globy1$habtype=="Urban",6,7))))))
globy1$species<-reorder(globy1$species,globy1$haborder)
pl1<-ggplot(data=globy1,aes(x=LRTchisq)) + geom_density(fill="blue") + 
		geom_vline(xintercept=14.05,color="red") +
		labs(x="Likelihood Ratio Test Chi-Square Value", title="One year of GEDI data") +
		theme_bw() + 
		theme(axis.title.y=element_blank(),	axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
		facet_grid(species~rez, scales="free") + theme(strip.text.y = element_text(angle = 0))

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/plots/GEDI_1yr_general_LRTest_1yr.jpg",
		width = 480, height = 480, quality=100)
print(pl1)
dev.off()

#2 years of GEDI
globy2<-subset(logdata,Model=="WithGEDI" & gediyr=="2yr" & LRTchisq < 50)
globy2$rezorder<-ifelse(globy2$rez=="250M",1,ifelse(globy2$rez=="500M",2,3))
globy2$rez<-reorder(globy2$rez,globy2$rezorder)
globy2$haborder<-ifelse(globy2$habtype=="Conifer",1,ifelse(globy2$habtype=="Oak",2,
				ifelse(globy2$habtype=="Shrub",3,ifelse(globy2$habtype=="Riparian",4,
								ifelse(globy2$habtype=="Grass",5,ifelse(globy2$habtype=="Urban",6,7))))))
globy2$species<-reorder(globy2$species,globy2$haborder)
pl2<-ggplot(data=globy2,aes(x=LRTchisq)) + geom_density(fill="blue") + 
		geom_vline(xintercept=14.05,color="red") +
		labs(x="Likelihood Ratio Test Chi-Square Value", title="Two years of GEDI data") +
		theme_bw() + 
		theme(axis.title.y=element_blank(),	axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
		facet_grid(species~rez, scales="free") + theme(strip.text.y = element_text(angle = 0))

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/plots/GEDI_2yr_general_LRTest.jpg",
		width = 480, height = 480, quality=100)
print(pl2)
dev.off()

#3 years of GEDI
globy3<-subset(logdata,Model=="WithGEDI" & gediyr=="3yr" & LRTchisq < 50)
globy3$rezorder<-ifelse(globy3$rez=="250M",1,ifelse(globy3$rez=="500M",2,3))
globy3$rez<-reorder(globy3$rez,globy3$rezorder)
globy3$haborder<-ifelse(globy3$habtype=="Conifer",1,ifelse(globy3$habtype=="Oak",2,
				ifelse(globy3$habtype=="Shrub",3,ifelse(globy3$habtype=="Riparian",4,
								ifelse(globy3$habtype=="Grass",5,ifelse(globy3$habtype=="Urban",6,7))))))
globy3$species<-reorder(globy3$species,globy3$haborder)
pl3<-ggplot(data=globy3,aes(x=LRTchisq)) + geom_density(fill="blue") + 
		geom_vline(xintercept=14.05,color="red") +
		labs(x="Likelihood Ratio Test Chi-Square Value", title="Three years of GEDI data") +
		theme_bw() + 
		theme(axis.title.y=element_blank(),	axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
		facet_grid(species~rez, scales="free") + theme(strip.text.y = element_text(angle = 0))

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/plots/GEDI_3yr_general_LRTest.jpg",
		width = 480, height = 480, quality=100)
print(pl3)
dev.off()

# Now need to use only 3yr and 250m and plot the vars independently
vardat<-subset(logdata,Model!="WithGEDI" & gediyr=="3yr" & LRTchisq < 12 & rez=="250M")
vardat$haborder<-ifelse(vardat$habtype=="Conifer",1,ifelse(vardat$habtype=="Oak",2,
				ifelse(vardat$habtype=="Shrub",3,ifelse(vardat$habtype=="Riparian",4,
								ifelse(vardat$habtype=="Grass",5,ifelse(vardat$habtype=="Urban",6,7))))))
vardat$species<-reorder(vardat$species,vardat$haborder)
pl4<-ggplot(data=vardat,aes(x=LRTchisq)) + geom_density(fill="blue") + 
		geom_vline(xintercept=3.84,color="red") +
		labs(x="Likelihood Ratio Test Chi-Square Value", title="Three years of GEDI data at 250M resolution") +
		theme_bw() + 
		theme(axis.title.y=element_blank(),	axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
		facet_grid(species~Model, scales="free") + theme(strip.text.y = element_text(angle = 0))

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/plots/GEDI_3yr_250M_singleVar_LRTest.jpg",
		width = 480, height = 480, quality=100)
print(pl4)
dev.off()



