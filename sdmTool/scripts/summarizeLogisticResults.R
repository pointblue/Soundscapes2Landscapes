# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(plyr); library(ggplot2)

species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
		"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
		"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
		"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
resolution<-c("250M","500M","1000M") #
gediyrs<-c("1yr","2yr","3yr")
datapth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/GEDIpaper/LogisticRegResults/"


sumdfa<-data.frame()
sumdfb<-data.frame()
for(zz in resolution){
	for(gg in gediyrs){
		for(ss in species){
			filena<-paste0(datapth,zz,"/",ss,"_",zz,"_",gg,"__AsIs_logisticModelResults.RData")
			if(file.exists(filena)){
				load(filena)
				rdf<-res$resdf
				rdf$species<-ss;rdf$resolution<-zz;rdf$gediyr<-gg
				rdf$signif<-ifelse(rdf$LRTpval<0.05,1,0)
				sumdfa<-rbind(sumdfa,rdf)
				rm(list=c("res","rdf"));gc()
			}
			filenb<-paste0(datapth,zz,"/",ss,"_",zz,"_",gg,"__balanced_logisticModelResults.RData")
			if(file.exists(filenb)){
				load(filenb)
				#there are 500 bootstraps, so we aregoing to profile the parameters from these 
				brdf<-ldply(.data=1:500,.fun=function(x,res){
							rdf<-res[[x]]$resdf
							rdf$species<-ss;rdf$resolution<-zz;rdf$gediyr<-gg
							rdf$signif<-ifelse(rdf$LRTpval<0.05,1,0)
							rdf$bootNum<-x
							return(rdf)
						},res=res)
				sumdfb<-rbind(sumdfb,brdf)
				rm(list=c("res","brdf"));gc()
			}
		}
	}
}

p1a<-ggplot(data=subset(sumdfa,Model=="WithGEDIoptimized" & gediyr=="3yr" & resolution=="250M" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_point(aes(color=signif)) +
		coord_flip() + theme(legend.position="none")

p2a<-ggplot(data=subset(sumdfa,Model=="WithGEDIoptimized" & gediyr=="3yr" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_hline(yintercept=0.05,color="black") +
		geom_point(aes(color=resolution),position=position_dodge(width = 0.5)) +
		coord_flip() 

p3a<-ggplot(data=subset(sumdfa,Model=="WithGEDIoptimized" & resolution=="250M" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_hline(yintercept=0.05,color="black") +
		geom_point(aes(color=gediyr),position=position_dodge(width = 0.5)) +
		coord_flip() 

birdhab<-read.csv("c:/users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/Birds/Birds_habitats.csv",stringsAsFactors=TRUE)
sumdfa<-merge(sumdfa,birdhab,by.x="species",by.y="SpeciesCode",all.x=T)
#Habitats: O=oak, R=riparian, C=conifer, U=urban, G=grasslands, S=scrub

habitats<-c("O","R","C","U","G","S")
for(hh in habitats){
	plotdf<-subset(sumdfa,grepl(hh,Habitat))
	ph<-ggplot(data=subset(plotdf,Model=="WithGEDIoptimized"),aes(x=species,y=LRTpval)) + geom_point(aes(color=signif)) +
			coord_flip() + theme(legend.position="none") + facet_grid(gediyr~resolution) + labs(x=paste(hh,"Species"),y="Likelihood ratio test p-value")
	dev.new();print(ph)
}

habadf<-data.frame()
for(hh in habitats){
	tdf<-subset(sumdfa,grepl(hh,Habitat) & Model=="WithGEDIoptimized")
	tdf$Habitat<-ifelse(hh=="O","Oak woodlands",ifelse(hh=="R","Riparian",ifelse(hh=="C","Conifer forests",ifelse(hh=="U","Urban",
									ifelse(hh=="G","Grasslands","Scrub")))))
	tdf$signif<-ifelse(tdf$signif==1,"Important","Not important")
	habadf<-rbind(habadf,tdf)
}
ph<-ggplot(data=habadf,aes(x=Habitat,y=LRTpval)) + geom_violin() + geom_jitter(height = 0, width = 0.1,aes(color=signif)) +
		coord_flip() + facet_grid(gediyr~resolution) + labs(x="Habitat",y="Likelihood ratio test p-value", color="GEDI relevance")

## need to profile the bootstrap
sumdfb<-merge(sumdfb,birdhab,by.x="species",by.y="SpeciesCode",all.x=T)
habbdf<-data.frame()
for(hh in habitats){
	tdf<-subset(sumdfb,grepl(hh,Habitat) & Model=="WithGEDIoptimized")
	tdf$Habitat<-ifelse(hh=="O","Oak woodlands",ifelse(hh=="R","Riparian",ifelse(hh=="C","Conifer forests",ifelse(hh=="U","Urban",
									ifelse(hh=="G","Grasslands","Scrub")))))
	tdf$signif<-ifelse(tdf$signif==1,"Important","Not important")
	habbdf<-rbind(habbdf,tdf)
}
ph<-ggplot(data=habbdf,aes(x=Habitat,y=LRTpval)) + geom_violin() + #geom_jitter(height = 0, width = 0.1,aes(color=signif)) +
		scale_y_continuous(limits=c(0,0.5), breaks=c(0,0.1,0.2,0.3,0.4,0.5)) + geom_hline(yintercept=0.05,color="red") +
		coord_flip() + facet_grid(gediyr~resolution) + labs(x="Habitat",y="Likelihood ratio test p-value", color="GEDI relevance")


#numGEDI


