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
svpath<-"c:/s2ltemp/sdmtool/results/"

sumdf<-data.frame()
for(zz in resolution){
	for(gg in gediyrs){
		for(ss in species){
			filen<-paste0(svpath,zz,"/",ss,"_",zz,"_",gg,"_logisticModelResults.RData")
			if(file.exists(filen)){
				load(filen)
				resdf$species<-ss;resdf$resolution<-zz;resdf$gediyr<-gg
				resdf$signif<-ifelse(resdf$LRTpval<0.05,1,0)
				sumdf<-rbind(sumdf,resdf)
			}
			
		}
	}
}

p1<-ggplot(data=subset(sumdf,Model=="WithGEDIoptimized" & gediyr=="3yr" & resolution=="250M" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_point(aes(color=signif)) +
		coord_flip() + theme(legend.position="none")

p2<-ggplot(data=subset(sumdf,Model=="WithGEDIoptimized" & gediyr=="3yr" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_hline(yintercept=0.05,color="black") +
		geom_point(aes(color=resolution),position=position_dodge(width = 0.5)) +
		coord_flip() 

p3<-ggplot(data=subset(sumdf,Model=="WithGEDIoptimized" & resolution=="250M" & LRTpval<0.5),aes(x=species,y=LRTpval)) + geom_hline(yintercept=0.05,color="black") +
		geom_point(aes(color=gediyr),position=position_dodge(width = 0.5)) +
		coord_flip() 

