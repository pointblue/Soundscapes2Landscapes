# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(ggplot2)

pathToData<-"c:/temp/s2l/250M/"

load(paste0(pathToData,"NOGEDI_topVariables_250M.RData"))
topvars$RelImportance<-as.numeric(unlist(topvars$RelImportance))

species<-data.frame(spcd=c("ACWO","CALT","WESJ","MODO","NOMO","OATI","AMGO","LEGO","SPTO","DEJU", "HOFI","BLPH","RWBL","SOSP", "WCSP"),
		habitat=c("Oak","Oak","Oak","Oak","Oak","Oak","Grass","Grass","Conn","Conn","Urb","Rip","Rip","Rip","Scrub"),
		plotorder=c(1:15))

species<-subset(species, spcd %in% c("ACWO","WESJ","BLPH","RWBL","AMGO","LEGO","DEJU","SPTO"))

top<-aggregate(RelImportance~Species+VarType+Model,topvars,sum)
top<-subset(top,Species %in% species$spcd)
top<-merge(top,species,by.x="Species",by.y="spcd",all.x=T)
top$Species<-reorder(top$Species,top$plotorder)

p<-ggplot(top,aes(x=Species,y=RelImportance)) + 
		geom_histogram(stat="identity",aes(color=VarType,fill=VarType),position="stack") + 
		facet_wrap(~Model,ncol=2) + coord_flip() + 
		labs(x="",y="Relative Importance",color="Var. Type",fill="Var. Type")

jpeg(file="c:/users/lsalas/desktop/NOGEDI_varImportance_250M.jpeg",quality=100, width=450, height=400)
print(p);dev.off()
