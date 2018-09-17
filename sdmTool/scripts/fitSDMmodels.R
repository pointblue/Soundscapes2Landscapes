# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## Dependencies


## Definitions
spp<-c("WESJ")
## Any one or a list of any of the following:
species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
			"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
			"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
			"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")

resolution<-c("1000M") # "500M", "250M"
noise<-c("noised")
gediyrs<-c("3yr")

# HERE: Loop through resolutions...
# Load the deflated bird file and filter for the desired species
dtpth<-paste0("C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/birds/",resolution)
load(file=paste0(dtpth,"/deflated_",resolution,".RData"))

# HERE: Loop through species
#select only the desired species
omitspecies<-subset(species,species!=spp)
omitnumdet<-paste0("NumDet",omitspecies)
spdata<-deflatedcovardf[,which(!names(deflatedcovardf) %in% c(omitspecies,omitnumdet))]
spdata<-as.data.frame(na.omit(spdata))

# Here fit the stack, predict, and save the wighted average of all models
#(get rid of the following columns: x, y, gId100M)

# ...next species
# ...next resolution
