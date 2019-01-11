# TODO: Add comment
# 
# Author: lsalas@pointblue.org
###############################################################################


## This code simply generates a data.frame that can be saved to a csv as shown below
## The purpose of the csv file is to list the set of arguments to pass to a script that will run in batch mode
## The combination of argument values is generated here

gitpath="/home/ubuntu/s2l/git/" 
svpath="/home/ubuntu/s2l/results/" 
logdir="/home/ubuntu/s2l/logs/"
species<-c("WESJ", "HOFI", "CALT", "BLPH", "DEJU", "WCSP", "OATI", "BRBL", "RWBL", "LEGO",
		"CBCH", "SOSP", "YRWA", "MODO", "ACWO", "RSHA", "AMGO", "WEBL", "NOFL", "BUSH",
		"SPTO", "NOMO", "NUWO", "CAQU", "BEWR", "STJA", "HOSP", "KILL", "AMKE", "DOWO",
		"WBNU", "PISI", "WEME", "WREN", "PUFI", "SAVS", "BRCR", "WIWA", "BHGR")
resolution<-c("250M","500M","1000M") #
noise<-c("noised")
gediyrs<-c("1yr","2yr","3yr")
addGEDI<-c(TRUE,FALSE)

cases<-expand.grid(gitpath=gitpath, svpath=svpath, logdir=logdir, spp=species, rez=resolution, 
		noise=noise, yrspan=gediyrs, gedi=addGEDI,stringsAsFactors=FALSE)

# create the csv where each argument for the script file is in a different column as usual...
#write.csv(cases,file=paste0(gitpath,"batchargs.csv"))

# or create a csv where each row is the full call to the script
# --datapath=/path/to/data/files.RData --svpath=/path/to/savedir --logdir=/path/to/logdir --species=WESJ --resolution=250M --yearspan=3yrs --withgedi=FALSE
bc<-cases
bc$calls<-paste(paste0(bc$gitpath,"fitSDMbatch.R"),paste0("--gitpath=",bc$gitpath),paste0("--svpath=",bc$svpath),paste0("--logdir=",bc$logdir),
		paste0("--species=",bc$spp),paste0("--resolution=",bc$rez),paste0("--yearspan=",bc$yrspan),paste0("--withgedi=",bc$gedi))
write.table(bc[,"calls"],paste0(gitpath,"batchcalls.csv"),row.names=FALSE,col.names=FALSE)


