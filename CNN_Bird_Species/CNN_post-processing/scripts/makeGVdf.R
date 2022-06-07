# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## We start by getting the validaion data, and the new recording name from AT
library(jsonlite); library(airtabler); library(plyr)

## Set the API in environ with usethis::edit_r_environ()

# Querying both Recording and the JSON makes AirTable to mix the orders after record 300, so querying separately and then linking...
validation<-airtable(base = "app3dyaO6mo6EBq6N", tables = "Validation")
val<-validation$Validation$select_all(fields=list("Name","Recording","JSONseconds"))
recdf<-as.data.frame(val[,c("Name","Recording")],row.names=NULL)
names(recdf)<-c("recNo","Recording")
recdf$Recording<-as.character(recdf$Recording)
recdf$Recording<-gsub("WAV","wav",recdf$Recording)

# separately...
jsondf<-as.data.frame(val[,c("Name","JSONseconds")],row.names=NULL)
names(jsondf)<-c("recNo","Detections")

resdf<-merge(recdf,jsondf,by="recNo",all.x=T)

#Remove the records without detection description - this removes the 2 dupes
resdf<-subset(resdf,!is.na(Detections))

# Get the registry names
recordings<-airtable(base = "app3dyaO6mo6EBq6N", tables = "Recordings")
fndf<-recordings$Recordings$select_all(fields=list("Recording","NewFilename"))
names(fndf)<-c("RecId","Recording","RegistryName","dtm")

# Has 1 dupe...
fndf<-unique(fndf[,c("Recording","RegistryName")])
fndf$Recording<-gsub("WAV","wav",fndf$Recording)

# Now merging...
resdf<-merge(resdf,fndf,by="Recording",all.x=T)

# Extract the json...
gvdf<-ldply(1:nrow(resdf),function(rr,resdf){
			rid<-as.character(resdf[rr,"recNo"])
			fnm<-as.character(resdf[rr,"Recording"])
			rnm<-as.character(resdf[rr,"RegistryName"])
			w<-as.character(resdf[rr,"Detections"])
			q<-try(fromJSON(w),silent = T)
			if(inherits(q,"try-error")){
				tdf<-data.frame(species=NA,secs=NA,call=NA)	
			}else{
				if(!"call" %in% names(q)){q$call<-NA}
				tdf<-data.frame(species=q$sp,secs=paste0("[",q$secs,"]"),call=q$call)
			}
			tdf$recno<-rid; tdf$Filename<-fnm; tdf$RegistryName<-rnm
			return(tdf)
		},resdf=resdf)

# Test that all json are valid
nrow(subset(gvdf,is.na(species)))	#2
gvdf<-subset(gvdf,!is.na(species))

## Now explode the secs field so that each row is a second-level detection.
# Fixing the "step statements:
gvdf[1054,"secs"]<-"[57,58,59]"
# Make all steps be 1, remove the "-"
for(rr in 1:nrow(gvdf)){
	sv<-gvdf[rr,"secs"]
	if(grepl("step",sv)){
		if(grepl("step 2",sv)){
			sv<-gsub("step 2","step 1",sv)
		}else if(grepl("step 3",sv)){
			sv<-gsub("step 3","step 1",sv)
		}else if(grepl("-",sv)){
			sv<-gsub("-",":",sv)
		}else{}
		gvdf[rr,"secs"]<-sv
	}
}
gvdf<-subset(gvdf,!is.na(species) & species!="UNSP")

## Will need this function:
## ss is a single record's secs content
breakSecsToNumbers<-function(ss){
	if(!grepl("step",ss)){
		nsv<-sort(as.numeric(fromJSON(ss)))
	}else{  #has step
		w<-strsplit(ss,",")
		spv<-unlist(lapply(w[[1]],function(x){
							if(grepl("\\[",x)){d<-substr(x,2,nchar(x))}
							else if(grepl("\\]",x)){d<-substr(x,1,nchar(x)-1)}
							else{d<-x}
							return(d)
						}))
		#some values in spv will have step, so must expand these
		nostp<-as.numeric(subset(spv,!grepl("step",spv)))
		stpv<-subset(spv,grepl("step",spv))
		stpvv<-unlist(lapply(stpv,function(x){return(substr(x,1,as.numeric(regexpr("step",x))-2))}))
		stpvn<-unlist(lapply(stpvv,function(x){
							stv<-as.numeric(substr(x,1,as.numeric(regexpr(":",x,fixed=T))-1))
							edv<-as.numeric(substr(x,as.numeric(regexpr(":",x,fixed=T))+1,nchar(x)))
							return(seqv<-seq(stv,edv,by = 1))
						}))
		nsv<-sort(c(nostp,stpvn))
	}
	return(nsv)
}

## Expand the seconds in gvdf
# Take each record,
gvedf<-ldply(1:nrow(gvdf),function(rr,df){
			spp<-as.character(df[rr,"species"])
			secv<-as.character(df[rr,"secs"])
			callt<-as.character(df[rr,"call"])
			rec<-as.character(df[rr,"recno"])
			filen<-as.character(df[rr,"Filename"])
			regn<-as.character(df[rr,"RegistryName"])
			
			if(grepl("NA",secv)){
				secnums<-NA
			}else{
				secnums<-breakSecsToNumbers(ss=secv)
			}
			rdf<-data.frame(secs=secnums)
			rdf$species<-spp; rdf$call<-callt; rdf$recno<-rec; rdf$Filename<-filen; rdf$RegistryName<-regn
			rdf<-rdf[,names(df)]
			return(rdf)
		}, df=gvdf)

## There are dupe entries
gvedf<-unique(gvedf)

save(gvedf,file="c:/users/lsalas/git/S2L_devel/GVanalyses/data/gvData_20210518.RData")


#############################################################################################################
## DEPRECATED
library(httr)
offset<-""
recdf<-data.frame()
while(!is.null(offset)){
	if(offset=="0"){offset<-""}
	aturl<-paste0("https://api.airtable.com/v0/app3dyaO6mo6EBq6N/Validation?fields[]=Name&fields[]=Recording&offset=",offset)
	wdt<-GET(aturl, add_headers(Authorization = "Bearer key-here")) 
	ldt<-content(wdt,"text")
	ldf<-fromJSON(ldt)
	rdf<-as.data.frame(ldf$records$fields)
	rdf$Recording<-as.character(rdf$Recording)
	names(rdf)<-c("recNo","Recording")
	recdf<-rbind(recdf,rdf)
	offset<-ldf$offset
}

offset<-""
jsondf<-data.frame()
while(!is.null(offset)){
	if(offset=="0"){offset<-""}
	aturl<-paste0("https://api.airtable.com/v0/app3dyaO6mo6EBq6N/Validation?fields[]=Name&fields[]=JSONseconds&offset=",offset)
	wdt<-GET(aturl, add_headers(Authorization = "Bearer key-here")) 
	ldt<-content(wdt,"text")
	ldf<-fromJSON(ldt)
	rdf<-as.data.frame(ldf$records$fields)
	if(nrow(rdf)>0){
		names(rdf)<-c("recNo","Detections")
		jsondf<-rbind(jsondf,rdf)
	}
	offset<-ldf$offset
}

offset<-""
fndf<-data.frame()
while(!is.null(offset)){
	if(offset=="0"){offset<-""}
	aturl<-paste0("https://api.airtable.com/v0/app3dyaO6mo6EBq6N/Recordings?fields[]=Recording&fields=NewFilename&offset=",offset)
	wdt<-GET(aturl, add_headers(Authorization = "Bearer key-here")) 
	ldt<-content(wdt,"text")
	ldf<-fromJSON(ldt)
	rdf<-as.data.frame(ldf$records$fields)
	names(rdf)<-c("Recording","RegistryName")
	fndf<-rbind(fndf,rdf)
	offset<-ldf$offset
}

