# TODO: Add comment
# 
# Authors: Leo Salas (lsalas@pointblue.org), Rose Snyder (rsnyder@pointblue.org), Matthew Clark (matthew.clark@sonoma.edu) & Colin Quinn (cq73@nau.edu)
###############################################################################


## This file seeks to ordinate survey sites based on sourscape indices.
## It does so by...


##############################################################
## Dependencies
library(fmsb);library(plyr);library(ggplot2);library(cluster)

## The data is a table of sites and the values of their estimate acoustic indices, as provided by PhD candidate Colin Quinn
dat<-read.csv("c:/users/lsalas/git/sparklemotion/NASA_S2L/all_site_acoustic_indices.csv", stringsAsFactors=F)

## We use a variance inflation filter function to reduce the number of indices to only those that are <= 99% colinear with each other
vif_func<-function(in_frame,thresh=10,trace=F,...){
	
	if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
	
	#get initial vif value for all comparisons of variables
	vif_init<-NULL
	var_names <- names(in_frame)
	for(val in var_names){
		regressors <- var_names[-which(var_names == val)]
		form <- paste(regressors, collapse = '+')
		form_in <- formula(paste(val, '~', form))
		vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
	}
	vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
	
	if(vif_max < thresh){
		if(trace==T){ #print output of each iteration
			prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
			cat('\n')
			cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
		}
		return(var_names)
	}
	else{
		
		in_dat<-in_frame
		
		#backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
		while(vif_max >= thresh){
			
			vif_vals<-NULL
			var_names <- names(in_dat)
			
			for(val in var_names){
				regressors <- var_names[-which(var_names == val)]
				form <- paste(regressors, collapse = '+')
				form_in <- formula(paste(val, '~', form))
				vif_add<-VIF(lm(form_in, data = in_dat, ...))
				vif_vals<-rbind(vif_vals,c(val,vif_add))
			}
			max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
			
			vif_max<-as.numeric(vif_vals[max_row,2])
			
			if(vif_max<thresh) break
			
			if(trace==T){ #print output of each iteration
				prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
				cat('\n')
				cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
				flush.console()
			}
			
			in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
			
		}
		
		return(names(in_dat))
		
	}
	
}

#############################################################
## We start by reducing the number of indices by the variance inflation filter
vifres<-vif_func(in_frame=dat[,2:8])
dat<-dat[,vifres]

## No acoustic index was too colinear with the rest, so none was excluded.
## How many clusters to make from the data? We explore below...
optk<-ldply(.dat=1:10, .fun=function(k,dat){
			kmm<-kmeans(dat, k, nstart=50,iter.max = 15 )
			tdf<-data.frame(k=k,twin=kmm$tot.withinss,tin=kmm$withinss,tbin=kmm$betweenss)
			return(tdf)
		},dat=dat)

ggplot(optk,aes(x=k,y=twin)) + geom_point() 
#7 clusters is more than enough to reduce the total within-cluster sum of squared differences to an assymptotic point of limited further reduction

kmm<-kmeans(dat, k=7, nstart=50,iter.max = 15)

dat$deploySiteite<-sapply(dat$file,function(x){
			spt<-gregexpr("_",x)[[1]][2]+1
			cpt<-gregexpr("_",x)[[1]][3]-1
			loc<-substr(x,spt,cpt)
			return(loc)
})

dat$cluster<-kmm$cluster

save(dat,file="c:/users/lsalas/git/sparklemotion/NASA_S2L/allSite_acousticIndices_7clusters.RData")

####################################################################################################################################
## We want to stratify by time of the day first, then cluster within...
library(fmsb);library(plyr);library(ggplot2);library(cluster)

dat<-read.csv("c:/users/lsalas/git/sparklemotion/NASA_S2L/all_site_acoustic_indices.csv", stringsAsFactors=F)
dat$stp<-grepl("-20",dat$file) # this gets rid of "testrec.wav and s2lam01_190520_101080002-1969-12-31_19-49.WAV
dat<-subset(dat,stp==TRUE)
dat<-subset(dat,grepl(".wav",file,ignore.case=TRUE))  #use only wav files

## We now add datetime to each recording from the filename
dat$stp<-sapply(dat$file,function(x){as.numeric(regexpr("-20",x)[[1]][1])+1})
dat$ept<-sapply(dat$file,function(x){as.numeric(regexpr(".wav",x,ignore.case=T))-1})
dat$srdate<-substr(dat$file,dat$stp,dat$ept)
getDattime<-function(x){
	dd<-try(as.POSIXlt(x,"%Y-%m-%d_%H-%M",tz="America/Los_Angeles"),silent=TRUE)
	if(inherits(dd,"try-error")){dd<-NA}
	return(dd)
}
dat$recdate<-getDattime(dat$srdate)

## We create bins based on time of the day: 6-9 am, 11 am -2 pm, and 4-7 pm
binTimeOfDay<-function(dd){
	hrv<-as.numeric(format(dd,"%H"))
	hrb<-ifelse(hrv>=6 & hrv <=9,1,
			ifelse(hrv>=11 & hrv<=14,2,
					ifelse(hrv>=16 & hrv<=19,3,0)))
	return(hrb)
}
dat$hourBin<-binTimeOfDay(dat$recdate)

## Vriance inflation filter function, again set at 99% colinearity, but this time evaluated within bins
vif_func<-function(in_frame,thresh=10,trace=F,...){
	
	if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
	
	#get initial vif value for all comparisons of variables
	vif_init<-NULL
	var_names <- names(in_frame)
	for(val in var_names){
		regressors <- var_names[-which(var_names == val)]
		form <- paste(regressors, collapse = '+')
		form_in <- formula(paste(val, '~', form))
		vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
	}
	vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
	
	if(vif_max < thresh){
		if(trace==T){ #print output of each iteration
			prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
			cat('\n')
			cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
		}
		return(var_names)
	}
	else{
		
		in_dat<-in_frame
		
		#backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
		while(vif_max >= thresh){
			
			vif_vals<-NULL
			var_names <- names(in_dat)
			
			for(val in var_names){
				regressors <- var_names[-which(var_names == val)]
				form <- paste(regressors, collapse = '+')
				form_in <- formula(paste(val, '~', form))
				vif_add<-VIF(lm(form_in, data = in_dat, ...))
				vif_vals<-rbind(vif_vals,c(val,vif_add))
			}
			max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
			
			vif_max<-as.numeric(vif_vals[max_row,2])
			
			if(vif_max<thresh) break
			
			if(trace==T){ #print output of each iteration
				prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
				cat('\n')
				cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
				flush.console()
			}
			
			in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
			
		}
		
		return(names(in_dat))
		
	}
	
}

## Let's inspect each bin independently
dat1<-subset(dat,hourBin==1)
vifres<-vif_func(in_frame=dat1[,2:8])
datt<-dat1[,vifres]
optk<-ldply(.dat=1:10, .fun=function(k,dt){
			kmm<-kmeans(dt, k, nstart=50,iter.max = 15 )
			tdf<-data.frame(k=k,twin=kmm$tot.withinss,tin=kmm$withinss,tbin=kmm$betweenss)
			return(tdf)
		},dt=datt)
ggplot(optk,aes(x=k,y=twin)) + geom_point() 
# k=6
kmm<-kmeans(datt, 6, nstart=50,iter.max = 15 )
dat1$cluster<-kmm$cluster
dat1$cluster<-paste0("D",dat1$cluster)

dat2<-subset(dat,hourBin==2)
vifres<-vif_func(in_frame=dat2[,2:8])
datt<-dat2[,vifres]
optk<-ldply(.dat=1:10, .fun=function(k,dt){
			kmm<-kmeans(dt, k, nstart=50,iter.max = 15 )
			tdf<-data.frame(k=k,twin=kmm$tot.withinss,tin=kmm$withinss,tbin=kmm$betweenss)
			return(tdf)
		},dt=datt)
ggplot(optk,aes(x=k,y=twin)) + geom_point() 
# k=6
kmm<-kmeans(datt, 6, nstart=50,iter.max = 15 )
dat2$cluster<-kmm$cluster
dat2$cluster<-paste0("N",dat2$cluster)

dat3<-subset(dat,hourBin==3)
vifres<-vif_func(in_frame=dat3[,2:8])
datt<-dat3[,vifres]
optk<-ldply(.dat=1:10, .fun=function(k,dt){
			kmm<-kmeans(dt, k, nstart=50,iter.max = 15 )
			tdf<-data.frame(k=k,twin=kmm$tot.withinss,tin=kmm$withinss,tbin=kmm$betweenss)
			return(tdf)
		},dt=dat3)
ggplot(optk,aes(x=k,y=twin)) + geom_point() 
# k=6
kmm<-kmeans(datt, 6, nstart=50,iter.max = 15 )
dat3$cluster<-kmm$cluster
dat3$cluster<-paste0("E",dat3$cluster)

## So, all bins cluster into 6 groups
## Back to the original data file from Colin - let's put cluster info from the three bins there
tdf<-rbind(dat1[,c("file","cluster")],dat2[,c("file","cluster")])
tdf<-rbind(tdf,dat3[,c("file","cluster")])
dat<-merge(dat,tdf,by="file",all.x=T)
dat$cluster<-ifelse(is.na(dat$cluster),"00",dat$cluster)

write.csv(dat,file="c:/users/lsalas/desktop/bioacuoustics_clustered.csv")

