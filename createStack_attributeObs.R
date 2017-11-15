# TODO: Add comment
# 
# Author: lsalas
###############################################################################

###################   dependencies
libs<-c("raster","rgdal","sp","rgeos","fmsb")
lapply(libs, require, character.only = TRUE)

## functions

# snapToGrid takes an input raster and ensures it is snapped to the provided base grid 
# inprast is a raster object, the input raster
# baserast is a raster object, the base grid to snap to
snapToGrid<-function(inprast,baserast){
	#compare: same projection, else reproject
	if(!compareRaster(baserast,inprast,extent=FALSE, rowcol=FALSE, crs=TRUE, res=FALSE, orig=FALSE, stopiffalse=FALSE)){
		inprast<-projectRaster(inprast,baserast)
	}
	#compare same extent (after re-projecting if needed), else crop
	if(!compareRaster(baserast,inprast,extent=TRUE, rowcol=FALSE, crs=FALSE, res=FALSE, orig=FALSE, stopiffalse=FALSE)){
		inprast<-crop(inprast,baserast)
	}
	#compare same dimensions (row/col), resolution (cell size), origin; else resample
	if(!compareRaster(baserast,inprast,extent=FALSE, rowcol=TRUE, crs=FALSE, res=FALSE, orig=FALSE, stopiffalse=FALSE) |
		!compareRaster(baserast,inprast,extent=FALSE, rowcol=FALSE, crs=FALSE, res=TRUE, orig=FALSE, stopiffalse=FALSE) |
		!compareRaster(baserast,inprast,extent=FALSE, rowcol=FALSE, crs=FALSE, res=FALSE, orig=TRUE, stopiffalse=FALSE)){
		inprast<-resample(inprast,baserast)
	}
	return(inprast)
}

# vif_func selects covariates based on variance inflation - outputs the most informative set
# in_frame is the input data.frame
# thresh is the threshold of the VIF, defaulting to a reasonable value (10)
# trace indicates if we want to print each iteration
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

## paths
gpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Geodata/"
dpth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/Classifications/"

## metadata
rastlist<-read.csv(paste(gpth,"covariatesListS2L_171114.csv",sep=""))
rastlist$CovariateFile<-gsub("\\\\","/",rastlist$CovariateFile)

###################   processing...
## load common grid
base<-raster(paste(gpth,"sahm/prototype_study_area_raster.tif",sep=""))

## load Matt's rasters, snap each to the base raster, then add to stack
covarstack<-stack()
for(ff in rastlist$CovariateFile){
	trast<-raster(paste(gpth,ff,sep=""))
	trast<-snapToGrid(inprast=trast,baserast=base)
	if(nlayers(covarstack)==0){
		covarstack<-trast
	}else{
		covarstack<-stack(covarstack,trast)
	}
}

## We have more covariates than we have data, so, let's use a sample of 1000 cells from the stack for the VIF selection
## estimate Variance Inflation Factor and subset the covariates to most informative subset
covardf<-as.data.frame(covarstack)
### Inspect!
lapply(names(covardf),FUN=function(x,covardf)summary(covardf[,x]),covardf)

#Happy with all currently there, so
covarstack<-stack()
for(ff in rastlist$CovariateFile){
	trast<-raster(paste(gpth,ff,sep=""))
	trast<-snapToGrid(inprast=trast,baserast=base)
	trast<-scale(trast)
	if(nlayers(covarstack)==0){
		covarstack<-trast
	}else{
		covarstack<-stack(covarstack,trast)
	}
}

covardf<-as.data.frame(covarstack)
covardf<-as.data.frame(na.omit(covardf))

in_frame<-covardf[sample(1:nrow(covardf),30000),]	#about 6% of all cells
optimcovars<-vif_func(in_frame=in_frame)

save(optimcovars, file=paste(dpth,"optimcovars.RData",sep=""))
writeRaster(covarstack, filename=paste(gpth,"covarstack.grd",sep=""), bandorder="BIL", overwrite=TRUE)

################################## IGNORE CODE BELOW ##################################



## load occupancy estimates and attribute with stack - the loaded data.frame is called "results"
load(paste(dpth,"testResults.RData",sep=""))
resmx<-results[,c("Easting","Northing")]
resext<-extract(covarstack,resmx,df=TRUE)
occuest<-cbind(results,resext[,optimcovars])

## save the attributed occupancy data
save(occuest,optimcovars, file=paste(dpth,"occu_est.RData",sep=""))


