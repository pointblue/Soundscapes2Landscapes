# TODO: Add comment
# 
# Author: lsalas
###############################################################################


#prepare the stack
#convert to df with cellId
#load the bird files at each rez
#merge and save
library(raster); library(fmsb);library(plyr)
rpth<-"C:/Users/lsalas/git/Soundscapes2Landscapes/sdmTool/data/"
rezz<-c("250M","500M","1000M")
ndvivars<-c("_ann_05p","_ann_95p","_ann_med","_ann_min","_seas_diff","_sum","_var")
bcmvars<-c("aet","cwd","pet","ppt","tmx","tmn")
#bcmwyrs<-c("_wy2013","_wy2014","_wy2015")
bcmwyrs<-"_wy2013-2015"
bcmperiods<-c("_q1_OctNovDec","_q2_JanFebMar","_q3_AprMayJun","_q4_JulAugSep")
gediyr<-"_3yr_"
gedinoise<-"noised_"
gedivars<-c("rhGss2","rhGss26","rhGss50","rhGss76","rhGss98","cover","FHDcan","FHDcnHs","gVDRt")
birdfiles<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Mateo/kriging/birdData/UDF/"


## This function retrieves the cellId for the cell within which each observation was made, for a given raster
# df is the table of records
# rast is the raster from which to obtain cellId values
# rez is a string indicating the resolution of the raster, and thus names the raster itself. Possible values are 200, 500, and 1000
getCellId<-function(df,rast,rez){
	cidnam<-paste0("gId",rez)
	df[,cidnam]<-cellFromXY(rast,df[,c("x","y")])
	
	return(df)
}

## This function crops and shifts a raster to match the other, per the specs of this project
# rast is the raster to fix
# dem is the correct base raster
cropShift<-function(rast,dem,xshft,yshft){
	rast<-crop(rast,dem)
	rast<-shift(rast,x=xshft,y=yshft)
	return(rast)
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

tm<-Sys.time()
##Start rezz loop here...
q<-l_ply(.data=rezz,.fun=function(zz,rpth,ndvivars,bcmvars,bcmyrs,bcmperiods,gediyr,gedinoise,gedivars){
			yshft<-ifelse(zz=="250M",62,ifelse(zz=="500M",-188,312))
			xshft<-ifelse(zz=="1000M",-381.2,118.8)
			# DEM_Rescaled
			pth<-paste0(rpth,"DEM_Rescaled/",zz,"/N38W123+124_1arc_V2_",zz,".tif")
			dem<-raster(pth)
			stk<-dem
			
			# Street_Distance
			pth<-paste0(rpth,"Street_Distance/",zz,"/StreetDistance_",zz,"_clip.tif")
			str_dist<-raster(pth)
			str_dist<-cropShift(rast=str_dist,dem=dem,xshft=xshft,yshft=yshft)
			stk<-stack(dem,str_dist)
			
			# Stream_Distance
			pth<-paste0(rpth,"Stream_Distance/",zz,"/StreamDistance_",zz,"_clip.tif")
			stm_dist<-raster(pth)
			stm_dist<-cropShift(rast=stm_dist,dem=dem,xshft=xshft,yshft=yshft)
			stk<-stack(stk,stm_dist)
			
			# Coast_Distance
			pth<-paste0(rpth,"Coast_Distance/",zz,"/CoastDistance_",zz,"_clip.tif")
			cst_dist<-raster(pth)
			cst_dist<-cropShift(rast=cst_dist,dem=dem,xshft=xshft,yshft=yshft)
			stk<-stack(stk,cst_dist)
			
			# DHI_MODIS = NDVI
			ndvinames<-character();i<-0
			for(nn in ndvivars){
				i<-i+1
				pth<-paste0(rpth,"DHI_MODIS/",zz,"/ndvi_",zz,nn,"-EPSG32610.tif")
				ndvirast<-raster(pth)
				if(i==1){
					ndvistack<-ndvirast
				}else{
					ndvistack<-stack(ndvistack,ndvirast)
				}
				nvname<-paste0("ndvi",nn,"_",zz)
				ndvinames<-c(ndvinames,nvname)
			}
			names(ndvistack)<-ndvinames
			stk<-stack(stk,ndvistack)
			
			# BCM
			i<-0
			bcmstkname<-character()
			for(vv in bcmvars){
				for(yy in bcmwyrs){
					for(ss in bcmperiods){
						i<-i+1
						pth<-paste0(rpth,"BCM/",zz,"/",vv,"/",vv,yy,ss,"_",zz,".tif")
						bcmvar<-raster(pth)
						if(i==1){
							bcmstk<-bcmvar
						}else{
							bcmstk<-stack(bcmstk,bcmvar)
						}
						vnam<-paste0(vv,yy,ss,"_",zz)
						bcmstkname<-c(bcmstkname,vnam)
					}
				}
			}
			names(bcmstk)<-bcmstkname
			stk<-stack(stk,bcmstk)
			
			#GEDI
			gdr<-paste0("GEDI/",gedinoise,zz)
			gedistkname<-character(); i<-0
			for(gg in gedivars){
				i<-i+1
				pth<-paste0(rpth,gdr,"/",gedinoise,gg,gediyr,zz,".tif")
				if(file.exists(pth)){
					gediv<-raster(pth)
					if(i==1){
						gedistk<-gediv
					}else{
						gedistk<-stack(gedistk,gediv)
					}
					vnam<-paste0(gedinoise,gg,gediyr,zz)
					gedistkname<-c(gedistkname,vnam)
				}else{
					print(paste("Did not find file",paste0(gedinoise,gg,gediyr,zz,".tif"),"Skipping it."))
				}
				
			}
			names(gedistk)<-gedistkname
			stk<-stack(stk,gedistk)
			
			# Make df...
			# raw
			covardf<-as.data.frame(stk,xy=TRUE)
			covardf<-getCellId(df=covardf,rast=dem,rez=zz)
			exclvars<-c("noised_niM2_3yr_","noised_rhGss50_3yr_"); exclvars<-paste0(exclvars,zz)
			covardf<-covardf[,which(!names(covardf) %in% exclvars)]
			nccdf<-ncol(covardf)
			
			# scaled
			scaledcovars<-scale(covardf[,3:(nccdf-1)])
			scaledcovardf<-cbind(scaledcovars,covardf[,c(1:2,nccdf)])
			
			# VIF-corrected
			covarInf<-vif_func(in_frame=scaledcovars)
			deflatedcovardf<-cbind(scaledcovars[,covarInf],covardf[,c(1:2,nccdf)])
			
			# loop through each species' data and merge with the above tables 
			for(ff in list.files(birdfiles)){
				load(paste0(birdfiles,ff))
				# use the right resolution
				resv<-ifelse(zz=="250M",1,ifelse(zz=="500M",2,3))
				bdf<-datalst[[resv]]
				spp<-unique(bdf$SpeciesCode)
				gidfld<-paste0("gId",zz)
				names(bdf)<-gsub("CellId",gidfld,names(bdf))
				names(bdf)<-gsub("presence",spp,names(bdf))
				names(bdf)<-gsub("NumDet",paste0("NumDet",spp),names(bdf))
				bdf<-bdf[,c(gidfld,spp,paste0("NumDet",spp))]
				
				covardf<-merge(covardf,bdf,by=gidfld,all.x=T)
				scaledcovardf<-merge(scaledcovardf,bdf,by=gidfld,all.x=T)
				deflatedcovardf<-merge(deflatedcovardf,bdf,by=gidfld,all.x=T)
			}
			# save
			svpth<-paste0(rpth,"birds/",zz)
			save(covardf,file=paste0(svpth,"/unscaled_",zz,".RData"))
			save(scaledcovardf,file=paste0(svpth,"/scaled_",zz,".RData"))
			save(deflatedcovardf,file=paste0(svpth,"/deflated_",zz,".RData"))
			
		}, rpth=rpth,ndvivars=ndvivars,bcmvars=bcmvars,bcmyrs=bcmyrs,bcmperiods=bcmperiods,gediyr=gediyr,gedinoise=gedinoise,gedivars=gedivars)

Sys.time()-tm

