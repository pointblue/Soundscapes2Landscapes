# TODO: This version does not do the variance inflation factor variable selection (M Clark, 1/15/19)
# 
# Author: lsalas
###############################################################################


# prepare the stack
# convert to df with cellId
# load the bird files at each rez
# starting with 250M resolution, determine the variables to include with VIF
# use that same set of variables at all resolutions
# merge and save
library(raster); library(fmsb);library(plyr)
rpth<-"C:/Soundscapes2Landscapes/sdmTool/data/"
rezz<-c("250M","500M","1000M") #ALWAYS start with 250M!!!
ndvivars<-c("_ann_05p","_ann_95p","_ann_med","_ann_min","_seas_diff","_sum","_var")
bcmvars<-c("aet","cwd","pet","ppt","tmx","tmn")
#bcmwyrs<-c("_wy2013","_wy2014","_wy2015")
bcmwyrs<-"_wy2013-2015"
bcmperiods<-c("_q1_OctNovDec","_q2_JanFebMar","_q3_AprMayJun","_q4_JulAugSep")
gediyr<-c("_1yr_","_2yr_","_3yr_")
gedinoise<-"noised_"
gedivars<-c("rhGss2","rhGss26","rhGss50","rhGss76","rhGss98","cover","FHDcan","FHDcnHs","niM2","gVDRt","gssHlfC","FHD","FHDhist","niM2_1","gLAI010","gLAI102","gLAI203","gLAI304",
		"hLAI010","hLAI102","hLAI203","hLAI304","gVDRm","gVDRb")
birdfiles<-"C:/Soundscapes2Landscapes/sdmTool/data/Birds/UDF/"


## This function retrieves the cellId for the cell within which each observation was made, for a given raster
# df is the table of records
# rast is the raster from which to obtain cellId values
# rez is a string indicating the resolution of the raster, and thus names the raster itself. Possible values are 200, 500, and 1000
getCellId<-function(df,rast,rez){
	cidnam<-paste0("gId",rez)
	df[,cidnam]<-cellFromXY(rast,df[,c("x","y")])
	
	return(df)
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
covarInf<-NULL
q<-l_ply(.data=rezz,.fun=function(zz,rpth,ndvivars,bcmvars,bcmyrs,bcmperiods,gediyr,gedinoise,gedivars,birdfiles,covarInf){
			#yshft<-ifelse(zz=="250M",62,ifelse(zz=="500M",-188,312))
			#xshft<-ifelse(zz=="1000M",-381.2,118.8)
			
			svpth<-paste0(rpth,"birds_noVIF/",zz,"/")
			
			# DEM_Rescaled
			pth<-paste0(rpth,"DEM_Rescaled/",zz,"/dem_clip_",zz,".tif")
			dem<-raster(pth)
			stk<-dem
			
			# Street_Distance
			pth<-paste0(rpth,"Street_Distance/",zz,"/StreetDistance_",zz,"_clip.tif")
			str_dist<-raster(pth)
			stk<-stack(dem,str_dist)
			
			# Stream_Distance
			pth<-paste0(rpth,"Stream_Distance/",zz,"/StreamDistance_",zz,"_clip.tif")
			stm_dist<-raster(pth)
			stk<-stack(stk,stm_dist)
			
			# Coast_Distance
			pth<-paste0(rpth,"Coast_Distance/",zz,"/CoastDistance_",zz,"_clip.tif")
			cst_dist<-raster(pth)
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
			stk1<-resample(ndvistack,stk,method="ngb")
			stk<-stack(stk,stk1)
			
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
			stk1<-resample(bcmstk,stk,method="ngb")
			stk<-stack(stk,stk1)
			
			#GEDI
			gdr<-paste0("GEDI/",gedinoise,zz)
			gedistkname<-character(); i<-0
			for(gg in gedivars){
			  for(yy in gediyr){
			    #print(paste(gg,yy))
			    i<-i+1
			    pth<-paste0(rpth,gdr,"/",gedinoise,gg,yy,zz,".tif")
			    if(file.exists(pth)){
			      gediv<-raster(pth)
			      if(i==1){
			        gedistk<-gediv
			      }else{
			        gedistk<-stack(gedistk,gediv)
			      }
			      vnam<-paste0(gedinoise,gg,yy,zz)
			      gedistkname<-c(gedistkname,vnam)
			    }else{
			      print(paste("Did not find file",paste0(gedinoise,gg,yy,zz,".tif"),"Skipping it."))
			    }
			  }
			  
				
			}
			names(gedistk)<-gedistkname
			stk1<-resample(gedistk,stk,method="ngb")
			stk<-stack(stk,stk1)
			
			# Make df...
			# raw
			covardf<-as.data.frame(stk,xy=TRUE)
			covardf<-getCellId(df=covardf,rast=dem,rez=zz)
			#exclvars<-c("noised_niM2_3yr_","noised_rhGss50_3yr_"); exclvars<-paste0(exclvars,zz)
			#covardf<-covardf[,which(!names(covardf) %in% exclvars)]
			nccdf<-ncol(covardf)
			
			# scaled
			scaledcovars<-scale(covardf[,3:(nccdf-1)])
			scaledcovardf<-cbind(scaledcovars,covardf[,c(1:2,nccdf)])
			
			# VIF-corrected - ASSUMING the first zz is indeed 250M
			# Note: this code just skips the vif_func, so has not VIF selection (M Clark)
			if(zz=="250M"){
			  #use only 3yr
			  covnams<-names(scaledcovardf)
			  covnams<-subset(covnams,!grepl("_2yr_",covnams) & !grepl("_1yr_",covnams))	#keep only 3yr GEDI covariates
			  covnams<-subset(covnams,covnams!="y" & covnams!="x" & !grepl("gId",covnams))	#remove x, y, and gId (should not be there - just in case)
			  subscaledcovardf<-scaledcovardf[,covnams]
				#covarInf<-vif_func(in_frame=subscaledcovardf)
			  covarInf<-covnams # don't run the VIF function, so selects all variables
				#but after deflation, add back the equivalent 2yr and 1yr
				gedikept<-subset(covarInf,grepl("_3yr_",covarInf))
				gyr2keep<-gsub("_3yr_","_2yr_",gedikept)
				gyr1keep<-gsub("_3yr_","_1yr_",gedikept)
				covarInf<-c(covarInf,gyr2keep,gyr1keep)
				save(covarInf, file=paste0(rpth,"birds_noVIF/covarInf.RData"))
			}else{	#load the 250M covar selected
				load(paste0(rpth,"birds_noVIF/covarInf.RData"))
				covarInf<-gsub("250M",zz,covarInf)
				print(NROW(covarInf))
			}
			deflatedcovardf<-cbind(scaledcovardf[,covarInf],covardf[,c(1:2,nccdf)])
			
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
			save(covardf,file=paste0(svpth,"unscaled_",zz,".RData"))
			save(scaledcovardf,file=paste0(svpth,"scaled_",zz,".RData"))
			save(deflatedcovardf,file=paste0(svpth,"deflated_",zz,".RData"))
			
		}, rpth=rpth,ndvivars=ndvivars,bcmvars=bcmvars,bcmyrs=bcmyrs,bcmperiods=bcmperiods,gediyr=gediyr,gedinoise=gedinoise,gedivars=gedivars,birdfiles=birdfiles,covarInf=covarInf)

Sys.time()-tm
