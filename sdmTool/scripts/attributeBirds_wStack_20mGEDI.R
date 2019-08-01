# TODO: Add comment
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
rpth<-"c:/S2Ltemp/sdmTool/data/"
rezz<-c("500M","250M","1000M") 
auxvars<-c("dem","StreetDistance","StreamDistance","CoastDistance")
ndvivars<-c("_ann_05p","_ann_95p","_ann_med","_ann_min","_seas_diff","_sum","_var")
bcmvars<-c("aet","cwd","pet","ppt","tmx","tmn")
#bcmwyrs<-c("_wy2013","_wy2014","_wy2015")
bcmwyrs<-"_wy2013-2015"
bcmperiods<-c("_q1_OctNovDec","_q2_JanFebMar","_q3_AprMayJun","_q4_JulAugSep")
gediyr<-c("_2yr_")	#"_1yr_",,"_3yr_"
gedinoise<-"noised_"
gedivars<-c("niM2_1","gLAI010","gLAI102","gLAI203","gLAI304","gVDRm","gVDRb")	#"rhGss2","rhGss26","rhGss50","rhGss76","rhGss98","cover","FHDcan","FHDcnHs","gssHlfC","FHD","FHDhist","niM2","gVDRt", ***
birdfiles<-"c:/S2Ltemp/sdmTool/data/Birds/UDF/"


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
q<-l_ply(.data=rezz,.fun=function(zz,rpth,ndvivars,bcmvars,bcmyrs,bcmperiods,gediyr,gedinoise,gedivars,birdfiles){
			print(zz)
			svpth<-paste0(rpth,"birds/20mGEDI/",zz,"/")
			dem<-paste0("dem_",zz)
			
			# Auxiliary variables
			auxnames<-character();i<-0
			for(nn in auxvars){
				i<-i+1
				pth<-paste0(rpth,nn,"/",zz,"/",nn,"_",zz,".tif")
				auxrast<-raster(pth)
				if(i==1){
					stk<-auxrast
				}else{
					stk<-stack(stk,auxrast)
				}
				auxname<-paste0(nn,"_",zz)
				auxnames<-c(auxnames,auxname)
			}
			names(stk)<-auxnames
			
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
			gdr<-paste0("GEDI/20mGEDI/",gedinoise,zz)
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
			covardf<-getCellId(df=covardf,rast=stk[[dem]],rez=zz)
			#exclvars<-c("noised_niM2_3yr_","noised_rhGss50_3yr_"); exclvars<-paste0(exclvars,zz)
			#covardf<-covardf[,which(!names(covardf) %in% exclvars)]
			nccdf<-ncol(covardf)
			
			# scaled
			scaledcovars<-scale(covardf[,3:(nccdf-1)])
			scaledcovardf<-cbind(scaledcovars,covardf[,c(1:2,nccdf)])
			
			# VIF-corrected - ASSUMING the first zz is indeed 500M - see order of input above, in line 15
			if(zz=="500M"){
				load(paste0(rpth,"birds/covarInf.RData"))
				covarInf<-gsub("250M",zz,covarInf)	#my file is not up to date!
				covarInf<-gsub("_clip","",covarInf)	#another update
				#This update is only because we chose to use only 2yr data, so removing the 1yr and 3yr we added in lines 253-256
				covarInf<-subset(covarInf,!covarInf %in% c("noised_rhGss2_3yr_500M","noised_niM2_1_3yr_500M","noised_gLAI010_3yr_500M","noised_gLAI102_3yr_500M",
								"noised_gLAI304_3yr_500M","noised_hLAI203_3yr_500M","noised_gVDRm_3yr_500M","noised_gVDRb_3yr_500M",
								"noised_rhGss2_2yr_500M","noised_hLAI203_2yr_500M","noised_rhGss2_1yr_500M","noised_niM2_1_1yr_500M", 
								"noised_gLAI010_1yr_500M","noised_gLAI102_1yr_500M","noised_gLAI304_1yr_500M","noised_hLAI203_1yr_500M",
								"noised_gVDRm_1yr_500M","noised_gVDRb_1yr_500M"))
				
				#use only 2yr
				covnams<-names(scaledcovardf)
				covnams<-subset(covnams,!grepl("_3yr_",covnams) & !grepl("_1yr_",covnams))	#keep only 2yr GEDI covariates
				covnams<-subset(covnams,covnams!="y" & covnams!="x" & !grepl("gId",covnams))	#remove x, y, and gId (should not be there - just in case)
				subscaledcovardf<-scaledcovardf[,covnams]
				#	covarInf<-vif_func(in_frame=subscaledcovardf)   ***
				
				#only GEDI
				gediNams<-c(paste0("noised_",gedivars,"_2yr_",zz));
				covarInfGEDI<-vif_func(in_frame=subscaledcovardf[,gediNams])
				
				#only BCM
				bcmNamsdf<-expand.grid(bcmvars,bcmwyrs,bcmperiods);bcmNamsdf$Var2<-gsub("-",".",bcmNamsdf$Var2)
				bcmNamsdf$bcmNams<-paste0(bcmNamsdf$Var1,bcmNamsdf$Var2,bcmNamsdf$Var3,"_500M")
				covarInfBCM<-vif_func(in_frame=subscaledcovardf[,bcmNamsdf$bcmNams])
				
				#only NDVI
				ndviNams<-paste0("ndvi",ndvivars,"_",zz)
				covarInfNDVI<-vif_func(in_frame=subscaledcovardf[,ndviNams])
				
				#no GEDI
				auxNams<-paste0(auxvars,"_",zz)
				nogediNams<-c(auxNams,bcmNamsdf$bcmNams,ndviNams)
				covarInfNoGEDI<-vif_func(in_frame=subscaledcovardf[,nogediNams])
				
				#	#but after deflation, add back the equivalent 3yr and 1yr
				#	gedikept<-subset(covarInf,grepl("_2yr_",covarInf))
				#	gyr2keep<-gsub("_2yr_","_3yr_",gedikept)
				#	gyr1keep<-gsub("_2yr_","_1yr_",gedikept)
				#	covarInf<-c(covarInf,gyr2keep,gyr1keep)
				
				#	#get gedi only for 1 and 2 gediyrs.
				gedionly2yr<-covarInfGEDI
				#	gedionly3yr<-gsub("_2yr_","_3yr_",gedionly2yr)
				#	gedionly1yr<-gsub("_2yr_","_1yr_",gedionly2yr)
				
				save(covarInf, covarInfBCM, covarInfNDVI, covarInfNoGEDI,gedionly2yr,file=paste0(rpth,"birds/covarInf_20mGEDI.RData"))	#gedionly3yr,  gedionly1yr, 
				
			}else{	#load the 500M covar selected onto the other resolutions....
				load(paste0(rpth,"birds/covarInf_20mGEDI.RData"))
				covarInf<-gsub("500M",zz,covarInf)	#my file is not up to date!
				covarInf<-gsub("_clip","",covarInf)	#another update
				#This update is only because we chose to use only 2yr data, so removing the 1yr and 3yr we added in lines 253-256
				covarInf<-subset(covarInf,!covarInf %in% c("noised_rhGss2_3yr_500M","noised_niM2_1_3yr_500M","noised_gLAI010_3yr_500M","noised_gLAI102_3yr_500M",
								"noised_gLAI304_3yr_500M","noised_hLAI203_3yr_500M","noised_gVDRm_3yr_500M","noised_gVDRb_3yr_500M",
								"noised_rhGss2_2yr_500M","noised_hLAI203_2yr_500M","noised_rhGss2_1yr_500M","noised_niM2_1_1yr_500M", 
								"noised_gLAI010_1yr_500M","noised_gLAI102_1yr_500M","noised_gLAI304_1yr_500M","noised_hLAI203_1yr_500M",
								"noised_gVDRm_1yr_500M","noised_gVDRb_1yr_500M"))
				#extend to the subsets
				covarInfBCM<-gsub("500M",zz,covarInfBCM)
				covarInfNDVI<-gsub("500M",zz,covarInfNDVI)
				covarInfNoGEDI<-gsub("500M",zz,covarInfNoGEDI)
				#gedionly3yr<-gsub("500M",zz,gedionly3yr)
				gedionly2yr<-gsub("500M",zz,gedionly2yr)
				#gedionly1yr<-gsub("500M",zz,gedionly1yr)
				print(NROW(covarInf))
			}
			
			
			#add the cell location info to all
			deflatedcovardf<-cbind(scaledcovardf[,covarInf],covardf[,c(1:2,nccdf)])
			deflatedBCMdf<-cbind(scaledcovardf[,covarInfBCM],covardf[,c(1:2,nccdf)])
			deflatedNDVIdf<-cbind(scaledcovardf[,covarInfNDVI],covardf[,c(1:2,nccdf)])
			deflatedNoGEDIdf<-cbind(scaledcovardf[,covarInfNoGEDI],covardf[,c(1:2,nccdf)])
			#deflatedGEDI3yrdf<-cbind(scaledcovardf[,gedionly3yr],covardf[,c(1:2,nccdf)])
			deflatedGEDI2yrdf<-cbind(scaledcovardf[,gedionly2yr],covardf[,c(1:2,nccdf)])
			#deflatedGEDI1yrdf<-cbind(scaledcovardf[,gedionly1yr],covardf[,c(1:2,nccdf)])
			
			# loop through each species' data and merge with the above tables 
			for(ff in list.files(birdfiles,pattern="2006_2015")){
				print(paste(zz,ff))
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
				
				#and the subsets...
				deflatedBCMdf<-merge(deflatedBCMdf,bdf,by=gidfld,all.x=T)
				deflatedNDVIdf<-merge(deflatedNDVIdf,bdf,by=gidfld,all.x=T)
				deflatedNoGEDIdf<-merge(deflatedNoGEDIdf,bdf,by=gidfld,all.x=T)
				#deflatedGEDI3yrdf<-merge(deflatedGEDI3yrdf,bdf,by=gidfld,all.x=T)
				deflatedGEDI2yrdf<-merge(deflatedGEDI2yrdf,bdf,by=gidfld,all.x=T)
				#deflatedGEDI1yrdf<-merge(deflatedGEDI1yrdf,bdf,by=gidfld,all.x=T)
			}
			
			# save
			save(covardf,file=paste0(svpth,"unscaled_",zz,".RData"))
			save(scaledcovardf,file=paste0(svpth,"scaled_",zz,".RData"))
			save(deflatedcovardf,file=paste0(svpth,"deflated_",zz,".RData"))
			
			#save subsets too
			save(deflatedBCMdf,file=paste0(svpth,"deflatedBCM_",zz,".RData"))
			save(deflatedNDVIdf,file=paste0(svpth,"deflatedNDVI_",zz,".RData"))
			save(deflatedNoGEDIdf,file=paste0(svpth,"deflatedNoGEDI_",zz,".RData"))
			#save(deflatedGEDI3yrdf,file=paste0(svpth,"deflatedGEDI3yr_",zz,".RData"))
			save(deflatedGEDI2yrdf,file=paste0(svpth,"deflatedGEDI2yr_",zz,".RData"))
			#save(deflatedGEDI1yrdf,file=paste0(svpth,"deflatedGEDI1yr_",zz,".RData"))
			
		}, rpth=rpth,ndvivars=ndvivars,bcmvars=bcmvars,bcmyrs=bcmyrs,bcmperiods=bcmperiods,gediyr=gediyr,gedinoise=gedinoise,gedivars=gedivars,birdfiles=birdfiles)

Sys.time()-tm




