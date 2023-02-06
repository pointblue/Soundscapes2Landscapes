# PURPOSE:
# Creates graphs and output analysis files for the paper
# "Classification of Bird Vocalizations in Soundscapes from an Urban-wildland Gradient for Applied Biodiversity Monitoring with Citizen Scientists"

# AUTHORS:
# Dr. Matthew Clark
# Dept of Geography, Environment, and Planning
# Sonoma State University
# matthew.clark@sonoma.edu

# Dr. Leo Salas
# EcoInformatics and Climate Solutions
# Point Blue Conservation Science
# lsalas@pointblue.org

# Version: January 28, 2023; R 4.2.2

# Point to your local cloned repository
pathToLocalGit<-"[insert local path here]/" 

# load packages
libs<-c("ggplot2","plyr","dplyr","RColorBrewer","ggpubr","data.table")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))

# load functions
source(paste0(pathToLocalGit,"scripts/dataFunctions.R"))

##############################################################################################################################
#load the matches
load(file=paste0(pathToLocalGit,"data/ROI_GV_allmatches_011523.RData"))  #ROI_GV_allmatches_011323.RData contains the nongeoBN and includes non-target species. Do not use.

#Need the following data to attribute the roiTest data...
afiles<-read.csv(paste0(pathToLocalGit,"data/audiofiles_20221216.csv"),stringsAsFactors=F)
afiles<-afiles[,-1]
afiles$chrdt<-as.character(as.POSIXct(paste0("20",afiles$Year,"-",afiles$Month,"-",afiles$Day," ",afiles$Hour,":",afiles$Minute),format="%Y-%m-%d %H:%M"))
afiles$SamplingEvent<-paste0(afiles$SiteName,"_",afiles$chrdt)
afiles$SamplingEvent<-substr(afiles$SamplingEvent,1,32)
afiles$SamplingEvent<-gsub(" ","_",afiles$SamplingEvent)
afiles$SamplingEvent<-gsub(":","-",afiles$SamplingEvent)
afiles<-afiles[,c("AudiofileId","SiteName","Year","Month","Day","Hour","Minute","SamplingEvent")]

# roiTestdf is needed to find optimal thresholds
roiTestdf<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_201109_testing.csv"), stringsAsFactors=FALSE)
roiTestdf$SamplingEvent<-substr(roiTestdf$filename,1,32)
roiTestdf<-merge(roiTestdf,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
roiTestdf<-subset(roiTestdf,!is.na(AudiofileId))
# Need the absent data for porper model evaluation to find FP
roiAbs_nptdf<-read.csv(paste0(pathToLocalGit,"data/pattern_matching_ROIs_230109_absent_testing.csv"), stringsAsFactors=FALSE)
roiAbs_nptdf$SamplingEvent<-substr(roiAbs_nptdf$filename,1,32)
roiAbs_nptdf<-merge(roiAbs_nptdf,afiles[,c("SamplingEvent","AudiofileId")],by="SamplingEvent",all.x=T)
roiAbs_nptdf<-subset(roiAbs_nptdf,!is.na(AudiofileId))
roiTestdf<-rbind(roiTestdf,roiAbs_nptdf)

# Then summarize and calculate indices by hurdle value
sumGV_NPTdf<-summarizeByHurdle(allmatches=nptGVmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumGV_NPTdf$Source<-"Soundscape - No XC Pre-training"

sumGV_PTdf<-summarizeByHurdle(allmatches=ptGVmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumGV_PTdf$Source<-"Soundscape - XC Pre-training"

sumROI_NPTdf<-summarizeByHurdle(allmatches=nptROImatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumROI_NPTdf$Source<-"ROI - No XC Pre-training"

sumROI_PTdf<-summarizeByHurdle(allmatches=ptROImatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumROI_PTdf$Source<-"ROI - XC Pre-training"

sumGV_BirdNETdf<-summarizeByHurdle(allmatches=bnGVmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumGV_BirdNETdf$Source<-"Soundscape - BirdNET geographic"

sumGV_ngBirdNETdf<-summarizeByHurdle(allmatches=ngbnGVmatches,bySpecies="no",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
sumGV_ngBirdNETdf$Source<-"Soundscape - BirdNET non-geographic"

reslist<-list(sumGV_NPTdf,sumGV_PTdf,sumROI_NPTdf,sumROI_PTdf,sumGV_BirdNETdf,sumGV_ngBirdNETdf)   #,sumGV_ngBirdNETdf
npdata<-rbindlist(reslist)
names(npdata)<-gsub("hurdle","Threshold",names(npdata))
names(npdata)<-gsub("ModelName","Model",names(npdata))

# Check! Test plot
ggplot(npdata,aes(x=Threshold,y=Prec)) + geom_line(aes(color=Source),linewidth=2) + facet_wrap(~Model,ncol=2)

####################################
## FIGURE 3
# Precision vs. Recall - ROIs and soundscape pre-trained vs not pretrained
p1da<-ggplot(subset(npdata, Threshold<=0.99 & Model!="BirdNET"),aes(x=Sens*100, y=Prec*100)) + facet_wrap(~Model,ncol=3) + 
		geom_line(aes(color=Source),linewidth=1) +
		labs(x="Recall", y="Precision",color="Test source") +
		theme_bw() +
		scale_colour_brewer(palette = "Paired") +
		scale_x_continuous(breaks=seq(0,100,10)) +
    scale_y_continuous(breaks=seq(0,100,10)) +
    #coord_fixed() +
    expand_limits(x = c(0,100), y = c(0,100)) +
    theme(legend.position="bottom", legend.title= element_blank()) +
    guides(color=guide_legend(nrow=2, byrow=TRUE))
dev.new();print(p1da)   

#write.csv(npdata,paste0(pathToLocalGit,"data/total_prediction_statistics.csv"),row.names=F)
#ggsave(paste0(pathToLocalGit,"figures/Pre-training_vs_no pre-training.png"), width = 7, height = 3, units = "in", dpi=600)

####################################
## FIGURE 4
# Precision vs. threshold - ROIs and soundscape
ptdata = npdata %>% filter(Source %in% c("Soundscape - XC Pre-training","ROI - XC Pre-training"))

colors = RColorBrewer::brewer.pal(4, "Paired")[1:4]
colors_select = c(colors[2], colors[4])

p2da<-ggplot(subset(ptdata, Threshold<=0.99),aes(x=Threshold, y=Prec*100)) + facet_wrap(~Model,ncol=3) + 
  geom_line(aes(color=Source),linewidth=1) +
  labs(x="Threshold", y="Precision",color="Test source") +
  theme_bw() +
  scale_colour_manual(values=colors_select)
dev.new();print(p2da)

#ggsave(paste0(pathToLocalGit,"figures/Pre-training_precision_vs_threshold.png"), width = 7, height = 3, units = "in", dpi=600)

##########################
## FIGURE 5 - Soundscape PR curves of models with BirdNET

# setup plot data
prplot <- (npdata %>% filter(Source %in% c("Soundscape - XC Pre-training","Soundscape - BirdNET geographic","Soundscape - BirdNET non-geographic")))[,c("Model","Sens","Prec","Threshold","Source")]
prplot[prplot$Source == "Soundscape - BirdNET geographic",]$Model <- "BirdNET geographic"
prplot[prplot$Source == "Soundscape - BirdNET non-geographic",]$Model <- "BirdNET non-geographic"


birdnetStats <- data.frame()
for (t in unique(prplot$Threshold)){
  d <- subset(prplot,Threshold == t)
  d$PrecDiffBNGeog <- d$Prec - d[d$Model == "BirdNET geographic"]$Prec
  d$SensDiffBNGeog <- d$Sens - d[d$Model == "BirdNET geographic"]$Sens
  d$PrecDiffBNNonGeog <- d$Prec - d[d$Model == "BirdNET non-geographic"]$Prec
  d$SensDiffBNNonGeog <- d$Sens - d[d$Model == "BirdNET non-geographic"]$Sens
  birdnetStats <- rbind(birdnetStats,d)
}
#write.csv(birdnetStats,paste0(pathToLocalGit,"data/birdnet_prediction_statistics.csv"),row.names=F)

colors = RColorBrewer::brewer.pal(5, "Set2")[1:5]
colors_select = c(colors[5],colors[4],colors[1], colors[2], colors[3])

p1<-ggplot(subset(prplot, Threshold<=0.99),aes(x=Threshold, y=Prec*100)) + 
  geom_line(aes(color=Model),linewidth=1) +
  labs(x="Threshold", y="Precision") + theme_bw() +
  #scale_colour_brewer(palette = "Set2") +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=colors_select) +
  expand_limits(x = c(0.6,1), y = c(0,80)) +
  scale_x_continuous(breaks=seq(0.6,1.0,0.1)) +
  scale_y_continuous(breaks=seq(0,80,10))
p2<-ggplot(subset(prplot, Threshold<=0.99999),aes(x=Sens*100, y=Prec*100)) + 
  geom_line(aes(color=Model),linewidth=1) +
  labs(x="Recall", y="Precision") + theme_bw() +
  #scale_colour_brewer(palette = "Set2") +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=colors_select) +
  expand_limits(x = c(0,80), y = c(0,80)) +
  scale_x_continuous(breaks=seq(0,80,10)) +
  scale_y_continuous(breaks=seq(0,80,10))
p3 <- ggarrange(p1, p2,
                labels = c("A", "B"),
                common.legend = TRUE,
                legend = "bottom",
                ncol = 2, nrow = 1) 
dev.new();print(p3)

#ggsave(paste0(pathToLocalGit,"figures/models_birdnet.png"), width = 7, height = 3, units = "in", dpi=600)

####################################
# Optimal GV model per species based on max F0.5
# This requires calculation of indices at the species level, not for the model as above
# So, we re-run the summarizations, but with the argument bySpecies="yes" and only for the XC pre-trained models with GV data
# But first we need to assign a SpeciesCode to FN (and to FP for not-present), which above were given an NA (i.e., present in the Soundscape or RI data but not in predictions)
ptGVmatches$SpeciesCode<-ifelse(is.na(ptGVmatches$SpeciesCode),ptGVmatches$GVspeciesCode,ptGVmatches$SpeciesCode)
gvStats<-summarizeByHurdle(allmatches=ptGVmatches,bySpecies="yes",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
names(gvStats)<-gsub("hurdle","Threshold",names(gvStats))
names(gvStats)<-gsub("Fbeta","F05v",names(gvStats))
#write.csv(subset(gvStats, truePos>0),paste0(pathToLocalGit,"data/soundscape_prediction_statistics.csv"),row.names=F)

# Find optimal model based on soundscape max F0.05 statistics, only for XC pre-trained models
gvF05Stats <- data.frame()
soundscapeOptimalStats <- data.frame()
allSpecies <- unique(gvStats$SpeciesCode)
allSpecies <- allSpecies[order(allSpecies)]
for (species in allSpecies){
  statsSpecies <- gvStats %>% filter(SpeciesCode == species)
  maxF05 <- group_by(statsSpecies, ModelName) %>% summarise(maxF05 = max(F05v,na.rm = TRUE)) %>% filter(!is.na(maxF05))
  if (dim(maxF05)[1] > 0){
    maxF05$SpeciesCode <- species
	  gvF05Stats<-rbind(gvF05Stats,maxF05)
    bestF05 <- max(maxF05$maxF05)
    bestModel <- maxF05[maxF05$maxF05==bestF05,1]
    bestStats <- statsSpecies %>% filter((ModelName == pull(bestModel, ModelName)) & F05v == bestF05)
    if (dim(bestStats)[1] > 1) {
      bestStats <- bestStats[bestStats$Threshold == min(bestStats$Threshold),] # if more than one with same max F05, pick lowest threshold
    } 
	soundscapeOptimalStats<-rbind(soundscapeOptimalStats,bestStats[1,]) # if more than 2 models with same stats, picks first based on alphabetic order (lower complexity, in this case)
  }
}
soundscapeOptimalStats$SpeciesCode<-allSpecies
soundscapeOptimalStats<-subset(soundscapeOptimalStats, truePos>0)
nrow(soundscapeOptimalStats)==37
#write.csv(soundscapeOptimalStats,paste0(pathToLocalGit,"data/soundscape_prediction_optimal_statistics.csv"),row.names=F)

###############
# Figure 6

speciesSelect <- c("CAVI","CAQU","WEME")
mxBetabySp<-subset(gvStats, SpeciesCode %in% speciesSelect)

## Need the mdf of the maxBetas...
mdf<-subset(soundscapeOptimalStats, SpeciesCode %in% speciesSelect)

pF<-ggplot(subset(mxBetabySp, Threshold <=0.9999),aes(x=Threshold,y=F05v)) + geom_line(aes(color=ModelName),linewidth=1) + facet_wrap(~SpeciesCode,ncol=3) +
  geom_point(data = mdf,aes(x=Threshold,y=F05v),size=2) +
  geom_point(data = mdf,aes(x=Threshold,y=F05v),size=2) +
  theme_bw() + labs(x="Threshold",y=paste0("F(","\u03b2","=0.5)"),color="Model") +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks=seq(0.6,1.0,.1)) +
  scale_y_continuous(breaks=seq(0.2,1.0,.1))
dev.new();print(pF)

#ggsave(paste0(pathToLocalGit,"figures/three_species_optimal_threshold_GV.png"), width = 7, height = 3, units = "in", dpi=600)

###############
# Figure 7
mdpdf<-subset(gvF05Stats,maxF05>0)
mdp<-ggplot(mdpdf,aes(x=SpeciesCode,y=maxF05)) + geom_point(aes(color=ModelName,size=ModelName)) + 
  coord_flip() +
  theme_bw() + labs(x="",y=paste0("F(","\u03b2","=0.5)"),color="Model") +
  theme(axis.text.y = element_text(size = 6)) +
  scale_colour_brewer(palette = "Set2") +
  scale_size_manual(values=c(2,2,2)) +
  scale_x_discrete(limits = rev(unique(sort(mdpdf$SpeciesCode)))) +
  scale_y_continuous(breaks=seq(0,1.0,.1)) +
  guides(size = F)
dev.new();print(mdp)
#ggsave(paste0(pathToLocalGit,"figures/GV Species MaxF05.png"), width = 7, height = 5, units = "in", dpi=600)

############
# Figure 8
## New version: Use optimal threshold from GV for maxF05, and for the 17 missing use MobileNet and mean threshold (0.76), use these on ROI data for figure 8

##get the optimal threshold and model for the 37 GV species, and MobileNet and mean threshold (0.76) for the other 17
#evaluate with this threshold against ROI data
Species<-unique(roiTestdf$birdcode)
addSpecies<-Species[which(!Species %in% soundscapeOptimalStats$SpeciesCode)]
optimModelThreshold<-soundscapeOptimalStats[,c("SpeciesCode","ModelName","Threshold")]
modalModel<-names(which.max(table(soundscapeOptimalStats$ModelName)))
meanThreshold<-mean(soundscapeOptimalStats[soundscapeOptimalStats$ModelName == modalModel,]$Threshold)
addOMT<-data.frame(SpeciesCode=addSpecies,ModelName=modalModel,Threshold=meanThreshold)
optimModelThreshold<-rbind(optimModelThreshold,addOMT)
optimModelThreshold<-optimModelThreshold[order(optimModelThreshold$SpeciesCode),]

# This requires calculation of indices at the species level, for ROI data
# So, we re-run the summarizations, but with the argument bySpecies="yes" and only for the XC pre-trained models with ROI data
roiSppStats<-summarizeByHurdle(allmatches=ptROImatches,bySpecies="yes",summarizeToSample,summarizeToEvent,sumLevel="clip",beta=0.5,addEvent=FALSE)
names(roiSppStats)<-gsub("Fbeta","F05v",names(roiSppStats))
#write.csv(roiSppStats,paste0(pathToLocalGit,"data/roi_prediction_statistics.csv"),row.names=F)

## Now we get the performance metrics for each species, best model, and optimal threshold...
roiOptimalStats<-ldply(1:nrow(optimModelThreshold), function(rr,optimModelThreshold,roiSppStats){
			spp<-optimModelThreshold[rr,"SpeciesCode"]
			mm<-optimModelThreshold[rr,"ModelName"]
			tt<-optimModelThreshold[rr,"Threshold"]
			rdf<-subset(roiSppStats, SpeciesCode==spp & ModelName==mm)
			rdf<-rdf[which.min(abs(rdf$hurdle - tt)),]
			return(rdf)
		},optimModelThreshold=optimModelThreshold,roiSppStats=roiSppStats)

## Let's add other Fbeta values...
getFvalbeta<-function(df,beta){
	fv<-(1+(beta^2))*(df$Prec*df$Sens)/(((beta^2)*df$Prec) + df$Sens)
	return(fv)
}

roiOptimalStats$F075v<-as.numeric(getFvalbeta(df=roiOptimalStats,beta=0.75))

#Add the soundscape optimal stats
names(soundscapeOptimalStats)<-gsub("Fbeta","F05v",names(soundscapeOptimalStats))
soundscapeOptimalStats$F075v<-as.numeric(getFvalbeta(df=soundscapeOptimalStats,beta=0.75))
soundscapeOptimalStats$type<-"Soundscape"
names(roiOptimalStats)<-gsub("hurdle","Threshold",names(roiOptimalStats))
roiOptimalStats$type<-"ROI"
optimalStats<-rbind(roiOptimalStats,soundscapeOptimalStats)
optimalStats$gvOpt = "F0.5 optimized"
optimalStats[optimalStats$SpeciesCode %in% addSpecies,]$gvOpt = "Mean F0.5 from MobileNet"

pal <- RColorBrewer::brewer.pal(4, "Set2")

roisoundscape1<-ggplot(optimalStats,aes(x=SpeciesCode,y=Prec*100)) + 
  geom_point(aes(color=type,shape=gvOpt),size=2) +
  geom_hline(yintercept = mean(roiOptimalStats$Prec*100, na.rm = TRUE),color="black") +
  geom_hline(yintercept = mean(soundscapeOptimalStats$Prec*100, na.rm = TRUE),color="black",linetype=2) + 
  coord_flip() +
  theme_bw() + labs(x="",y="Precision") +
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size = 6)) +
  font("legend.text", size = 12) +
  scale_color_manual(values=pal[c(3,4)]) +
  scale_x_discrete(limits = rev(unique(sort(roiOptimalStats$SpeciesCode)))) + 
  scale_y_continuous(breaks=seq(0,100,10)) 
#dev.new();print(roisoundscape1)

roisoundscape2<-ggplot(optimalStats,aes(x=SpeciesCode,y=Sens*100)) + 
  geom_point(aes(color=type,shape=gvOpt),size=2) +
  geom_hline(yintercept = mean(roiOptimalStats$Sens*100, na.rm = TRUE),color="black") + 
  geom_hline(yintercept = mean(soundscapeOptimalStats$Sens*100, na.rm = TRUE),color="black",linetype=2) + 
  coord_flip() +
  theme_bw() + labs(x="",y="Recall") +
  theme(legend.title=element_blank()) +
  theme(axis.text.y = element_text(size = 6)) +
  font("legend.text", size = 12) +
  scale_color_manual(values=pal[c(3,4)]) +
  scale_x_discrete(limits = rev(unique(sort(roiOptimalStats$SpeciesCode)))) + 
  scale_y_continuous(breaks=seq(0,100,10)) 
#dev.new();print(roisoundscape2)

roisoundscape3<- ggarrange(roisoundscape1, roisoundscape2,
                           labels = c("A", "B"),
                           common.legend = TRUE,
                           legend = "bottom",
                           ncol = 2, nrow = 1) 
dev.new();print(roisoundscape3)

#ggsave(paste0(pathToLocalGit,"figures/ROI Soundscape Species Optimal MaxF05.png"), width = 7, height = 6, units = "in", dpi=600)
#write.csv(optimalStats,paste0(pathToLocalGit,"data/optimal_statistics.csv"),row.names=F)

############
# Figure 9

abgData <- read.csv(paste0(pathToLocalGit,"data/soundscape_abg.csv"))
abgData$Species <- ifelse(!abgData$SpeciesCode == 0,abgData$SpeciesCode,abgData$GVspeciesCode)

gvSpecies <- soundscapeOptimalStats$SpeciesCode
abgDataSub <- abgData %>%
  select(match,Species,Anthropophony, Biophony, Geophony, Interference, Quiet) %>%
  filter(Species %in% gvSpecies)

abgDataSum <- abgDataSub %>% 
  group_by(match,Species) %>%
  summarise(across(Anthropophony:Quiet, ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
  pivot_longer(
  cols = Anthropophony:Quiet,
  names_to = "ABGQI", 
  values_to = "count")

abgPlot <- ggplot(abgDataSum, aes(fill=ABGQI, y=count, x=match)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw() + labs(x="",y="Number of ABGQI detections") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels=c('False Negative', 'False Positive', 'True Positive')) +
  guides(fill=guide_legend(title="Soundscape components"))
dev.new();print(abgPlot)

#ggsave(paste0(pathToLocalGit,"figures/ABGQI.png"), width = 7, height = 3, units = "in", dpi=600)

############
# Table 2

# Model architecture	Maximum F0.5	Precision	Recall
# Just species found in GV data
optimalStatsGVspp <- subset(optimalStats, gvOpt == "GVF05optimized")
optimalStatsSummaryAll <- optimalStatsGVspp %>% group_by(type) %>%
  summarise_at(.vars = names(.)[c(6,7,10,14)], c(Mean="mean", Sd="sd"), na.rm = TRUE)
optimalStatsSummaryAll$ModelName = "All"

optimalStatsSummary <- optimalStatsGVspp %>% group_by(type,ModelName) %>%
  summarise_at(.vars = names(.)[c(6,7,10,14)], c(Mean="mean", Sd="sd"), na.rm = TRUE)

optimalStatsSummary <- rbind(optimalStatsSummary,optimalStatsSummaryAll)

#write.csv(optimalStatsSummary,paste0(pathToLocalGit,"data/roi_soundscape_optimal_stats_summary.csv"),row.names=F)


