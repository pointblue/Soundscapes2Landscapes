# Performs logistic regression mixed-effect models testing the relationship between soundscape bird detection
# FP, FN, and TP and soundscape components (biophony, anthropophony, geophony, inteference, quiet), treated as fixed effects. 
# Species and recordings are random effects.

# AUTHOR:
# Dr. Matthew Clark
# Dept of Geography, Environment, and Planning
# Sonoma State University
# matthew.clark@sonoma.edu

# Version: January 19, 2023; R 4.2.2

library(dplyr)
library(lme4)

# Point to your local cloned repository
pathToLocalGit<-"[insert local path here]/" 

abgData <- read.csv(paste0(pathToLocalGit,"data/soundscape_abg.csv"))
abgData$Species <- ifelse(!abgData$SpeciesCode == 0,abgData$SpeciesCode,abgData$GVspeciesCode)

gvSpecies <-c("ACWO","AMCR","AMRO","BEWR","BGGN","BHGR","BTYW","CALT","CAQU","CASJ","CAVI","CBCH","CORA","COYE","DEJU",
  "EUCD","HOFI","MAWR","MODO","MOUQ","OATI","OCWA","PAWR","PSFL","RWBL","SAVS","SOSP","SPTO","STJA","WAVI",
  "WBNU","WCSP","WEME","WETA","WITU","WIWA","WREN")

abgDataSub <- abgData %>%
  select(SamplingEvent,match,Species,Anthropophony, Biophony, Geophony, Interference, Quiet) %>%
  filter(Species %in% gvSpecies)

abgDataSub$FP <- ifelse(abgDataSub$match == "FALSEPOS",1,0)
abgDataSub$TP <- ifelse(abgDataSub$match == "TRUEPOS",1,0)
abgDataSub$FN <- ifelse(abgDataSub$match == "FALSENEG",1,0)

# True positive logistic regression
abglogitTP <- glmer(TP ~ (1|Species) + (1|SamplingEvent) + Anthropophony + Biophony + Geophony + Interference + Quiet, data = abgDataSub, family = "binomial", verbose=1)
summary(abglogitTP)

# False positive logistic regression
abglogitFP <- glmer(FP ~ (1|Species) + (1|SamplingEvent) + Anthropophony + Biophony + Geophony + Interference + Quiet, data = subset(abgDataSub,match != "FALSENEG"), family = "binomial", verbose=1)
summary(abglogitFP)

# False negative logistic regression
abglogitFN <- glmer(FN ~ (1|Species) + (1|SamplingEvent) + Anthropophony + Biophony + Geophony + Interference + Quiet, data = subset(abgDataSub,match != "FALSEPOS"), family = "binomial", verbose=1)
summary(abglogitFN)
