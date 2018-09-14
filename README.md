# Soundscapes2Landscapes Project

Code for the NASA Soundscapes to Landscapes project 
P-I: Matt Clark

Processing order:

Task - append new species to the allSpecies data sheet.
In: existing species file https://drive.google.com/open?id=1g85xVP03kjO4Gz9NJr7UOkOCOTtBj8zt
In: new species compilation [location, presence/absence]
File: appendNewSpecies_toAllSpecies.R
Out: updated sheet with a new appended species to the table

Task - compile all the geospatial covariates into a single stack, then subset based on variance inflation factor
In: the directory with the rasters of covariates
File: createStack_attributObs.R
Out: a file with the stack of covariates filtered for variance inflation

Task - fit imperfect detection models to the Arbimon classification results [not do]
In: a file with the result by date and time of the classification by Arbimon (presence or absence)
In: a file with the habitat type covariate used for stratifying the location of the recorders
File: estimateOccupancy_from_ArbimonOutput.R file
Out: estimates of probability of detection, of false positives, and of occupancy
Out: predicted occupancy

Task - fit the landscape model stack and predict to the covariate stack
In: A species presence/absence file by site - from volunteer data
In: covariate stack - from createStack_attributObs.R
File: fitDistModels_PA.R
Out: four predictive models and their support from test against OOB sample
Out: predicted raster that is the weighted average of all four predictive models

Task - estiamte Goodness-of-fit of landscape models, rank variable importance, generate plots of RMSE and variable importance
In: fitted landscape models (see file: fitDistModels_PA.R)
In: test dataset (see file: fitDistModels_PA.R)
File: getGOF_VI_otherPlots.R
Out: Plot of RMSE values for each species and landscape model fit
Out: AUC estimates (table) for each species and landscape model fit 
Out: Variable importance table for each landscape model fit for each species
Out: Plot of variable importance composition based on geospatial data source, for the top 10 variables in each landscape model of each species
