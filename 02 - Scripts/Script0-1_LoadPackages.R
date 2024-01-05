## --------------------------------------------------------------#
## Script name: ScriptX-2_LoadPackages.R 
##
## Purpose of script: 
##    Install and load all packages needed to run project scripts   
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-06-06
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#

#List the packages needed in Fast Alt Est
list.of.packages <- c(
  "raster", #problematic packages go first
  "tidyverse", "ggmap", "plotly", "sjPlot", "ggsn",  "caret", "pdp", 
  "ROCR", "pROC", "lme4", "data.table", "patchwork", "rgdal", "leaflet", 
  "rio", "readxl", "emmeans", "lubridate", "vegan", "randomForest", "corrplot", "rfUtilities")

#Identify packages in the last that are not on the computer
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#Install packages in "new.packages"
if(length(new.packages)) install.packages(new.packages); rm(list.of.packages); rm(new.packages)

#Problematic packages go first
library(raster)

#Normal packages
library(tidyverse)
library(ggmap)
library(plotly)
library(ggsn)
library(caret)
library(pdp)
library(ROCR)
library(pROC)
library(lme4)
library(data.table)
library(patchwork)
#library(sp)
#library(rgdal)
library(leaflet)

library(rio) #Import lists of files
library(readxl) #Real excel files
library(emmeans) #Plot marginal means
library(lubridate) #Manipulate dates
library(vegan) #Community diversity metrics
library(corrplot) #Make correlation plots
library(randomForest) #Random Forest decision trees
library(rfUtilities) #Random Forest Model Selection


theme_set(theme_classic()) #Set global ggplot theme
options(scipen=999) #Remove scientific notation


###To get bibliography for packages:
# library(bibtex)
# write.bib(c('base', 'tidyverse', 'ggmap',
#             'randomForest','caret', 'pdp', 'iml'), 
#           file="01 - Data/PackageBib.bib")


