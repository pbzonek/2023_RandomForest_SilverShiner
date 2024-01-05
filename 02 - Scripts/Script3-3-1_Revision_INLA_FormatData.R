## --------------------------------------------------------------#
## Script name: Script1-1_FormatData.R 
##
## Purpose of script: 
##    Format data for life-stage specific 
##     INLA models testing for spatial autocorrelation 
##
## Author: Paul Bzonek
##
## Date Created: 2022-09-09
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#


#####Load packages and prep data##################################----
#-------------------------------------------------------------# 
library(INLA) #INLA models
library(tidyverse) #workflow and plots
library(rgdal) #Read shapefile
library(lattice) #make levelplot()
library(gstat) # Make Variogram
library(patchwork) #Combine ggplot objects
library(ggregplot) #Model comparison plots
library(magrittr) #Run ggregplot functions



#####Read in Raw data ############################################----
#-------------------------------------------------------------# 
#Make home for plots
plots_INLA <- list()

###Read in raw data
data_INLA_Full <- data_DFO_site
data_INLA_Full <- data_INLA_Full %>% 
 dplyr::arrange(Date) %>% 
 mutate(SiteID = as.factor(paste(Start.Latitude, Start.Longitude, sep = "_")),
        SSp = as.numeric(SSp)-1, #Make SSp numeric for easy inla coding
        #Year = as.numeric(Year)
 ) 

table(table(data_INLA_Full$SiteID)) # Not enough repeat samples for a mixed model

data_INLA_Full <- data_INLA_Full %>%
 group_by(SiteID) %>% 
 slice_tail()


###Read and Format custom Shapefile
#---------------------#
SXM_shapefile <- readOGR(dsn = file.path(                       
 "01 - Data/shapefile_SixteenMileCreek/Bzonek_SixteenMileCreek.shp"), stringsAsFactors = F)
SXM_shapefile <- spTransform(SXM_shapefile, CRS("+proj=longlat +datum=WGS84")) #Tranform projection
plot(SXM_shapefile)

#specify a boundary for INLA mesh outputs
SXM_bdry1 <- inla.sp2segment(SXM_shapefile)
SXM_bdry1$loc <- inla.mesh.map(SXM_bdry1$loc)


