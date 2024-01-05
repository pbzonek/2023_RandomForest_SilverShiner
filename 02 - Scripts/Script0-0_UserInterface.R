## --------------------------------------------------------------#
## Script name: Script00_UserInterface.R 
##
## Purpose of script: 
##      Console to run all Silver Shiner habitat/community association scripts
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-07-06
##
## --------------------------------------------------------------#  
## Modification Notes:   
##   
## --------------------------------------------------------------#


#####Set global environment#######################################----
#-------------------------------------------------------------#

source("02 - Scripts/00 - Global/ScriptX-2_LoadPackages.R") #Load packages
data_DFO_site <- readRDS("01 - Data/data_DFO_SXMCreek_2024_01_02.rds")

#Format google map environment
register_google(key = "AIzaSyC7M4nwUb2wgOAPvn_g1hsWeN-3O0FfGz0")
SSmap <- get_googlemap(center = c(lon=-79.76, lat=43.48),
                       zoom = 13, size = c(640, 640), scale = 4,
                       maptype = c("satellite"))
SSmapBW <- get_googlemap(center = c(lon=-79.76, lat=43.48),
                       zoom = 11, size = c(640, 640), scale = 4,
                       maptype = c("satellite"),
                       color = "bw")
  


#####Analyse SS Association data##################################----
#-------------------------------------------------------------#
 
###Run Scripts
#----------------------------#
#Full Random Forest Models
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-1-1_RF_Full_Combined.R", print.eval = TRUE) #Combined life stage
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-1-2_RF_Full_Juvenile.R", print.eval = TRUE) #Juvenile
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-1-3_RF_Full_Adult.R", print.eval = TRUE) #Adult
#Lamothe Random Forest Models
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-2-1_RF_Lamothe_Combined.R", print.eval = TRUE) #Combined life stage
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-2-2_RF_Lamothe_JuvenileAdult.R", print.eval = TRUE) #Specific life stages
#Manually Picked Random Forest Models
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-3-1_RF_Manual_All.R", print.eval = TRUE) #All life stages
#Model Selection Random Forest Models
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-4-1_RF_ModelSelection_All.R", print.eval = TRUE) #All life stages
#Auxillary Scripts
source("02 - Scripts/02 - Fish_Hab_Assoc/Script1-5_AuxillaryScripts.R", print.eval = TRUE) #All life stages

###Make plots
#----------------------------#
source("02 - Scripts/02 - Fish_Hab_Assoc/Script2-1_Plots_Univariate.R", print.eval = TRUE) #Build univariate plot objects
source("02 - Scripts/02 - Fish_Hab_Assoc/Script2-2_Plots_Publication.R", print.eval = TRUE) #Print and save manuscript figures
source("02 - Scripts/02 - Fish_Hab_Assoc/Script2-3_Plots_Bivariate.R", print.eval = TRUE) #Print and save bivariate plots
source("02 - Scripts/02 - Fish_Hab_Assoc/Script2-4_Plots_Maps_Revision.R", print.eval = TRUE) #Make occurrence maps with reviewer revisions

###Make manuscript revisions
#----------------------------#
source("02 - Scripts/02 - Fish_Hab_Assoc/Script3-1_Revision_Basic.R", print.eval = TRUE) #Construct random forest decision trees
source("02 - Scripts/02 - Fish_Hab_Assoc/Script3-1_Revision_FeatureInteractions.R", print.eval = TRUE) #Construct random forest decision trees

 
 
 
 
 
 
 
 


