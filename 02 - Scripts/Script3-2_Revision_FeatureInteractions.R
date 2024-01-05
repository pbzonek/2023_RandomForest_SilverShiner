## --------------------------------------------------------------#
## Script name: Script3-2_Revision_FeatureInteractions.R 
##
## Purpose of script: 
##      Investigate interactions between predictors
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-07-06
##
## --------------------------------------------------------------#  
## Modification Notes:The packages loaded here have LOTS of dependencies   
##   and the code to remove the dependencies is a little sketchy. If 
##   issues pop up after running this script, try avoiding package 'iml'
## --------------------------------------------------------------#

library(iml, include.only = c('Predictor', 'Interaction')) 
plot_interactions <- list() #Make list to hold plots

#####Combined Life Stage##########################################----
#-------------------------------------------------------------#

#----------------------------#
###Interpret model variables with 'iml'
#----------------------------#
#Create a model object
RF_F_C_int_predictor <- iml::Predictor$new(RF_F_C_model, data = dplyr::select(RF_data_C_test, -SSp), 
                                    y=RF_data_C_test$SSp)
#Measure the interaction strength
RF_F_C_int_Interaction <- iml::Interaction$new(RF_F_C_int_predictor) #OPTIONAL: Specify interactions: feature="FishDiversity"

#Plot H value of variables or interaction terms 
RF_F_C_int_Interaction$results %>%  
  dplyr::group_by(.feature) %>% 
  dplyr::summarise(H=mean(.interaction)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_histogram(stat="identity", col="black", fill="brown", alpha=0.4)+
  xlab("Interaction")+ylab("H value")+
  theme(axis.text.x = element_text(angle = 90))


#----------------------------#
###Interpret model variable interactions with 'iml'
#----------------------------#
#Create a model object
RF_F_C_int_predictor <- iml::Predictor$new(RF_F_C_model, data = dplyr::select(RF_data_C_test, -SSp), 
                                    y=RF_data_C_test$SSp)
RF_F_C_int_Interaction_List <- data.frame()
# loopPredictors <- c(
#   "Month", "Year", "Water.Temperature", "Conductivity", "Dissolved.Oxygen", "pH", "AvgOfStream.depth..m.",
#   "AvgOfWater.velocity..msec.", "Stream.Width..m.", "Bank.Slope....", "Channel.Cover....",
#   "substrate_Organic", "substrate_Clay", "substrate_Silt", "substrate_Sand",
#   "substrate_Gravel", "substrate_Cobble", "substrate_Boulder", "substrate_Bedrock",
#   "aquaticveg_Emergent", "aquaticveg_Submerged", "riparianveg_Deciduous", "riparianveg_Herbaceous",
#   "riparianveg_shrubs", "riparianveg_None")

loopPredictors <- c(
  "AvgOfStream.depth..m.","AvgOfWater.velocity..msec.",
  "Month", "Year", "Water.Temperature", "Conductivity",
  "riparianveg_None", "substrate_Silt","substrate_Organic","substrate_Boulder"
)

for(i in unique(loopPredictors)){
  print(i)
  #Measure the interaction strength
  RF_F_C_int_Interaction <- iml::Interaction$new(RF_F_C_int_predictor, feature= i) #OPTIONAL: Specify interactions: feature="FishDiversity"
  #Plot H value of variables or interaction terms 
  df_loopA <-RF_F_C_int_Interaction$results %>%  
    dplyr::group_by(.feature) %>% 
    dplyr::summarise(H=mean(.interaction)) %>% 
    as.data.frame() 
  RF_F_C_int_Interaction_List <- rbind(RF_F_C_int_Interaction_List, df_loopA)
}; rm(df_loopA)



#----------------------------#
###Identify and plot important bivariate interactions
#----------------------------#
###Find and rank bivriate interactions
plot_interactions$Lolipop_RF_F_C <- RF_F_C_int_Interaction_List %>% 
  # filter(.feature %in%
  #          c("Dominant.Substrate:Macrophytes.Submergent", "Water.Temp:Dominant.Substrate", 
  #            "widthm:Max.Depth", "DO:Conductivity", "Pool.or.Riffle:Conductivity",
  #            "Water.Temp:Conductivity","DO:Macrophytes.Submergent",
  #            "BenthicAbundance:widthm")) %>% 
  
  arrange(desc(H)) %>%  # Arrange by Column A in descending order
  slice(1:10) %>%  # Select the top 10 rows

  ggplot(aes(y=H, x= reorder(.feature, H)))+
    geom_hline(yintercept = 0)+
    geom_segment(aes(x=reorder(.feature, H),
                     xend=reorder(.feature, H), y=0, yend=H))+
    geom_point(aes(fill=H), col="black", size=4.5, pch=21)+
    coord_flip()+
    ylab("H-value")+ xlab("Predictor")+
    theme_bw()+
    theme(legend.position="none",
          #axis.text.y=element_text(
          #face=c(replace(rep("plain", 8), c(4,6,8), "bold")), #Embolden key features
          #size=c(replace(rep(9, 8), c(4,6,8), 11))) #Increase size
    )+ 
    labs(title = "A. Combined Life Stage") 
plot_interactions$Lolipop_RF_F_C




#####Juvenile Life Stage##########################################----
#-------------------------------------------------------------#

#----------------------------#
###Interpret model variables with 'iml'
#----------------------------#
#Create a model object
RF_F_J_int_predictor <- iml::Predictor$new(RF_F_J_model, data = dplyr::select(RF_data_J_test, -immaturep), 
                                        y=RF_data_J_test$immaturep)
#Measure the interaction strength
RF_F_J_int_Interaction <- iml::Interaction$new(RF_F_J_int_predictor) #OPTIONAL: Specify interactions: feature="FishDiversity"

#Plot H value of variables or interaction terms 
RF_F_J_int_Interaction$results %>%  
  dplyr::group_by(.feature) %>% 
  dplyr::summarise(H=mean(.interaction)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_histogram(stat="identity", col="black", fill="brown", alpha=0.4)+
  xlab("Interaction")+ylab("H value")+
  theme(axis.text.x = element_text(angle = 90))


#----------------------------#
###Interpret model variable interactions with 'iml'
#----------------------------#
#Create a model object
RF_F_J_int_predictor <- iml::Predictor$new(RF_F_J_model, data = dplyr::select(RF_data_J_test, -immaturep), 
                                        y=RF_data_J_test$immaturep)
RF_F_J_int_Interaction_List <- data.frame()
# loopPredictors <- c(
#   "Month", "Year", "Water.Temperature", "Conductivity", "Dissolved.Oxygen", "pH", "AvgOfStream.depth..m.",
#   "AvgOfWater.velocity..msec.", "Stream.Width..m.", "Bank.Slope....", "Channel.Cover....",
#   "substrate_Organic", "substrate_Clay", "substrate_Silt", "substrate_Sand",
#   "substrate_Gravel", "substrate_Cobble", "substrate_Boulder", "substrate_Bedrock",
#   "aquaticveg_Emergent", "aquaticveg_Submerged", "riparianveg_Deciduous", "riparianveg_Herbaceous",
#   "riparianveg_shrubs", "riparianveg_None")

loopPredictors <- c(
  "AvgOfStream.depth..m.","AvgOfWater.velocity..msec.",
  "Month", "Year", "Water.Temperature", "Conductivity",
  "riparianveg_None", "substrate_Silt","substrate_Organic","substrate_Boulder"
)

for(i in unique(loopPredictors)){
  print(i)
  #Measure the interaction strength
  RF_F_J_int_Interaction <- iml::Interaction$new(RF_F_J_int_predictor, feature= i) #OPTIONAL: Specify interactions: feature="FishDiversity"
  #Plot H value of variables or interaction terms 
  df_loopA <-RF_F_J_int_Interaction$results %>%  
    dplyr::group_by(.feature) %>% 
    dplyr::summarise(H=mean(.interaction)) %>% 
    as.data.frame() 
  RF_F_J_int_Interaction_List <- rbind(RF_F_J_int_Interaction_List, df_loopA)
}; rm(df_loopA)



#----------------------------#
###Identify and plot important bivariate interactions
#----------------------------#
###Find and rank bivriate interactions
plot_interactions$Lolipop_RF_F_J <- RF_F_J_int_Interaction_List %>% 
  # filter(.feature %in%
  #          c("Dominant.Substrate:Macrophytes.Submergent", "Water.Temp:Dominant.Substrate", 
  #            "widthm:Max.Depth", "DO:Conductivity", "Pool.or.Riffle:Conductivity",
  #            "Water.Temp:Conductivity","DO:Macrophytes.Submergent",
  #            "BenthicAbundance:widthm")) %>% 
  
  arrange(desc(H)) %>%  # Arrange by Column A in descending order
  slice(1:10) %>%  # Select the top 10 rows
  
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=reorder(.feature, H),
                   xend=reorder(.feature, H), y=0, yend=H))+
  geom_point(aes(fill=H), col="black", size=4.5, pch=21)+
  coord_flip()+
  ylab("H-value")+ xlab("Predictor")+
  theme_bw()+
  theme(legend.position="none",
        #axis.text.y=element_text(
        #face=c(replace(rep("plain", 8), c(4,6,8), "bold")), #Embolden key features
        #size=c(replace(rep(9, 8), c(4,6,8), 11))) #Increase size
  )+ 
  labs(title = "B. Juvenile Life Stage") 
plot_interactions$Lolipop_RF_F_J


 
#####Adult Life Stage##########################################----
#-------------------------------------------------------------#

#----------------------------#
###Interpret model variables with 'iml'
#----------------------------#
#Create a model object
RF_F_A_int_predictor <- iml::Predictor$new(RF_F_A_model, data = dplyr::select(RF_data_A_test, -maturep), 
                                        y=RF_data_A_test$maturep)
#Measure the interaction strength
RF_F_A_int_Interaction <- iml::Interaction$new(RF_F_A_int_predictor) #OPTIONAL: Specify interactions: feature="FishDiversity"

#Plot H value of variables or interaction terms 
RF_F_A_int_Interaction$results %>%  
  dplyr::group_by(.feature) %>% 
  dplyr::summarise(H=mean(.interaction)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_histogram(stat="identity", col="black", fill="brown", alpha=0.4)+
  xlab("Interaction")+ylab("H value")+
  theme(axis.text.x = element_text(angle = 90))


#----------------------------#
###Interpret model variable interactions with 'iml'
#----------------------------#
#Create a model object
RF_F_A_int_predictor <- iml::Predictor$new(RF_F_A_model, data = dplyr::select(RF_data_A_test, -maturep), 
                                        y=RF_data_A_test$maturep)
RF_F_A_int_Interaction_List <- data.frame()
# loopPredictors <- c(
#   "Month", "Year", "Water.Temperature", "Conductivity", "Dissolved.Oxygen", "pH", "AvgOfStream.depth..m.",
#   "AvgOfWater.velocity..msec.", "Stream.Width..m.", "Bank.Slope....", "Channel.Cover....",
#   "substrate_Organic", "substrate_Clay", "substrate_Silt", "substrate_Sand",
#   "substrate_Gravel", "substrate_Cobble", "substrate_Boulder", "substrate_Bedrock",
#   "aquaticveg_Emergent", "aquaticveg_Submerged", "riparianveg_Deciduous", "riparianveg_Herbaceous",
#   "riparianveg_shrubs", "riparianveg_None")

loopPredictors <- c(
  "AvgOfStream.depth..m.","AvgOfWater.velocity..msec.",
  "Month", "Year", "Water.Temperature", "Conductivity",
  "riparianveg_None", "substrate_Silt","substrate_Organic","substrate_Boulder"
)

for(i in unique(loopPredictors)){
  print(i)
  #Measure the interaction strength
  RF_F_A_int_Interaction <- iml::Interaction$new(RF_F_A_int_predictor, feature= i) #OPTIONAL: Specify interactions: feature="FishDiversity"
  #Plot H value of variables or interaction terms 
  df_loopA <-RF_F_A_int_Interaction$results %>%  
    dplyr::group_by(.feature) %>% 
    dplyr::summarise(H=mean(.interaction)) %>% 
    as.data.frame() 
  RF_F_A_int_Interaction_List <- rbind(RF_F_A_int_Interaction_List, df_loopA)
}; rm(df_loopA)



#----------------------------#
###Identify and plot important bivariate interactions
#----------------------------#
###Find and rank bivriate interactions
plot_interactions$Lolipop_RF_F_A <- RF_F_A_int_Interaction_List %>% 
  # filter(.feature %in%
  #          c("Dominant.Substrate:Macrophytes.Submergent", "Water.Temp:Dominant.Substrate", 
  #            "widthm:Max.Depth", "DO:Conductivity", "Pool.or.Riffle:Conductivity",
  #            "Water.Temp:Conductivity","DO:Macrophytes.Submergent",
  #            "BenthicAbundance:widthm")) %>% 
  
  arrange(desc(H)) %>%  # Arrange by Column A in descending order
  slice(1:10) %>%  # Select the top 10 rows
  
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=reorder(.feature, H),
                   xend=reorder(.feature, H), y=0, yend=H))+
  geom_point(aes(fill=H), col="black", size=4.5, pch=21)+
  coord_flip()+
  ylab("H-value")+ xlab("Predictor")+
  theme_bw()+
  theme(legend.position="none",
        #axis.text.y=element_text(
        #face=c(replace(rep("plain", 8), c(4,6,8), "bold")), #Embolden key features
        #size=c(replace(rep(9, 8), c(4,6,8), 11))) #Increase size
  )+ 
  labs(title = "C. Adult Life Stage") 
plot_interactions$Lolipop_RF_F_A
 


#####Combine Plots################################################----
#-------------------------------------------------------------#
with(plot_interactions,
     Lolipop_RF_F_C + Lolipop_RF_F_J + Lolipop_RF_F_A)


#plot_interactions$Lolipop_RF_F_C + plot_interactions$Lolipop_RF_F_J + plot_interactions$Lolipop_RF_F_A

# wrap_elements(full = Lolipop_RF_F_C)
# /
#   (B1 | B2 | B3)
# +plot_layout(#heights = c(1, 1.5), 
#   #guides = 'collect'
# )


#####Get rid of package:'iml' and its troublesum dependencies#####----
#-------------------------------------------------------------#
invisible({
  #Formally library all background loaded packages
  suppressPackageStartupMessages(
    lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE))
  #Detach all non-base packages
  lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE)
})

#Reload project packages
source("02 - Scripts/00 - Global/ScriptX-2_LoadPackages.R") #Load packages

