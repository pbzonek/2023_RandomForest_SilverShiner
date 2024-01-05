## --------------------------------------------------------------#
## Script name: Script3-1_Revision_Basic.R 
##
## Purpose of script: 
##      Make reviewer revisions 
##           - look at data structure
##           - look at correlations
##
## Author: Paul Bzonek
##
## Date Created: 2023-01-27
##
## --------------------------------------------------------------#  
## Modification Notes:
## --------------------------------------------------------------#

#####Use new package to produce summary of working dataset########----
#-------------------------------------------------------------#
library(gtExtras)
library(svglite)

data_DFO_site %>% 
 select(
    SSp, Month, Year, Water.Temperature, Conductivity, Dissolved.Oxygen, pH, AvgOfStream.depth..m., 
    AvgOfWater.velocity..msec., Stream.Width..m., Bank.Slope...., Channel.Cover...., 
    substrate_Organic, substrate_Clay, substrate_Silt, substrate_Sand, 
    substrate_Gravel, substrate_Cobble, substrate_Boulder, substrate_Bedrock, 
    aquaticveg_Emergent, aquaticveg_Submerged, riparianveg_Deciduous, riparianveg_Herbaceous, 
    riparianveg_shrubs, riparianveg_None) %>% 
  gtExtras::gt_plt_summary()


#####Look at correlations ########################################----
#-------------------------------------------------------------#
#Important variables
data_DFO_site %>% 
  select(Month, Year, 
         Water.Temperature, 
         AvgOfStream.depth..m., AvgOfWater.velocity..msec., Stream.Width..m.,
         riparianveg_None) %>% 
  mutate(Month = as.numeric(Month),
         Year = as.numeric(Year)) %>% 
  na.omit() %>% 
  cor() 

#All variables
df_cor <- data_DFO_site %>% 
  select(Month, Year, Water.Temperature, Conductivity, Dissolved.Oxygen, pH, AvgOfStream.depth..m., 
         AvgOfWater.velocity..msec., Stream.Width..m., Bank.Slope...., Channel.Cover...., 
         substrate_Organic, substrate_Clay, substrate_Silt, substrate_Sand, 
         substrate_Gravel, substrate_Cobble, substrate_Boulder, substrate_Bedrock, 
         aquaticveg_Emergent, aquaticveg_Submerged, riparianveg_Deciduous, riparianveg_Herbaceous, 
         riparianveg_shrubs, riparianveg_None) %>% 
  mutate(Month = as.numeric(Month),
         Year = as.numeric(Year)) %>% 
  na.omit() %>% 
  cor() 


corrplot(df_cor, method = 'number', order = 'AOE')
corrplot(df_cor, add = TRUE, type = 'lower', order = 'AOE',
         tl.pos = 'n', cl.pos = 'n')


#####Bar plot of monthly detections #################################----
#-------------------------------------------------------------#

data_DFO_site %>% 
  filter(!is.na(immaturep)) %>% 
  ggplot(aes(x=month(Date, label=TRUE), fill=))+
  geom_bar()+
  facet_wrap(~year(Date))


#####Spatial Autocorrelation #1  #################################----
#-------------------------------------------------------------#
library(nlme)

#Make working dataset
S_data <- data_DFO_site %>% 
  select(SSp, immaturep, maturep, Date,  #Pick columns
         Month, Year, Water.Temperature, 
         AvgOfStream.depth..m., AvgOfWater.velocity..msec., Stream.Width..m.,
         riparianveg_None, Start.Latitude, Start.Longitude) %>%  
  na.omit() %>% #Handle NAs
  dplyr::arrange(Date) %>% 
  mutate(SiteID = as.factor(paste(Start.Latitude, Start.Longitude, sep = "_"))) %>% 
  group_by(SiteID) %>% 
  slice_tail()

S_Model1 <- gls(SSp ~ Year + AvgOfStream.depth..m. + AvgOfWater.velocity..msec.,
                data = S_data
                )

S_Model2 <- gls(SSp ~ Year + AvgOfStream.depth..m. + AvgOfWater.velocity..msec.,
                data = S_data, 
                correlation = corExp(#value = c(50, 0.1),
                                     form = ~ Start.Latitude + Start.Longitude, 
                                     nugget = TRUE)
                )

#Looks like Ratio performs the best by a thin margin. 
#I dont understand Ratio so I'll go with Exp
AIC(S_Model1, S_Model2)

summary(S_Model2)
plot(S_Model2)

