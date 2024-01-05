## --------------------------------------------------------------#
## Script name: Script2-1_Plots_Univariate.R 
##
## Purpose of script: 
##      Build the univariate plot objects to be used in 
##        combined multiple-panel figures
##
## Author: Paul Bzonek
##
## Date Created: 2022-11-16
##
## --------------------------------------------------------------#  
## Modification Notes:
## --------------------------------------------------------------#

#####Combined##################################----
#-------------------------------------------------------------#

###Univariate Plots
#----------------------------#
RF_F_C_plots_univiariate <- list() #Make list to hold plots
#ylab(bquote("Marginal effect" ~(hat(y))))

#Temporal variables
RF_F_C_plots_univiariate$Month <- RF_F_C_model %>% pdp::partial(pred.var = "Month", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Month.plot <- ggplot(RF_F_C_plots_univiariate$Month, aes(x=Month, y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1),colour="#8e3253")+theme_bw()+xlab("Month")+
  ylim(0.2,.75)+xlim("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Temporal")
RF_F_C_plots_univiariate$Month.plot

RF_F_C_plots_univiariate$Year <- RF_F_C_model %>% pdp::partial(pred.var = "Year", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Year.plot <- ggplot(RF_F_C_plots_univiariate$Year, aes(x=as.factor(Year), y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1), colour="#8e3253")+theme_bw()+xlab("Year")+
  coord_cartesian(ylim=c(0.2,0.75))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())
RF_F_C_plots_univiariate$Year.plot

#in stream variables
RF_F_C_plots_univiariate$depth <- RF_F_C_model %>% pdp::partial(pred.var = "AvgOfStream.depth..m.", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$depth.plot <- ggplot(RF_F_C_plots_univiariate$depth, aes(x=AvgOfStream.depth..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2, size=2)+theme_bw()+xlab("Depth (m)")+
  coord_cartesian(ylim=c(0.2,0.75), xlim=c(0,1.2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("In-Stream")
RF_F_C_plots_univiariate$depth.plot

RF_F_C_plots_univiariate$velocity <- RF_F_C_model %>% pdp::partial(pred.var = "AvgOfWater.velocity..msec.", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$velocity.plot <- ggplot(RF_F_C_plots_univiariate$velocity, aes(x=AvgOfWater.velocity..msec., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Velocity (m/sec)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75), xlim=c(0,1))
RF_F_C_plots_univiariate$velocity.plot

RF_F_C_plots_univiariate$Stream.Width..m. <- RF_F_C_model %>% pdp::partial(pred.var = "Stream.Width..m.", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Stream.Width.plot <- ggplot(RF_F_C_plots_univiariate$Stream.Width..m., aes(x=Stream.Width..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Stream Width (m)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75), xlim=c(0,35))
RF_F_C_plots_univiariate$Stream.Width.plot

RF_F_C_plots_univiariate$bankslope <- RF_F_C_model %>% pdp::partial(pred.var = "Bank.Slope....", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$bankslope.plot <- ggplot(RF_F_C_plots_univiariate$bankslope, aes(x=Bank.Slope...., y=yhat))+geom_smooth(col="#018be7", fill="NA", size=2)+theme_bw()+xlab("Bank Slope")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$bankslope.plot


#Substrate
RF_F_C_plots_univiariate$silt <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$silt.plot <- ggplot(RF_F_C_plots_univiariate$silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  coord_cartesian(ylim=c(0.2,0.75))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Substrate")
RF_F_C_plots_univiariate$silt.plot

RF_F_C_plots_univiariate$organic <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Organic", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$organic.plot <- ggplot(RF_F_C_plots_univiariate$organic, aes(x=substrate_Organic, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Organic Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$organic.plot

RF_F_C_plots_univiariate$cobble <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Cobble", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$cobble.plot <- ggplot(RF_F_C_plots_univiariate$cobble, aes(x=substrate_Cobble, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Cobble Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$cobble.plot

RF_F_C_plots_univiariate$Boulder <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Boulder", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Boulder.plot <- ggplot(RF_F_C_plots_univiariate$Boulder, aes(x=substrate_Boulder, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Boulder Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Boulder.plot

RF_F_C_plots_univiariate$Gravel <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Gravel", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Gravel.plot <- ggplot(RF_F_C_plots_univiariate$Gravel, aes(x=substrate_Gravel, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Gravel Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Gravel.plot

RF_F_C_plots_univiariate$Bedrock <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Bedrock", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Bedrock.plot <- ggplot(RF_F_C_plots_univiariate$Bedrock, aes(x=substrate_Bedrock, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Bedrock Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Bedrock.plot

RF_F_C_plots_univiariate$Sand <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Sand", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Sand.plot <- ggplot(RF_F_C_plots_univiariate$Sand, aes(x=substrate_Sand, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Sand Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Sand.plot

RF_F_C_plots_univiariate$Clay <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Clay", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Clay.plot <- ggplot(RF_F_C_plots_univiariate$Clay, aes(x=substrate_Clay, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Clay Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Clay.plot

RF_F_C_plots_univiariate$Silt <- RF_F_C_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$Silt.plot <- ggplot(RF_F_C_plots_univiariate$Silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$Silt.plot


#water chem 
RF_F_C_plots_univiariate$pH <- RF_F_C_model %>% pdp::partial(pred.var = "pH", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$pH.plot <- ggplot(RF_F_C_plots_univiariate$pH, aes(x=pH, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("pH")+
  coord_cartesian(ylim=c(0.2,0.75))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Water Chem")
RF_F_C_plots_univiariate$pH.plot

RF_F_C_plots_univiariate$oxygen <- RF_F_C_model %>% pdp::partial(pred.var = "Dissolved.Oxygen", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$oxygen.plot <- ggplot(RF_F_C_plots_univiariate$oxygen, aes(x=Dissolved.Oxygen, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Dissolved Oxygen")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$oxygen.plot

RF_F_C_plots_univiariate$temperature <- RF_F_C_model %>% pdp::partial(pred.var = "Water.Temperature", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$temperature.plot <- ggplot(RF_F_C_plots_univiariate$temperature, aes(x=Water.Temperature, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Temp")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$temperature.plot

RF_F_C_plots_univiariate$conductivity <- RF_F_C_model %>% pdp::partial(pred.var = "Conductivity", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$conductivity.plot <- ggplot(RF_F_C_plots_univiariate$conductivity, aes(x=Conductivity, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Conductivity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$conductivity.plot

#Vegetation
RF_F_C_plots_univiariate$aqveg <-  RF_F_C_model %>% pdp::partial(pred.var = "aquaticveg_Submerged", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$aqveg.plot <- ggplot(RF_F_C_plots_univiariate$aqveg, aes(x=aquaticveg_Submerged, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Aquatic vegetation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0.2,0.75))+ggtitle("Vegetation")
RF_F_C_plots_univiariate$aqveg.plot

RF_F_C_plots_univiariate$aqemerge <- RF_F_C_model %>% pdp::partial(pred.var = "aquaticveg_Emergent", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$aqemerge.plot <- ggplot(RF_F_C_plots_univiariate$aqemerge, aes(x=aquaticveg_Emergent, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Emergent Vegetation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$aqemerge.plot

RF_F_C_plots_univiariate$chcover <- RF_F_C_model %>% pdp::partial(pred.var = "Channel.Cover....", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$chcover.plot <- ggplot(RF_F_C_plots_univiariate$chcover, aes(x=Channel.Cover...., y=yhat))+geom_smooth(col="#01ce82", fill="NA", size=2)+theme_bw()+xlab("Channel Cover")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$chcover.plot

RF_F_C_plots_univiariate$herb <- RF_F_C_model %>% pdp::partial(pred.var = "riparianveg_Herbaceous", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$herb.plot <- ggplot(RF_F_C_plots_univiariate$herb, aes(x=riparianveg_Herbaceous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Herbaceous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$herb.plot

RF_F_C_plots_univiariate$deciduous <- RF_F_C_model %>% pdp::partial(pred.var = "riparianveg_Deciduous", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$deciduous.plot <- ggplot(RF_F_C_plots_univiariate$deciduous, aes(x=riparianveg_Deciduous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Deciduous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$deciduous.plot

RF_F_C_plots_univiariate$shrubs <- RF_F_C_model %>% pdp::partial(pred.var = "riparianveg_shrubs", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$shrubs.plot <- ggplot(RF_F_C_plots_univiariate$shrubs, aes(x=riparianveg_shrubs, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Shrub Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$shrubs.plot

RF_F_C_plots_univiariate$ripnone <- RF_F_C_model %>% pdp::partial(pred.var = "riparianveg_None", prob = TRUE, which.class=2, train=data_DFO_site) 
RF_F_C_plots_univiariate$ripnone.plot <- ggplot(RF_F_C_plots_univiariate$ripnone, aes(x=riparianveg_None, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("No Vegetation Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0.2,0.75))
RF_F_C_plots_univiariate$ripnone.plot
 
 

#####Juvenile##################################----
#-------------------------------------------------------------#
###Univariate Plots
#----------------------------#
RF_F_J_plots_univiariate <- list() #Make list to hold plots

#Temporal variables
RF_F_J_plots_univiariate$Month <- RF_F_J_model %>% pdp::partial(pred.var = "Month", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Month.plot <- ggplot(RF_F_J_plots_univiariate$Month, aes(x=Month, y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1),colour="#8e3253")+theme_bw()+xlab("Month")+
  ylim(0,0.7)+xlim("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Temporal")
RF_F_J_plots_univiariate$Month.plot

RF_F_J_plots_univiariate$Year <- RF_F_J_model %>% pdp::partial(pred.var = "Year", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Year.plot <- ggplot(RF_F_J_plots_univiariate$Year, aes(x=as.factor(Year), y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1), colour="#8e3253")+theme_bw()+xlab("Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))+xlim("2011", "2016", "2017", "2018")
RF_F_J_plots_univiariate$Year.plot

#in stream variables

RF_F_J_plots_univiariate$depth <- RF_F_J_model %>% pdp::partial(pred.var = "AvgOfStream.depth..m.", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$depth.plot <- ggplot(RF_F_J_plots_univiariate$depth, aes(x=AvgOfStream.depth..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2, size=2)+theme_bw()+xlab("Depth (m)")+
  coord_cartesian(ylim=c(0,0.7), xlim=c(0,1.2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("In-Stream")
RF_F_J_plots_univiariate$depth.plot

RF_F_J_plots_univiariate$velocity <- RF_F_J_model %>% pdp::partial(pred.var = "AvgOfWater.velocity..msec.", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$velocity.plot <- ggplot(RF_F_J_plots_univiariate$velocity, aes(x=AvgOfWater.velocity..msec., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Velocity (m/sec)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7), xlim=c(0,1))
RF_F_J_plots_univiariate$velocity.plot

RF_F_J_plots_univiariate$Stream.Width..m. <- RF_F_J_model %>% pdp::partial(pred.var = "Stream.Width..m.", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Stream.Width.plot <- ggplot(RF_F_J_plots_univiariate$Stream.Width..m., aes(x=Stream.Width..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Stream Width (m)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,.7), xlim=c(0,35))
RF_F_J_plots_univiariate$Stream.Width.plot

RF_F_J_plots_univiariate$bankslope <- RF_F_J_model %>% pdp::partial(pred.var = "Bank.Slope....", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$bankslope.plot <- ggplot(RF_F_J_plots_univiariate$bankslope, aes(x=Bank.Slope...., y=yhat))+geom_smooth(col="#018be7", fill="NA", size=2)+theme_bw()+xlab("Bank Slope")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$bankslope.plot


#Substrate
RF_F_J_plots_univiariate$silt <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$silt.plot <- ggplot(RF_F_J_plots_univiariate$silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))+ggtitle("Substrate")
RF_F_J_plots_univiariate$silt.plot

RF_F_J_plots_univiariate$organic <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Organic", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$organic.plot <- ggplot(RF_F_J_plots_univiariate$organic, aes(x=substrate_Organic, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Organic Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$organic.plot

RF_F_J_plots_univiariate$cobble <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Cobble", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$cobble.plot <- ggplot(RF_F_J_plots_univiariate$cobble, aes(x=substrate_Cobble, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Cobble Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$cobble.plot

RF_F_J_plots_univiariate$Boulder <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Boulder", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Boulder.plot <- ggplot(RF_F_J_plots_univiariate$Boulder, aes(x=substrate_Boulder, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Boulder Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Boulder.plot

RF_F_J_plots_univiariate$Gravel <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Gravel", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Gravel.plot <- ggplot(RF_F_J_plots_univiariate$Gravel, aes(x=substrate_Gravel, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Gravel Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Gravel.plot

RF_F_J_plots_univiariate$Bedrock <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Bedrock", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Bedrock.plot <- ggplot(RF_F_J_plots_univiariate$Bedrock, aes(x=substrate_Bedrock, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Bedrock Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Bedrock.plot

RF_F_J_plots_univiariate$Sand <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Sand", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Sand.plot <- ggplot(RF_F_J_plots_univiariate$Sand, aes(x=substrate_Sand, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Sand Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Sand.plot

RF_F_J_plots_univiariate$Clay <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Clay", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Clay.plot <- ggplot(RF_F_J_plots_univiariate$Clay, aes(x=substrate_Clay, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Clay Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Clay.plot

RF_F_J_plots_univiariate$Silt <- RF_F_J_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$Silt.plot <- ggplot(RF_F_J_plots_univiariate$Silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$Silt.plot


#water chem 
RF_F_J_plots_univiariate$pH <- RF_F_J_model %>% pdp::partial(pred.var = "pH", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$pH.plot <- ggplot(RF_F_J_plots_univiariate$pH, aes(x=pH, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("pH")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))+ggtitle("Water Chem")
RF_F_J_plots_univiariate$pH.plot

RF_F_J_plots_univiariate$oxygen <- RF_F_J_model %>% pdp::partial(pred.var = "Dissolved.Oxygen", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$oxygen.plot <- ggplot(RF_F_J_plots_univiariate$oxygen, aes(x=Dissolved.Oxygen, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Dissolved Oxygen")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$oxygen.plot

RF_F_J_plots_univiariate$temperature <- RF_F_J_model %>% pdp::partial(pred.var = "Water.Temperature", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$temperature.plot <- ggplot(RF_F_J_plots_univiariate$temperature, aes(x=Water.Temperature, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Temp")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$temperature.plot

RF_F_J_plots_univiariate$conductivity <- RF_F_J_model %>% pdp::partial(pred.var = "Conductivity", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$conductivity.plot <- ggplot(RF_F_J_plots_univiariate$conductivity, aes(x=Conductivity, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Conductivity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$conductivity.plot

#Vegetation
RF_F_J_plots_univiariate$aqveg <-  RF_F_J_model %>% pdp::partial(pred.var = "aquaticveg_Submerged", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$aqveg.plot <- ggplot(RF_F_J_plots_univiariate$aqveg, aes(x=aquaticveg_Submerged, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Aquatic vegetation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))+ggtitle("Vegetation")
RF_F_J_plots_univiariate$aqveg.plot

RF_F_J_plots_univiariate$aqemerge <- RF_F_J_model %>% pdp::partial(pred.var = "aquaticveg_Emergent", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$aqemerge.plot <- ggplot(RF_F_J_plots_univiariate$aqemerge, aes(x=aquaticveg_Emergent, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Emergent Vegetation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$aqemerge.plot

RF_F_J_plots_univiariate$chcover <- RF_F_J_model %>% pdp::partial(pred.var = "Channel.Cover....", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$chcover.plot <- ggplot(RF_F_J_plots_univiariate$chcover, aes(x=Channel.Cover...., y=yhat))+geom_smooth(col="#01ce82", fill="NA", size=2)+theme_bw()+xlab("Channel Cover")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$chcover.plot

RF_F_J_plots_univiariate$herb <- RF_F_J_model %>% pdp::partial(pred.var = "riparianveg_Herbaceous", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$herb.plot <- ggplot(RF_F_J_plots_univiariate$herb, aes(x=riparianveg_Herbaceous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Herbaceous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$herb.plot

RF_F_J_plots_univiariate$deciduous <- RF_F_J_model %>% pdp::partial(pred.var = "riparianveg_Deciduous", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$deciduous.plot <- ggplot(RF_F_J_plots_univiariate$deciduous, aes(x=riparianveg_Deciduous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Deciduous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$deciduous.plot

RF_F_J_plots_univiariate$shrubs <- RF_F_J_model %>% pdp::partial(pred.var = "riparianveg_shrubs", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$shrubs.plot <- ggplot(RF_F_J_plots_univiariate$shrubs, aes(x=riparianveg_shrubs, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Shrub Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$shrubs.plot

RF_F_J_plots_univiariate$ripnone <- RF_F_J_model %>% pdp::partial(pred.var = "riparianveg_None", prob = TRUE, which.class=2, train=RF_data_J) 
RF_F_J_plots_univiariate$ripnone.plot <- ggplot(RF_F_J_plots_univiariate$ripnone, aes(x=riparianveg_None, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("No Vegetation Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_J_plots_univiariate$ripnone.plot

 

#####Adult##################################----
#-------------------------------------------------------------#

###Univariate Plots
#----------------------------#
RF_F_A_plots_univiariate <- list() #Make list to hold plots

#Temporal variables
RF_F_A_plots_univiariate$Month <- RF_F_A_model %>% pdp::partial(pred.var = "Month", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Month.plot <- ggplot(RF_F_A_plots_univiariate$Month, aes(x=Month, y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1),colour="#8e3253")+theme_bw()+xlab("Month")+
  ylim(0,0.7)+xlim("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Temporal")
RF_F_A_plots_univiariate$Month.plot

RF_F_A_plots_univiariate$Year <- RF_F_A_model %>% pdp::partial(pred.var = "Year", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Year.plot <- ggplot(RF_F_A_plots_univiariate$Year, aes(x=as.factor(Year), y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1), colour="#8e3253")+theme_bw()+xlab("Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+ 
  coord_cartesian(ylim=c(0,0.7))+xlim("2011", "2016", "2017", "2018")
RF_F_A_plots_univiariate$Year.plot

#in stream variables
RF_F_A_plots_univiariate$depth <- RF_F_A_model %>% pdp::partial(pred.var = "AvgOfStream.depth..m.", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$depth.plot <- ggplot(RF_F_A_plots_univiariate$depth, aes(x=AvgOfStream.depth..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2, size=2)+theme_bw()+xlab("Depth (m)")+
  coord_cartesian(ylim=c(0,0.7), xlim=c(0,1.2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("In-Stream")
RF_F_A_plots_univiariate$depth.plot

RF_F_A_plots_univiariate$velocity <- RF_F_A_model %>% pdp::partial(pred.var = "AvgOfWater.velocity..msec.", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$velocity.plot <- ggplot(RF_F_A_plots_univiariate$velocity, aes(x=AvgOfWater.velocity..msec., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Velocity (m/sec)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7), xlim=c(0,1))
RF_F_A_plots_univiariate$velocity.plot

RF_F_A_plots_univiariate$Stream.Width..m. <- RF_F_A_model %>% pdp::partial(pred.var = "Stream.Width..m.", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Stream.Width.plot <- ggplot(RF_F_A_plots_univiariate$Stream.Width..m., aes(x=Stream.Width..m., y=yhat))+geom_smooth(col="#018be7",fill="NA", size=2)+theme_bw()+xlab("Stream Width (m)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,.7), xlim=c(0,35))
RF_F_A_plots_univiariate$Stream.Width.plot

RF_F_A_plots_univiariate$bankslope <- RF_F_A_model %>% pdp::partial(pred.var = "Bank.Slope....", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$bankslope.plot <- ggplot(RF_F_A_plots_univiariate$bankslope, aes(x=Bank.Slope...., y=yhat))+geom_smooth(col="#018be7", fill="NA", size=2)+theme_bw()+xlab("Bank Slope")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$bankslope.plot


#Substrate
RF_F_A_plots_univiariate$silt <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$silt.plot <- ggplot(RF_F_A_plots_univiariate$silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  coord_cartesian(ylim=c(0,0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Substrate")
RF_F_A_plots_univiariate$silt.plot

RF_F_A_plots_univiariate$organic <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Organic", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$organic.plot <- ggplot(RF_F_A_plots_univiariate$organic, aes(x=substrate_Organic, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Organic Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$organic.plot

RF_F_A_plots_univiariate$cobble <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Cobble", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$cobble.plot <- ggplot(RF_F_A_plots_univiariate$cobble, aes(x=substrate_Cobble, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Cobble Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$cobble.plot

RF_F_A_plots_univiariate$Boulder <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Boulder", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Boulder.plot <- ggplot(RF_F_A_plots_univiariate$Boulder, aes(x=substrate_Boulder, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Boulder Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Boulder.plot

RF_F_A_plots_univiariate$Gravel <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Gravel", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Gravel.plot <- ggplot(RF_F_A_plots_univiariate$Gravel, aes(x=substrate_Gravel, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Gravel Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Gravel.plot

RF_F_A_plots_univiariate$Bedrock <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Bedrock", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Bedrock.plot <- ggplot(RF_F_A_plots_univiariate$Bedrock, aes(x=substrate_Bedrock, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Bedrock Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Bedrock.plot

RF_F_A_plots_univiariate$Sand <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Sand", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Sand.plot <- ggplot(RF_F_A_plots_univiariate$Sand, aes(x=substrate_Sand, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Sand Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Sand.plot

RF_F_A_plots_univiariate$Clay <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Clay", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Clay.plot <- ggplot(RF_F_A_plots_univiariate$Clay, aes(x=substrate_Clay, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Clay Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Clay.plot

RF_F_A_plots_univiariate$Silt <- RF_F_A_model %>% pdp::partial(pred.var = "substrate_Silt", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$Silt.plot <- ggplot(RF_F_A_plots_univiariate$Silt, aes(x=substrate_Silt, y=yhat))+geom_smooth(col="#cda200",fill="NA", size=2)+theme_bw()+xlab("Silt Substrate")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$Silt.plot


#water chem 
RF_F_A_plots_univiariate$pH <- RF_F_A_model %>% pdp::partial(pred.var = "pH", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$pH.plot <- ggplot(RF_F_A_plots_univiariate$pH, aes(x=pH, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("pH")+
  coord_cartesian(ylim=c(0,0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Water Chem")
RF_F_A_plots_univiariate$pH.plot

RF_F_A_plots_univiariate$oxygen <- RF_F_A_model %>% pdp::partial(pred.var = "Dissolved.Oxygen", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$oxygen.plot <- ggplot(RF_F_A_plots_univiariate$oxygen, aes(x=Dissolved.Oxygen, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Dissolved Oxygen")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$oxygen.plot

RF_F_A_plots_univiariate$temperature <- RF_F_A_model %>% pdp::partial(pred.var = "Water.Temperature", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$temperature.plot <- ggplot(RF_F_A_plots_univiariate$temperature, aes(x=Water.Temperature, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Temp")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$temperature.plot

RF_F_A_plots_univiariate$conductivity <- RF_F_A_model %>% pdp::partial(pred.var = "Conductivity", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$conductivity.plot <- ggplot(RF_F_A_plots_univiariate$conductivity, aes(x=Conductivity, y=yhat))+geom_smooth(col="#ff81ba",fill="NA", size=2)+theme_bw()+xlab("Water Conductivity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$conductivity.plot

#Vegetation
RF_F_A_plots_univiariate$aqveg <-  RF_F_A_model %>% pdp::partial(pred.var = "aquaticveg_Submerged", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$aqveg.plot <- ggplot(RF_F_A_plots_univiariate$aqveg, aes(x=aquaticveg_Submerged, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Aquatic vegetation")+
  coord_cartesian(ylim=c(0,0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Vegetation")
RF_F_A_plots_univiariate$aqveg.plot

RF_F_A_plots_univiariate$aqemerge <- RF_F_A_model %>% pdp::partial(pred.var = "aquaticveg_Emergent", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$aqemerge.plot <- ggplot(RF_F_A_plots_univiariate$aqemerge, aes(x=aquaticveg_Emergent, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Emergent Vegetation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$aqemerge.plot

RF_F_A_plots_univiariate$chcover <- RF_F_A_model %>% pdp::partial(pred.var = "Channel.Cover....", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$chcover.plot <- ggplot(RF_F_A_plots_univiariate$chcover, aes(x=Channel.Cover...., y=yhat))+geom_smooth(col="#01ce82", fill="NA", size=2)+theme_bw()+xlab("Channel Cover")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$chcover.plot

RF_F_A_plots_univiariate$herb <- RF_F_A_model %>% pdp::partial(pred.var = "riparianveg_Herbaceous", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$herb.plot <- ggplot(RF_F_A_plots_univiariate$herb, aes(x=riparianveg_Herbaceous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Herbaceous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$herb.plot

RF_F_A_plots_univiariate$deciduous <- RF_F_A_model %>% pdp::partial(pred.var = "riparianveg_Deciduous", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$deciduous.plot <- ggplot(RF_F_A_plots_univiariate$deciduous, aes(x=riparianveg_Deciduous, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Deciduous Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$deciduous.plot

RF_F_A_plots_univiariate$shrubs <- RF_F_A_model %>% pdp::partial(pred.var = "riparianveg_shrubs", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$shrubs.plot <- ggplot(RF_F_A_plots_univiariate$shrubs, aes(x=riparianveg_shrubs, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("Shrub Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$shrubs.plot

RF_F_A_plots_univiariate$ripnone <- RF_F_A_model %>% pdp::partial(pred.var = "riparianveg_None", prob = TRUE, which.class=2, train=RF_data_A) 
RF_F_A_plots_univiariate$ripnone.plot <- ggplot(RF_F_A_plots_univiariate$ripnone, aes(x=riparianveg_None, y=yhat))+geom_smooth(col="#01ce82",fill="NA", size=2)+theme_bw()+xlab("No Vegetation Riparian")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y=element_blank())+
  coord_cartesian(ylim=c(0,0.7))
RF_F_A_plots_univiariate$ripnone.plot
