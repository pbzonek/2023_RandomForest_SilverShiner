## --------------------------------------------------------------#
## Script name: Script2-4_Plots_Maps_Revision.R 
##
## Purpose of script: 
##      Build the map figures with modification from reviewers.
##    
##
## Author: Paul Bzonek
##
## Date Created: 2023-01-27
##
## --------------------------------------------------------------#  
## Modification Notes:
## --------------------------------------------------------------#

###DFO and ConsHalton sites
#----------------------------#
#Pull data for plotting
dataplot <- data_DFO_site %>% 
  select(Start.Longitude, Start.Latitude,
         SSp, immaturep, maturep) %>% 
  cbind(RF_data_C$pred.RF_F_C) %>% 
  cbind(RF_L_C_data$pred.RF_L_C) %>% 
  cbind(RF_MS_C_data$pred.RF_MS_C) %>%
  cbind(RF_M_C_data$pred.RF_M_C)

dataplot2 <- data_DFO_site %>% 
  filter(Year %in% c(2011, 2016, 2017)) %>% droplevels() %>% 
  select(Start.Longitude, Start.Latitude) %>% 
  cbind(RF_data_J$pred.RF_F_J) %>% 
  cbind(RF_data_A$pred.RF_F_A) %>% 
  cbind(RF_L_J_data$pred.RF_L_J) %>% 
  cbind(RF_L_A_data$pred.RF_L_A) %>% 
  cbind(RF_MS_J_data$pred.RF_MS_J) %>% 
  cbind(RF_MS_A_data$pred.RF_MS_A) %>% 
  cbind(RF_M_J_data$pred.RF_M_J) %>% 
  cbind(RF_M_A_data$pred.RF_M_A) 

dataplot <- full_join(dataplot, dataplot2, 
                      by=c("Start.Longitude", "Start.Latitude")) %>%  
  rename(lon=Start.Longitude, lat=Start.Latitude,
         pred.RF_F_C=`RF_data_C$pred.RF_F_C`,
         pred.RF_L_C=`RF_L_C_data$pred.RF_L_C`,
         pred.RF_F_J=`RF_data_J$pred.RF_F_J`,
         pred.RF_F_A=`RF_data_A$pred.RF_F_A`,
         pred.RF_L_J=`RF_L_J_data$pred.RF_L_J`,
         pred.RF_L_A=`RF_L_A_data$pred.RF_L_A`,
         pred.RF_MS_C=`RF_MS_C_data$pred.RF_MS_C`,
         pred.RF_MS_J=`RF_MS_J_data$pred.RF_MS_J`,
         pred.RF_MS_A=`RF_MS_A_data$pred.RF_MS_A`,
         pred.RF_M_C=`RF_M_C_data$pred.RF_M_C`,
         pred.RF_M_J=`RF_M_J_data$pred.RF_M_J`,
         pred.RF_M_A=`RF_M_A_data$pred.RF_M_A`)
rm(dataplot2)



maps <- list()

#####Combined Life Stage##########################################----
#-------------------------------------------------------------#
#Full population map
maps$C_Raw <- ggmap(SSmap, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data = dataplot, aes(x=lon, y=lat, shape=SSp, size=as.factor(SSp)), inherit.aes = FALSE,
             fill="NA", stroke=1.5, colour="#00BFC4")+
  scale_shape_manual(name="Observed Silver Shiner Occurrence", 
                     labels=c("Absent","Present"), values=c(21, 18))+
  scale_size_manual(name="Test",
                    values=c(4, 2.5), guide=NULL)+
  ggtitle(paste("Combined Life Stage:", "Observed occurrence", sep="\n"))+
  theme(legend.position=c(0.75,0.85))

maps$C_Raw


#RF_F_C population map
temp_dataplot <- dataplot %>% 
  select(SSp, lon, lat, pred.RF_F_C) %>% 
  mutate(match = case_when(pred.RF_F_C == SSp ~ "Accurate",
                           TRUE ~ "Inaccurate"))

maps$C_RF_F_C <- ggmap(SSmap, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data = arrange(temp_dataplot, match), aes(x=lon, y=lat, colour=match, shape=SSp, size=as.factor(SSp)), inherit.aes = FALSE,
             fill="NA", stroke=1.5)+
  scale_color_manual(name="Model Accuracy", 
                     labels=c("Accurate","Inaccurate"), values=c("#00BFC4", "#F8766D"))+
  scale_shape_manual(name="Observed Silver Shiner Occurrence", 
                     labels=c("Absent","Present"), values=c(21, 18))+
  scale_size_manual(name="Test",
                    values=c(4, 2.5), guide=NULL)+
  ggtitle("Full model")+
  theme(legend.position=c(0.75,0.8))+
  guides(color = guide_legend(override.aes = list(size = 3)),
         shape = guide_legend(override.aes = list(size = 3)))

maps$C_RF_F_C


#RF_L_C population map
temp_dataplot <- dataplot %>% 
  select(SSp, lon, lat, pred.RF_L_C) %>% 
  mutate(match = case_when(pred.RF_L_C == SSp ~ "Accurate",
                           TRUE ~ "Inaccurate"))

maps$C_RF_L_C <- ggmap(SSmap, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data =arrange(temp_dataplot, match), aes(x=lon, y=lat, colour=match, shape=SSp, size=as.factor(SSp)), inherit.aes = FALSE,
             fill="NA", stroke=1.5)+
  scale_color_manual(name="Model Accuracy", 
                     labels=c("Accurate","Inaccurate"), values=c("#00BFC4", "#F8766D"))+
  scale_shape_manual(name="Observed Silver Shiner Occurrence", 
                     labels=c("Absent","Present"), values=c(21, 18))+
  scale_size_manual(name="Test",
                    values=c(4, 2.5), guide=NULL)+
  ggtitle("Literature model")+
  theme(legend.position=c(0.75,0.8))+
  guides(color = guide_legend(override.aes = list(size = 3)),
         shape = guide_legend(override.aes = list(size = 3)))

maps$C_RF_L_C



#RF_M_C population map
temp_dataplot <- dataplot %>% 
  select(SSp, lon, lat, pred.RF_M_C) %>% 
  mutate(match = case_when(pred.RF_M_C == SSp ~ "Accurate",
                           TRUE ~ "Inaccurate"))

maps$C_RF_M_C <- ggmap(SSmap, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data =arrange(temp_dataplot, match), aes(x=lon, y=lat, colour=match, shape=SSp, size=as.factor(SSp)), inherit.aes = FALSE,
             fill="NA", stroke=1.5)+
  scale_color_manual(name="Model Accuracy", 
                     labels=c("Accurate","Inaccurate"), values=c("#00BFC4", "#F8766D"))+
  scale_shape_manual(name="Observed Silver Shiner Occurrence", 
                     labels=c("Absent","Present"), values=c(21, 18))+
  scale_size_manual(name="Test",
                    values=c(4, 2.5), guide=NULL)+
  ggtitle("Manual model")+
  theme(legend.position=c(0.75,0.8))+
  guides(color = guide_legend(override.aes = list(size = 3)),
         shape = guide_legend(override.aes = list(size = 3)))

maps$C_RF_M_C




#####Multi-panel maps##############################################----
#-------------------------------------------------------------#

with(maps,
     C_RF_F_C + guide_area() +
     C_RF_L_C + C_RF_M_C +
    plot_layout(guides = 'collect') 
)

# ggsave(filename = "Fig1.svg",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures",
#        dpi=1200, width=174, height = 234, units="mm"
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )

