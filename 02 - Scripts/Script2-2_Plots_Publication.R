## --------------------------------------------------------------#
## Script name: Script2-6-0_PublicationPlots.R 
##
## Purpose of script: 
##      Build the plots used for the publication
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-11-16
##
## --------------------------------------------------------------#  
## Modification Notes:
## --------------------------------------------------------------#

#Specify pretty names for plots
names_FactorMapping <- c(
  "Channel.Cover...."="Channel Cover",
  "riparianveg_shrubs"="Shrub Riparian", 
  "substrate_Bedrock"="Bedrock",
  "substrate_Boulder"="Boulder",
  "substrate_Clay"="Clay",
  "substrate_Cobble"="Cobble", 
  "substrate_Organic"="Organic",
  "substrate_Sand"="Sand",
  "substrate_Silt"="Silt",
  "aquaticveg_Emergent"="Emergent vegetation",
  "aquaticveg_Submerged"="Submerged Vegetation",   
  "substrate_Gravel"="Gravel", 
  "Dissolved.Oxygen"="Dissolved Oxygen",
  "Bank.Slope...."="Bank Slope",
  "riparianveg_Deciduous"="Deciduous vegetation",
  "riparianveg_Herbaceous"="Herbaceous vegetation",
  "pH"="pH",
  "Conductivity"="Conductivity",
  "Year"="Year",  
  "Stream.Width..m."="Stream Width",
  "Month"="Month", 
  "Water.Temperature"="Water Temperature",
  "riparianveg_None"="Barren Riparian",
  "AvgOfWater.velocity..msec."="Water Velocity", 
  "AvgOfStream.depth..m."="Depth")
  
#####pseudo R2####################################################----
#-------------------------------------------------------------#
data.table(predictor = RF_F_C_VarImp$predictor,
           R2 = RF_F_C_VarImp$pR2,
           Stage = "Combined") %>% 
  mutate(predictor=fct_reorder(as.factor(predictor), R2)) %>% 
  rbind(data.table(predictor = RF_F_J_VarImp$predictor,
                   R2 = RF_F_J_VarImp$pR2,
                   Stage = "Juvenile")) %>% 
  rbind(data.table(predictor = RF_F_A_VarImp$predictor,
                   R2 = RF_F_A_VarImp$pR2,
                   Stage = "Mature")) %>% 
  
  ggplot(., aes(x=predictor, y=R2))+
  geom_segment(aes(x=predictor,
                   xend=predictor, y=0, yend=R2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip(ylim=c(0,0.25))+
  facet_wrap(~Stage)+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)

# ggsave(filename = "R2_All2.tiff",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/02 - Draft 1",
#        dpi=1200,
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )

#####Multi Model R2###############################################----
#-------------------------------------------------------------#
temp_plotdata <- 
  cbind(select(RF_F_C_VarImp, predictor, pR2), Model="Full", LifeStage="Combined") %>% 
  
  mutate(predictor=fct_reorder(as.factor(predictor), pR2),
         LifeStage=factor(LifeStage, levels=c("Combined", "Juvenile", "Adult")),
         Model=factor(Model, levels=c("Full", "ModelSelection", "ManualSelection", "Lamothe"))) %>% 
  
  rbind(cbind(select(RF_L_C_VarImp, predictor, pR2), Model="Lamothe", LifeStage="Combined")) %>% 
  rbind(cbind(select(RF_MS_C_VarImp, predictor, pR2), Model="ModelSelection", LifeStage="Combined")) %>% 
  rbind(cbind(select(RF_M_C_VarImp, predictor, pR2), Model="ManualSelection", LifeStage="Combined")) %>% 
  
  rbind(cbind(select(RF_F_J_VarImp, predictor, pR2), Model="Full", LifeStage="Juvenile")) %>% 
  rbind(cbind(select(RF_L_J_VarImp, predictor, pR2), Model="Lamothe", LifeStage="Juvenile")) %>% 
  rbind(cbind(select(RF_MS_J_VarImp, predictor, pR2), Model="ModelSelection", LifeStage="Juvenile")) %>% 
  rbind(cbind(select(RF_M_J_VarImp, predictor, pR2), Model="ManualSelection", LifeStage="Juvenile")) %>% 
  
  rbind(cbind(select(RF_F_A_VarImp, predictor, pR2), Model="Full", LifeStage="Adult")) %>% 
  rbind(cbind(select(RF_L_A_VarImp, predictor, pR2), Model="Lamothe", LifeStage="Adult")) %>% 
  rbind(cbind(select(RF_MS_A_VarImp, predictor, pR2), Model="ModelSelection", LifeStage="Adult")) %>% 
  rbind(cbind(select(RF_M_A_VarImp, predictor, pR2), Model="ManualSelection", LifeStage="Adult")) 


temp_plotdata$predictor2 <- factor(temp_plotdata$predictor, levels = names(names_FactorMapping))
levels(temp_plotdata$predictor2) <- names_FactorMapping[levels(temp_plotdata$predictor2)]
temp_plotdata  

ggplot(temp_plotdata, aes(x=predictor2, y=pR2))+
  geom_segment(aes(x=predictor2,
                   xend=predictor2, y=0, yend=pR2))+
  geom_point(aes(fill=Model), color="black", size=3.5, pch=21, stroke=1.5)+
  coord_flip()+
  scale_fill_manual(values=c("#b7001480","#01c04c80","#3641c480","#a8775580"))+
  facet_wrap(~LifeStage)+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)+
  theme(legend.position="bottom", axis.text = element_text(size = 14))

# ggsave(filename = "Fig2.tiff",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/05 - Hydrobiologia Final",
#        dpi=700, width=174, height=234, units=c("mm")
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )

rm(temp_plotdata)

#####Univariate Plots#############################################----
#-------------------------------------------------------------#

###Combined
#----------------------------#
RF_F_C_plots_univiariate$full <- 
  with(RF_F_C_plots_univiariate,
       Month.plot+Year.plot+plot_spacer()+plot_spacer()+
         
         depth.plot+velocity.plot+Stream.Width.plot+bankslope.plot+
         silt.plot+organic.plot+Sand.plot+Boulder.plot+
         cobble.plot+Gravel.plot+Bedrock.plot+Clay.plot+
         
         pH.plot+oxygen.plot+temperature.plot+conductivity.plot+
         
         aqveg.plot+aqemerge.plot+chcover.plot+herb.plot+
         deciduous.plot+shrubs.plot+ripnone.plot+
         
         plot_layout(ncol=4))+plot_annotation(title='Combined Life Stages')
RF_F_C_plots_univiariate$full

# ggsave(filename = "Fig3.svg",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/05 - Hydrobiologia Final",
#        dpi=600, width=174, height = 234, units="mm"
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )

###Adults
#----------------------------#
RF_F_A_plots_univiariate$full <- 
  with(RF_F_A_plots_univiariate,
       Month.plot+Year.plot+plot_spacer()+plot_spacer()+
         
         depth.plot+velocity.plot+Stream.Width.plot+bankslope.plot+
         silt.plot+organic.plot+Sand.plot+Boulder.plot+
         cobble.plot+Gravel.plot+Bedrock.plot+Clay.plot+
         
         pH.plot+oxygen.plot+temperature.plot+conductivity.plot+
         
         aqveg.plot+aqemerge.plot+chcover.plot+herb.plot+
         deciduous.plot+shrubs.plot+ripnone.plot+
         
         plot_layout(ncol=4))+plot_annotation(title='Adult Silver Shiner')
RF_F_A_plots_univiariate$full 

# ggsave(filename = "Fig5.tiff",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/05 - Hydrobiologia Final",
#        dpi=600, width=174, height = 234, units="mm"
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )

###Juvenile
#----------------------------#
RF_F_J_plots_univiariate$full <- 
  with(RF_F_J_plots_univiariate,
       Month.plot+Year.plot+plot_spacer()+plot_spacer()+
         
         depth.plot+velocity.plot+Stream.Width.plot+bankslope.plot+
         silt.plot+organic.plot+Sand.plot+Boulder.plot+
         cobble.plot+Gravel.plot+Bedrock.plot+Clay.plot+
         
         pH.plot+oxygen.plot+temperature.plot+conductivity.plot+
         
         aqveg.plot+aqemerge.plot+chcover.plot+herb.plot+
         deciduous.plot+shrubs.plot+ripnone.plot+
         
         plot_layout(ncol=4))+plot_annotation(title='Juvenile Silver Shiner')
RF_F_J_plots_univiariate$full 

# ggsave(filename = "Fig4.tiff",
#        path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/05 - Hydrobiologia Final",
#        dpi=600, width=174, height = 234, units="mm"
#        #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
# )
# 



