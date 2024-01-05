## --------------------------------------------------------------#
## Script name: Script2-3_Plots_Bivariate.R 
##
## Purpose of script: 
##      Build additional bivariate plots used for the publication
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-11-16
##
## --------------------------------------------------------------#  
## Modification Notes:
## --------------------------------------------------------------#


Plots_Bivariate <- list()
Plots_Bivariate$RF_F_C <- RF_F_C_model %>% 
  partial(pred.var = c("AvgOfWater.velocity..msec.", "AvgOfStream.depth..m."), prob=TRUE, which.class=2, train=data_DFO_site) %>% 
  ggplot(aes(x=AvgOfWater.velocity..msec., y=AvgOfStream.depth..m.,  fill=yhat))+
  geom_tile()+
  scale_y_reverse()+
  scale_fill_viridis_c()+
  xlab("Water velocity (m/sec)")+ylab("Stream depth (m)")+
  #theme(legend.position="bottom")+
  labs(title = "Combined Life Stages")

Plots_Bivariate$RF_F_A <- RF_F_A_model %>% 
  partial(pred.var = c("AvgOfWater.velocity..msec.", "AvgOfStream.depth..m."), prob=TRUE, which.class=2, train=RF_data_A) %>% 
  ggplot(aes(x=AvgOfWater.velocity..msec., y=AvgOfStream.depth..m.,  fill=yhat))+
  geom_tile()+
  scale_y_reverse()+
  scale_fill_viridis_c()+
  xlab("Water velocity (m/sec)")+ylab("Stream depth (m)")+
  #theme(legend.position="bottom")+
  labs(title = "Adults")

Plots_Bivariate$RF_F_J <-RF_F_J_model %>% 
  partial(pred.var = c("AvgOfWater.velocity..msec.", "AvgOfStream.depth..m."), prob=TRUE, which.class=2, train=RF_data_J) %>% 
  ggplot(aes(x=AvgOfWater.velocity..msec., y=AvgOfStream.depth..m.,  fill=yhat))+
  geom_tile()+
  scale_y_reverse()+
  scale_fill_viridis_c()+
  xlab("Water velocity (m/sec)")+ylab("Stream depth (m)")+
  #theme(legend.position="bottom")+
  labs(title = "Juveniles")

with(Plots_Bivariate, RF_F_C/RF_F_J/RF_F_A)

ggsave(filename = "Fig6.svg",
       path="C:/Users/BZONEKP/Documents/06 - Updates/01 - Silver Shiner/01 - Figures/",
       dpi=1200, width=84, height = 234, units="mm"
       #Size; Width: 84 mm, 174 mm; Height: 234 #height=90, width=84, units=c("mm")
       )
