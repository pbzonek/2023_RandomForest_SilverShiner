## --------------------------------------------------------------#
## Script name: Script1-1-1_RF_Full_Combined.R 
##
## Purpose of script: 
##      Run RF model to predict Silver Shiner Occurrence
##      Full Features; Combined Life Stage
##
## Author: Paul Bzonek
##
## Date Created: 2023-10-06
##
## --------------------------------------------------------------#  
## Modification Notes:   
##   
## --------------------------------------------------------------#


#####Mature Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RF_data_C <- data_DFO_site %>% select(
  SSp, Month, Year, Water.Temperature, Conductivity, Dissolved.Oxygen, pH, AvgOfStream.depth..m., 
    AvgOfWater.velocity..msec., Stream.Width..m., Bank.Slope...., Channel.Cover...., 
    substrate_Organic, substrate_Clay, substrate_Silt, substrate_Sand, 
    substrate_Gravel, substrate_Cobble, substrate_Boulder, substrate_Bedrock, 
    aquaticveg_Emergent, aquaticveg_Submerged, riparianveg_Deciduous, riparianveg_Herbaceous, 
    riparianveg_shrubs, riparianveg_None) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RF_data_C_train <- sample_frac(RF_data_C, 0.7) #Subset 70% for train
RF_data_C_test <-anti_join(RF_data_C, RF_data_C_train) #Remaining 30% for test

 

###Make RandomForest model
#----------------------------#
set.seed(1758)
RF_F_C_model <- randomForest(formula=SSp ~., data = RF_data_C_train, replace=TRUE, 
                          #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                          classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                          importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RF_data_C_test$prediction1 <- predict(RF_F_C_model, RF_data_C_test)

caret::confusionMatrix(
  data=RF_data_C_test$prediction1, 
  reference=RF_data_C_test$SSp,  
  positive="1")

RF_F_C_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RF_F_C_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RF_data_C_test$prediction1, 
    reference=RF_data_C_test$SSp,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RF_F_C_diagnostic$Summary <-
  data.frame(Accuracy = (RF_F_C_diagnostic$ConfusionMatrix[1]+RF_F_C_diagnostic$ConfusionMatrix[4])/sum(RF_F_C_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RF_F_C_diagnostic$ConfusionMatrix[1]/(RF_F_C_diagnostic$ConfusionMatrix[1]+RF_F_C_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RF_F_C_diagnostic$ConfusionMatrix[4]/(RF_F_C_diagnostic$ConfusionMatrix[4]+RF_F_C_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RF_data_C$pred.RF_F_C <- predict(RF_F_C_model, RF_data_C)
RF_F_C_CM <- caret::confusionMatrix(RF_data_C$pred.RF_F_C, RF_data_C$SSp, positive="1")
RF_F_C_CM


RF_F_C_VarImp <- data.frame(importance(RF_F_C_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RF_F_C_VarImp <- RF_F_C_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RF_F_C_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RF_F_C_plot <- list()

RF_F_C_plot$R2 <- ggplot(RF_F_C_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip(ylim=c(0,0.25))+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
RF_F_C_plot$R2


