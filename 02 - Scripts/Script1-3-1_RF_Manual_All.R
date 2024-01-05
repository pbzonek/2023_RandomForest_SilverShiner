## --------------------------------------------------------------#
## Script name: Script1-3-1_RF_Manual_All.R 
##
## Purpose of script: 
##      Run RF model to predict Silver Shiner Occurrence
##       Manually selected features; 
##       Combined, Juvenile, and adult Life Stages
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
RF_M_C_data <- data_DFO_site %>% select(
  SSp, Year, Water.Temperature, AvgOfStream.depth..m., 
  AvgOfWater.velocity..msec., riparianveg_None) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RF_M_C_data_train <- sample_frac(RF_M_C_data, 0.7) #Subset 70% for train
RF_M_C_data_test <-anti_join(RF_M_C_data, RF_M_C_data_train) #Remaining 30% for test

 

###Make RandomForest model
#----------------------------#
set.seed(1758)
RF_M_C_model <- randomForest(formula=SSp ~., data = RF_M_C_data_train, replace=TRUE, 
                          #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                          classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                          importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RF_M_C_data_test$prediction1 <- predict(RF_M_C_model, RF_M_C_data_test)

caret::confusionMatrix(
  data=RF_M_C_data_test$prediction1, 
  reference=RF_M_C_data_test$SSp,  
  positive="1")

RF_M_C_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RF_M_C_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RF_M_C_data_test$prediction1, 
    reference=RF_M_C_data_test$SSp,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RF_M_C_diagnostic$Summary <-
  data.frame(Accuracy = (RF_M_C_diagnostic$ConfusionMatrix[1]+RF_M_C_diagnostic$ConfusionMatrix[4])/sum(RF_M_C_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RF_M_C_diagnostic$ConfusionMatrix[1]/(RF_M_C_diagnostic$ConfusionMatrix[1]+RF_M_C_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RF_M_C_diagnostic$ConfusionMatrix[4]/(RF_M_C_diagnostic$ConfusionMatrix[4]+RF_M_C_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RF_M_C_data$pred.RF_M_C <- predict(RF_M_C_model, RF_M_C_data)
RF_M_C_CM <- caret::confusionMatrix(RF_M_C_data$pred.RF_M_C, RF_M_C_data$SSp, positive="1")
RF_M_C_CM


RF_M_C_VarImp <- data.frame(importance(RF_M_C_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RF_M_C_VarImp <- RF_M_C_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RF_M_C_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RF_M_C_plot <- list()

RF_M_C_plot$R2 <- ggplot(RF_M_C_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
RF_M_C_plot$R2




#####Juvenile Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RF_M_J_data <- RF_data_J %>% select(
  immaturep, Month, Year, Water.Temperature) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RF_M_J_data_train <- sample_frac(RF_M_J_data, 0.7) #Subset 70% for train
RF_M_J_data_test <-anti_join(RF_M_J_data, RF_M_J_data_train) #Remaining 30% for test



###Make RandomForest model
#----------------------------#
set.seed(1758)
RF_M_J_model <- randomForest(formula=immaturep ~., data = RF_M_J_data_train, replace=TRUE, 
                            #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                            classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                            importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RF_M_J_data_test$prediction1 <- predict(RF_M_J_model, RF_M_J_data_test)

caret::confusionMatrix(
  data=RF_M_J_data_test$prediction1, 
  reference=RF_M_J_data_test$immaturep,  
  positive="1")

RF_M_J_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RF_M_J_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RF_M_J_data_test$prediction1, 
    reference=RF_M_J_data_test$immaturep,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RF_M_J_diagnostic$Summary <-
  data.frame(Accuracy = (RF_M_J_diagnostic$ConfusionMatrix[1]+RF_M_J_diagnostic$ConfusionMatrix[4])/sum(RF_M_J_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RF_M_J_diagnostic$ConfusionMatrix[1]/(RF_M_J_diagnostic$ConfusionMatrix[1]+RF_M_J_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RF_M_J_diagnostic$ConfusionMatrix[4]/(RF_M_J_diagnostic$ConfusionMatrix[4]+RF_M_J_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RF_M_J_data$pred.RF_M_J <- predict(RF_M_J_model, RF_M_J_data)
RF_M_J_CM <- caret::confusionMatrix(RF_M_J_data$pred.RF_M_J, RF_M_J_data$immaturep, positive="1")


RF_M_J_VarImp <- data.frame(importance(RF_M_J_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RF_M_J_VarImp <- RF_M_J_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RF_M_J_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RF_M_J_plot <- list()

RF_M_J_plot$R2 <- ggplot(RF_M_J_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
RF_M_J_plot$R2




#####Mature Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RF_M_A_data <- RF_data_A %>% select(
  maturep, Year, AvgOfStream.depth..m., 
  AvgOfWater.velocity..msec., substrate_Silt) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RF_M_A_data_train <- sample_frac(RF_M_A_data, 0.7) #Subset 70% for train
RF_M_A_data_test <-anti_join(RF_M_A_data, RF_M_A_data_train) #Remaining 30% for test



###Make RandomForest model
#----------------------------#
set.seed(1758)
RF_M_A_model <- randomForest(formula=maturep ~., data = RF_M_A_data_train, replace=TRUE, 
                            #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                            classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                            importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RF_M_A_data_test$prediction1 <- predict(RF_M_A_model, RF_M_A_data_test)

caret::confusionMatrix(
  data=RF_M_A_data_test$prediction1, 
  reference=RF_M_A_data_test$maturep,  
  positive="1")

RF_M_A_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RF_M_A_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RF_M_A_data_test$prediction1, 
    reference=RF_M_A_data_test$maturep,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RF_M_A_diagnostic$Summary <-
  data.frame(Accuracy = (RF_M_A_diagnostic$ConfusionMatrix[1]+RF_M_A_diagnostic$ConfusionMatrix[4])/sum(RF_M_A_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RF_M_A_diagnostic$ConfusionMatrix[1]/(RF_M_A_diagnostic$ConfusionMatrix[1]+RF_M_A_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RF_M_A_diagnostic$ConfusionMatrix[4]/(RF_M_A_diagnostic$ConfusionMatrix[4]+RF_M_A_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RF_M_A_data$pred.RF_M_A <- predict(RF_M_A_model, RF_M_A_data)
RF_M_A_CM <- caret::confusionMatrix(RF_M_A_data$pred.RF_M_A, RF_M_A_data$maturep, positive="1")


RF_M_A_VarImp <- data.frame(importance(RF_M_A_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RF_M_A_VarImp <- RF_M_A_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RF_M_A_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RF_M_A_plot <- list()

RF_M_A_plot$R2 <- ggplot(RF_M_A_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))

RF_M_A_plot$R2
