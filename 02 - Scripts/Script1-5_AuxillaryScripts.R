
###Summary data table
#----------------------------#
df_summary <- data.frame(
  Combined_Mean = RF_data_C %>% filter(SSp==1) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% t(),
  Combined_sd = RF_data_C %>% filter(SSp==1) %>% 
    summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% t(),
  
  Juvenile_Mean = RF_data_J %>% filter(immaturep==1) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% t(),
  Juvenile_sd = RF_data_J %>% filter(immaturep==1) %>% 
    summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% t(),
  
  Mature_Mean = RF_data_A %>% filter(maturep==1) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% t(),
  Mature_sd = RF_data_A %>% filter(maturep==1) %>% 
    summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% t()
  
) %>% mutate_if(is.numeric, substr, 1, 5) %>% 
  mutate(Combined = paste0(Combined_Mean, " ± (", Combined_sd, ")"),
         Juvenile = paste0(Juvenile_Mean, " ± (", Juvenile_sd, ")"),
         Mature = paste0(Mature_Mean, " ± (", Mature_sd, ")"))


###Report model performance
#----------------------------#

#Build confusion matrix
RF_M_C_data_train$prediction1 <- predict(RF_M_C_model, RF_M_C_data_train)
RF_M_C_diagnostic$Summary2 <- 
  caret::confusionMatrix(
    data=RF_M_C_data_train$prediction1, 
    reference=RF_M_C_data_train$SSp,  
    positive="1") 

#Display confusion matrix
print(RF_M_C_diagnostic$Summary2)

#Print and copy values of interest
data.frame(Accuracy = paste0(round(RF_M_C_diagnostic$Summary2[["overall"]][[1]], digits=2),
                             " (",
                             round(RF_M_C_diagnostic$Summary2[["overall"]][[3]], digits=2),
                             ", ",
                             round(RF_M_C_diagnostic$Summary2[["overall"]][[4]], digits=2),
                             ")"),
           `No Information Rate` = round(RF_M_C_diagnostic$Summary2[["overall"]][[5]], digits=2),
           Sensitivity = round(RF_M_C_diagnostic$Summary2[["byClass"]][[1]], digits=2),
           Specificity = round(RF_M_C_diagnostic$Summary2[["byClass"]][[2]], digits=2),
           `Pos Pred Value` = round(RF_M_C_diagnostic$Summary2[["byClass"]][[3]], digits=2),
           `Neg Pred Value` = round(RF_M_C_diagnostic$Summary2[["byClass"]][[4]], digits=2),
           `Balanced Accuracy` = round(RF_M_C_diagnostic$Summary2[["byClass"]][[11]], digits=2),
           `P value` = round(RF_M_C_diagnostic$Summary2[["overall"]][[6]], digits=2)
          ) %>%
  print() %>% write.table(., file="clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
