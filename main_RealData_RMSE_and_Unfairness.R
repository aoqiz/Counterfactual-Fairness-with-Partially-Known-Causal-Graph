rm(list=ls())
source('causalRelation.R')
library(ggpubr)
set.seed(831)

#### Student
student.data <- read.csv("~/Desktop/fair_domain_adaptation-master/Student/student.data.terad")
student.data <- student.data[, -which(names(student.data) %in% c('G1', 'G2', 'G3'))]
col_names <- sapply(student.data, function(col) length(unique(col)) < 3)
student.data[ , col_names] <- lapply(student.data[ , col_names] , factor)

iter=10
df_RMSE=as.data.frame(matrix(nrow=0, ncol = 4))
df_Unfairness=as.data.frame(matrix(nrow=0, ncol = 4))

for(i in 1:iter)
{
  trainingRowIndex <- sample(1: nrow(student.data), 0.8*nrow(student.data))  # row indices for training data
  trainingData <- student.data[trainingRowIndex, ]  # model training data
  testData <- student.data[-trainingRowIndex, ]  
  
  # full model
  fullMod = lm(G ~., data = trainingData)
  testData$fullPred <- predict(fullMod, testData)
  (fullRMSE <- sqrt(mean((testData$fullPred - testData$G)^2)))
  
  # unawareness model
  unMod = lm(G ~ . - sex, data = trainingData)
  testData$unPred <- predict(unMod, testData)
  (unRMSE <- sqrt(mean((testData$unPred - testData$G)^2)))
  
  # relax counterfactual fair model
  cfRelaxMod = lm(G ~ . - sex - Walc- studytime - goout -Dalc , data = trainingData)
  testData$cfRelaxPred <- predict(cfRelaxMod, testData)
  (cfRelaxRMSE <- sqrt(mean((testData$cfRelaxPred - testData$G)^2)))
  
  # counterfactual fair model
  cfMod = lm(G ~ . - sex - Walc- studytime - goout -Dalc , data = trainingData)
  testData$cfPred <- predict(cfMod, testData)
  (cfRMSE <- sqrt(mean((testData$cfPred - testData$G)^2)))
  
  ####### RMSE ######
  RMSE_results <- data.frame(Full=c(round(fullRMSE,digits = 3)),
                             Unaware=c(round(unRMSE,digits = 3)),
                             FairRelax=c(round(cfRelaxRMSE,digits = 3)),
                             Fair=c(round(cfRMSE,digits = 3)))
  df_RMSE <- rbind(df_RMSE, RMSE_results) 
  
  ##### generate counterfactual data #####
  # obtain the coefficient
  mod_sex2Walc = lm(Walc ~ sex, data = trainingData)
  mod_Walc2Dalc = lm(Dalc ~ Walc, data = trainingData)
  mod_Walc2goout = lm(goout ~ Walc, data = trainingData)
  mod_sex2studytime = lm(studytime ~ sex, data = trainingData)
  # obtain the residual
  testData$resid_sex2Walc = testData$Walc-predict(mod_sex2Walc, newdata = testData)
  testData$resid_Walc2Dalc = testData$Dalc-predict(mod_Walc2Dalc, newdata = testData)
  testData$resid_Walc2goout = testData$goout-predict(mod_Walc2goout, newdata = testData)
  testData$resid_sex2studytime = testData$studytime-predict(mod_sex2studytime, newdata = testData)
  # obtain counterfactual data
  cfData <- data.frame(testData)
  loc_M <- which(testData$sex=='M')
  loc_F <- which(testData$sex=='F')
  cfData$sex[loc_M] = 'F'
  cfData$sex[loc_F] = 'M'
  cfData$Walc = predict(mod_sex2Walc, newdata = cfData)+cfData$resid_sex2Walc
  cfData$Dalc = predict(mod_Walc2Dalc, newdata = cfData)+cfData$resid_Walc2Dalc
  cfData$goout = predict(mod_Walc2goout, newdata = cfData)+cfData$resid_Walc2goout
  cfData$studytime = predict(mod_sex2studytime, newdata = cfData)+cfData$resid_sex2studytime
  # Prediction in counterfactual data
  cfData$fullPred <- predict(fullMod, cfData)
  cfData$unPred <- predict(unMod, cfData)
  cfData$cfRelaxPred <- predict(cfRelaxMod, cfData)
  cfData$cfPred <- predict(cfMod, cfData)
  
  ###### Unfairness #######
  Unfair_cf <- abs(testData$cfPred - cfData$cfPred)[1]
  Unfair_cfRelax <- abs(testData$cfRelaxPred - cfData$cfRelaxPred)[1]
  Unfair_un <- abs(testData$unPred - cfData$unPred)[1]
  Unfair_full <- abs(testData$fullPred - cfData$fullPred)[1]
  
  unfair_results <- data.frame(Full=c(round(Unfair_full,digits = 3)),
                               Unaware=c(round(Unfair_un,digits = 3)),
                               FairRelax=c(round(Unfair_cfRelax,digits = 3)),
                               Fair=c(round(Unfair_cf,digits = 3)))
  df_Unfairness <- rbind(df_Unfairness, unfair_results) 
}

###### FINAL RESULTS!! ########
(RMSE_mean <- round(colMeans(df_RMSE), digits = 3))
(RMSE_sd <- round(apply(df_RMSE, 2, sd), digits = 3))
(Unfairness_mean <- round(colMeans(df_Unfairness), digits = 3))
(Unfairness_sd <- round(apply(df_Unfairness, 2, sd), digits = 3))

########## Print Results! ########
results_file = paste0('./RealData/RealData_Results.csv')
results <- data.frame(Full=c(paste(RMSE_mean[1], "±", RMSE_sd[1], sep = ''), paste(Unfairness_mean[1], "±", Unfairness_sd[1], sep = '')),
            Unaware=c(paste(RMSE_mean[2], "±", RMSE_sd[2], sep = ''), paste(Unfairness_mean[2], "±", Unfairness_sd[2], sep = '')),
            FairRelax=c(paste(RMSE_mean[3], "±", RMSE_sd[3], sep = ''), paste(Unfairness_mean[3], "±", Unfairness_sd[3], sep = '')),
            Fair=c(paste(RMSE_mean[4], "±", RMSE_sd[4], sep = ''), paste(Unfairness_mean[4], "±", Unfairness_sd[4], sep = '')))
#write_csv(results, results_file, row_names =paste(d,"nodes",s,"edges",sep=''))
write.table(results, results_file, sep = ",", append = T, quote = FALSE,
            col.names = !file.exists(results_file), row.names=c('RMSE', 'Unfairness'))

