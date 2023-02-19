rm(list=ls())
source('causalRelation.R')
library(ggpubr)
library(RColorBrewer)
set.seed(831)

#### Student
student.data <- read.csv("~/Desktop/fair_domain_adaptation-master/Student/student.data.terad")
student.data <- student.data[, -which(names(student.data) %in% c('G1', 'G2', 'G3'))]
col_names <- sapply(student.data, function(col) length(unique(col)) < 3)
student.data[ , col_names] <- lapply(student.data[ , col_names] , factor)

iter=1
df_RMSE=as.data.frame(matrix(nrow=0, ncol = 4))
df_Unfairness=as.data.frame(matrix(nrow=0, ncol = 4))

trainingRowIndex <- sample(1: nrow(student.data), 0.8*nrow(student.data))  # row indices for training data
trainingData <- student.data[trainingRowIndex, ]  # model training data
testData <- student.data[-trainingRowIndex, ]  

# full model
fullMod = lm(G ~., data = trainingData)
testData$fullPred <- predict(fullMod, testData)
(fullRMSE <- sqrt(mean((testData$fullPred - testData$G)^2)))
student.data$fullPred <- predict(fullMod, student.data)

# unawareness model
unMod = lm(G ~ . - sex, data = trainingData)
testData$unPred <- predict(unMod, testData)
(unRMSE <- sqrt(mean((testData$unPred - testData$G)^2)))
student.data$unPred <- predict(unMod, student.data)

# relax counterfactual fair model
cfRelaxMod = lm(G ~ . - sex - Walc- studytime - goout -Dalc , data = trainingData)
testData$cfRelaxPred <- predict(cfRelaxMod, testData)
(cfRelaxRMSE <- sqrt(mean((testData$cfRelaxPred - testData$G)^2)))
student.data$cfRelaxPred <- predict(cfRelaxMod, student.data)

# counterfactual fair model
cfMod = lm(G ~ . - sex - Walc- studytime - goout -Dalc , data = trainingData)
testData$cfPred <- predict(cfMod, testData)
(cfRMSE <- sqrt(mean((testData$cfPred - testData$G)^2)))
student.data$cfPred <- predict(cfMod, student.data)


##### generate counterfactual data #####
# obtain the coefficient
mod_sex2Walc = lm(Walc ~ sex, data = trainingData)
mod_Walc2Dalc = lm(Dalc ~ Walc, data = trainingData)
mod_Walc2goout = lm(goout ~ Walc, data = trainingData)
mod_sex2studytime = lm(studytime ~ sex, data = trainingData)
# obtain the residual
student.data$resid_sex2Walc = student.data$Walc-predict(mod_sex2Walc, newdata = student.data)
student.data$resid_Walc2Dalc = student.data$Dalc-predict(mod_Walc2Dalc, newdata = student.data)
student.data$resid_Walc2goout = student.data$goout-predict(mod_Walc2goout, newdata = student.data)
student.data$resid_sex2studytime = student.data$studytime-predict(mod_sex2studytime, newdata = student.data)
# obtain counterfactual data
cfData <- data.frame(student.data)
loc_M <- which(student.data$sex=='M')
loc_F <- which(student.data$sex=='F')
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



# 
# # plot
cfData$Datatype <- 'counterfactual data'
student.data$Datatype <- 'original data'
sampleData <- rbind(student.data[student.data$sex=='F',], cfData[cfData$sex=='M',])

# pdf("RealData/Densityplot_unfairness_real_data.pdf", width = 14, height = 8, onefile=FALSE)
# #library('grid')
# fullPlot <- ggplot(sampleData, aes(x=fullPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(Full)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=25),
#         axis.title=element_text(size = 25),
#         legend.title = element_blank())
# unPlot <- ggplot(sampleData, aes(x=unPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(Unaware)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=25),
#         axis.title=element_text(size = 25),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# cfRelaxPlot <- ggplot(sampleData, aes(x=cfRelaxPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(FairRelax)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=25),
#         axis.title=element_text(size = 25),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# tcfPlot <- ggplot(sampleData, aes(x=cfPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(Oracle)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=25),
#         axis.title=element_text(size = 25),
#         legend.title = element_blank())
# cfPlot <- ggplot(sampleData, aes(x=cfPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(Fair)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=25),
#         axis.title=element_text(size = 25),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
#grid.newpage()
#grid.draw(cbind(ggplotGrob(fullPlot), ggplotGrob(unPlot), ggplotGrob(cfRelaxPlot),
#                ggplotGrob(tcfPlot), ggplotGrob(cfPlot)))

# ggarrange(fullPlot, unPlot, cfRelaxPlot, tcfPlot, cfPlot, nrow =2, ncol = 3, common.legend = TRUE)
# dev.off()


pdf("RealData/Densityplot_unfairness_real_data.pdf", width = 14, height = 3, onefile=FALSE)
#library('grid')
fullPlot <- ggplot(sampleData, aes(x=fullPred, fill=Datatype)) + geom_density(alpha=.5) +
  labs(x=expression(~hat(Grade)~(Full)), y='density', color="") +
  scale_fill_manual(values = c("#F4A582","#92C5DE")) +
  theme(legend.position = "none",
        legend.text=element_text(size=20),
        axis.title=element_text(size = 20),
        legend.title = element_blank())
unPlot <- ggplot(sampleData, aes(x=unPred, fill=Datatype)) + geom_density(alpha=.5) +
  labs(x=expression(~hat(Grade)~(Unaware)), y='density', color="") +
  scale_fill_manual(values = c("#F4A582","#92C5DE")) +
  theme(legend.position = "none",
        legend.text=element_text(size=20),
        axis.title=element_text(size = 20),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
cfRelaxPlot <- ggplot(sampleData, aes(x=cfRelaxPred, fill=Datatype)) + geom_density(alpha=.5) +
  labs(x=expression(~hat(Grade)~(FairRelax)), y='density', color="") +
  scale_fill_manual(values = c("#F4A582","#92C5DE")) +
  theme(legend.position = "none",
        legend.text=element_text(size=20),
        axis.title=element_text(size = 20),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
# tcfPlot <- ggplot(sampleData, aes(x=cfPred, fill=Datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Grade)~(Oracle)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         axis.title=element_text(size = 20),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
cfPlot <- ggplot(sampleData, aes(x=cfPred, fill=Datatype)) + geom_density(alpha=.5) +
  labs(x=expression(~hat(Grade)~(Fair)), y='density', color="") +
  scale_fill_manual(values = c("#F4A582","#92C5DE")) +
  theme(legend.position = "none",
        legend.text=element_text(size=20),
        axis.title=element_text(size = 20),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
#grid.newpage()
#grid.draw(cbind(ggplotGrob(fullPlot), ggplotGrob(unPlot), ggplotGrob(cfRelaxPlot),
#                ggplotGrob(tcfPlot), ggplotGrob(cfPlot)))

ggarrange(fullPlot, unPlot, cfRelaxPlot, cfPlot, nrow =1, common.legend = TRUE)
dev.off()