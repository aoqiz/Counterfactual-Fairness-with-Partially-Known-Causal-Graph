rm(list=ls())
source('causalRelation.R')

suppressPackageStartupMessages(library(pcalg))
library(ggpubr, quietly = TRUE)
options(warn = -1)

args = commandArgs(trailingOnly = TRUE)
# number of nodes
d = args[1]
# number of edges
s = args[2]
# which graph
k = args[3]
 # d=10;s=20;k=52
cat(k, ":\n")

bk_prop = 0.1
set.seed(2022)
filename = paste0("./Repository/", d, "nodes", s, "edges")

# 1.  --- DAG2CPDAG ---
B_bin = read.csv(paste0(filename, "/adjacency_matrix_", k, ".csv"), header=FALSE)
amat.dag = t(B_bin)
rownames(amat.dag) = NULL
rDAG = as(t(amat.dag), "graphNEL")
CPDAG <- dag2cpdag(rDAG)
# if (require(Rgraphviz)) {
#   par(mfrow=c(1,2))
#   plot(rDAG, main = "True DAG")
#   plot(CPDAG, main = "Estimated CPDAG")
# }


## 2. --- Add background knowledge and get MPDAG ----
amat = t(as(CPDAG, 'matrix'))
diff_dag2cpdag = amat-ifelse(amat.dag!=0,1,0)
undirected_edge = which(diff_dag2cpdag==1, arr.ind = TRUE, useNames = FALSE)
n_undirected = nrow(undirected_edge)
bk = matrix(undirected_edge[sample(1:n_undirected, ceiling(bk_prop*n_undirected)),], ncol=2)
#print('Background knowledge')
#print(bk)
amat.mpdag2 = addBgKnowledge(gInput = amat, x=bk[,1], y=bk[,2])

# pdf(paste0(filename, "/causal_graphs_", k, ".pdf"), width = 14, height = 7)
# # show the DAG, CPDAG and MPDAG
# if (require(Rgraphviz)) {
#    par(mfrow=c(1,3))
#    plot(rDAG, main = "True DAG")
#    plot(CPDAG, main = "Estimated CPDAG")
#    plot(as(t(as(amat.mpdag2, "matrix")), "graphNEL"), main='MPDAG')
# }
# dev.off()

## 3.  --- Identify causal relationships ---
config <- read.csv(paste0(filename, "/config_", k, ".txt"))
protected <- config$protected
protected_classes <- config$protected_classes
outcome <- config$outcome
# cat("protected_attributes:", protected, "\n")
# cat("outcome:", outcome, "\n\n")
causal_relations = IdentifyAllCausalRelation(amat.mpdag2, protected)
# print(causal_relations)
defNonDes = causal_relations$defNonDes
possDes = causal_relations$possDes
defDes = causal_relations$defDes
# Des and Nondes in true DAG
Des <- possDe(m = amat.dag, x = protected, possible = FALSE, ds = FALSE, type = "dag")
NonDes <- setdiff(1:length(amat.dag[1,]), Des)


## 4. --- Train the model and fit observational data ----
set.seed(2022)

m <- config$num_sample
n <- config$sample_size
observational_data = read.csv(paste0(filename, "/observational_data_", k, ".csv"), header=FALSE)
fullRMSE_list = rep(0,m)
unRMSE_list = rep(0,m)
cfRelaxRMSE_list = rep(0,m)
tcfRMSE_list = rep(0,m) #true cf
cfRMSE_list = rep(0,m)

for (i in 1:m)
{
  # Fit the model
  Data = as.matrix(observational_data[(n*(i-1)+1):(n*i),]) 
  Y = Data[,outcome]
  A = Data[,protected]
  X_defNonDes = Data[,unlist(defNonDes)[!unlist(defNonDes)==outcome]]
  X_NonDes = Data[,NonDes[!NonDes==outcome]]
  X_possDes = Data[,unlist(possDes)[!unlist(possDes)==outcome]]
  X_defDes = Data[,unlist(defDes)[!unlist(defDes) %in% c(protected,outcome)]]
  trainingRowIndex <- sample(1: nrow(Data), 0.8*nrow(Data))  # row indices for training data
  
  # counterfactual fair model
  Dataset= as.data.frame(cbind(X_defNonDes, Y))
  colnames(Dataset)[-length(Dataset[1,])] <- paste("V", 1:(length(Dataset[1,])-1), sep = "")
  if(ncol(Dataset)>1)
  {
    trainingData <- Dataset[trainingRowIndex, ]  # model training data
    testData <- Dataset[-trainingRowIndex, ] 
    cfMod = lm(Y~., data = trainingData)
    cfPred <- predict(cfMod, testData)
    (cfRMSE <- sqrt(mean((cfPred - testData$Y)^2))) 
    Dataset_cfPred <- predict(cfMod, Dataset)
  }else{cfRMSE<-NA; cfPred<- NA; Dataset_cfPred<-NA}
  
  # counterfactual fair model in true DAG
  Dataset2= as.data.frame(cbind(X_NonDes, Y))
  colnames(Dataset2)[-length(Dataset2[1,])] <- paste("V", 1:(length(Dataset2[1,])-1), sep = "")
  if(ncol(Dataset2)>1)
  {
    trainingData <- Dataset2[trainingRowIndex, ]  # model training data
    testData <- Dataset2[-trainingRowIndex, ] 
    tcfMod = lm(Y~., data = trainingData)
    tcfPred <- predict(tcfMod, testData)
    (tcfRMSE <- sqrt(mean((tcfPred - testData$Y)^2))) 
    Dataset_tcfPred <- predict(tcfMod, Dataset2)
  }else{tcfRMSE <- NA; tcfPred <- NA; Dataset_tcfPred<-NA} #NA
  
  # relaxed counterfactual fair model
  Dataset= cbind(X_possDes, Dataset)
  colnames(Dataset)[-length(Dataset[1,])] <- paste("V", 1:(length(Dataset[1,])-1), sep = "")
  if(ncol(Dataset)>1)
  {
    trainingData <- Dataset[trainingRowIndex, ]
    testData <- Dataset[-trainingRowIndex, ] 
    cfRelaxMod = lm(Y~., data = trainingData)
    cfRelaxPred <- predict(cfRelaxMod, testData)
    (cfRelaxRMSE <- sqrt(mean((cfRelaxPred - testData$Y)^2))) 
    Dataset_cfRelaxPred <- predict(cfRelaxMod, Dataset)
  }else{cfRelaxRMSE <- NA; cfRelaxPred <- NA; Dataset_cfRelaxPred<-NA} #Na
  
  # unawareness model
  Dataset= cbind(X_defDes, Dataset)
  colnames(Dataset)[-length(Dataset[1,])] <- paste("V", 1:(length(Dataset[1,])-1), sep = "")
  trainingData <- Dataset[trainingRowIndex, ]
  testData <- Dataset[-trainingRowIndex, ] 
  unMod = lm(Y ~ ., data = trainingData)
  unPred <- predict(unMod, testData)
  (unRMSE <- sqrt(mean((unPred - testData$Y)^2)))
  Dataset_unPred <- predict(unMod, Dataset)
  
  # full model
  Dataset= cbind(A, Dataset)
  trainingData <- Dataset[trainingRowIndex, ]
  testData <- Dataset[-trainingRowIndex, ] 
  fullMod = lm(Y ~., data = trainingData)
  fullPred <- predict(fullMod, testData)
  (fullRMSE <- sqrt(mean((fullPred - testData$Y)^2)))
  Dataset_fullPred <- predict(fullMod, Dataset)
  
  # cfRMSE_list[i] <- cfRMSE
  # tcfRMSE_list[i] <- tcfRMSE
  # cfRelaxRMSE_list[i] <- cfRelaxRMSE
  # unRMSE_list[i] <- unRMSE
  # fullRMSE_list[i] <- fullRMSE   
}

## 5. --- Print results ---
# results <- data.frame(Full=c(round(mean(fullRMSE_list),digits = 3)),
#                      Unaware=c(round(mean(unRMSE_list),digits = 3)),
#                      FairRelax=c(round(mean(cfRelaxRMSE_list),digits = 3)),
#                      Oracle=c(round(mean(tcfRMSE_list),digits = 3)),
#                      Fair=c(round(mean(cfRMSE_list),digits = 3)))
results <- data.frame(Full=c(ifelse(is.na(fullRMSE), fullRMSE, round(fullRMSE,digits = 3))),
                     Unaware=c(ifelse(is.na(unRMSE), unRMSE, round(unRMSE,digits = 3))),
                     FairRelax=c(ifelse(is.na(cfRelaxRMSE), cfRelaxRMSE, round(cfRelaxRMSE,digits = 3))),
                     Oracle=c(ifelse(is.na(tcfRMSE), tcfRMSE, round(tcfRMSE,digits = 3))),
                     Fair=c(ifelse(is.na(cfRMSE), cfRMSE, round(cfRMSE,digits = 3))))
# results_file = paste0(filename, '/RMSE.csv')
# #write_csv(results, results_file, row_names =paste(d,"nodes",s,"edges",sep=''))
# write.table(results, results_file, sep = ",", append = T, quote = FALSE, 
#             col.names = !file.exists(results_file), row.names=paste(d,"nodes",s,"edges",k, sep=''))

#cat("Mean and sd of fullPred RMSE: ", mean(fullRMSE_list), "+-", sd(fullRMSE_list), "\n")
#cat("Mean and sd of unPred RMSE: ", mean(unRMSE_list), "+-", sd(unRMSE_list), "\n")
#cat("Mean and sd of cfRelaxPred RMSE: ", mean(cfRelaxRMSE_list), "+-", sd(cfRelaxRMSE_list), "\n")
#cat("Mean and sd of tcfRelaxPred RMSE: ", mean(tcfRMSE_list), "+-", sd(tcfRMSE_list), "\n")
#cat("Mean and sd of cfPred RMSE: ", mean(cfRMSE_list), "+-" ,sd(cfRMSE_list) , "\n")


## 6. --- Fit the counterfactual data ---
counterfactual_data = read.csv(paste0(filename, "/counterfactual_data_", k, ".csv"), header=FALSE)
CF_Data = as.matrix(counterfactual_data)
Y = CF_Data[,outcome]
A = CF_Data[,protected]
X_defNonDes = CF_Data[,unlist(defNonDes)[!unlist(defNonDes)==outcome]]
X_NonDes = CF_Data[,NonDes[!NonDes==outcome]]
X_possDes = CF_Data[,unlist(possDes)[!unlist(possDes)==outcome]]
X_defDes = CF_Data[,unlist(defDes)[!unlist(defDes) %in% c(protected,outcome)]]

# counterfactual fair model
CF_Dataset= as.data.frame(cbind(X_defNonDes, Y))
colnames(CF_Dataset)[-length(CF_Dataset[1,])] <- paste("V", 1:(length(CF_Dataset[1,])-1), sep = "")
if(ncol(CF_Dataset)>1)
{
  CF_Dataset_cfPred <- predict(cfMod, CF_Dataset)
}else{CF_cfPred <- NA; CF_Dataset_cfPred<-NA}

# counterfactual fair model in true DAG
CF_Dataset2= as.data.frame(cbind(X_NonDes, Y))
colnames(CF_Dataset2)[-length(CF_Dataset2[1,])] <- paste("V", 1:(length(CF_Dataset2[1,])-1), sep = "")
if(ncol(CF_Dataset2)>1)
{
  CF_Dataset_tcfPred <- predict(tcfMod, CF_Dataset2)
}else{CF_tcfPred <- NA; CF_Dataset_tcfPred<-NA}

# relaxed counterfactual fair model
CF_Dataset= cbind(X_possDes, CF_Dataset)
colnames(CF_Dataset)[-length(CF_Dataset[1,])] <- paste("V", 1:(length(CF_Dataset[1,])-1), sep = "")
if(ncol(CF_Dataset)>1)
{
  CF_Dataset_cfRelaxPred <- predict(cfRelaxMod, CF_Dataset)
}else{CF_cfPred <- NA; CF_Dataset_cfRelaxPred<-NA}

# unawareness model
CF_Dataset= cbind(X_defDes, CF_Dataset)
colnames(CF_Dataset)[-length(CF_Dataset[1,])] <- paste("V", 1:(length(CF_Dataset[1,])-1), sep = "")
CF_Dataset_unPred <- predict(unMod, CF_Dataset)

# full model
CF_Dataset= cbind(A, CF_Dataset)
CF_Dataset_fullPred <- predict(fullMod, CF_Dataset)



## 7. --- Fairness Performance ---
## --- Plot the prediction distribution for both observational and counterfactual data ---
Dataset$cfPred <- Dataset_cfPred
Dataset$tcfPred <- Dataset_tcfPred
Dataset$cfRelaxPred <- Dataset_cfRelaxPred
Dataset$unPred <- Dataset_unPred
Dataset$fullPred <- Dataset_fullPred

CF_Dataset$cfPred <- CF_Dataset_cfPred
CF_Dataset$tcfPred <- CF_Dataset_tcfPred
CF_Dataset$cfRelaxPred <- CF_Dataset_cfRelaxPred
CF_Dataset$unPred <- CF_Dataset_unPred
CF_Dataset$fullPred <- CF_Dataset_fullPred

Unfair_cf <- abs(Dataset$cfPred - CF_Dataset$cfPred)[1]
Unfair_tcf <- abs(Dataset$tcfPred - CF_Dataset$tcfPred)[1]
Unfair_cfRelax <- abs(Dataset$cfRelaxPred - CF_Dataset$cfRelaxPred)[1]
Unfair_un <- abs(Dataset$unPred - CF_Dataset$unPred)[1]
Unfair_full <- abs(Dataset$fullPred - CF_Dataset$fullPred)[1]

unfair_results <- data.frame(Full=c(round(Unfair_full,digits = 3)),
                Unaware=c(round(Unfair_un,digits = 3)),
                FairRelax=c(round(Unfair_cfRelax,digits = 3)),
                Oracle=c(round(Unfair_tcf,digits = 3)),
                Fair=c(round(Unfair_cf,digits = 3)))

# unfairness_results_file = paste0(filename, '/Unfairness.csv')
# write.table(unfair_results, unfairness_results_file, sep = ",", append = T, quote = FALSE,
#             col.names = !file.exists(unfairness_results_file), row.names=paste(d,"nodes",s,"edges",k, sep=''))




############### Fairness Density Plot #######################################
# CF_Dataset$datatype <- 'counterfactual data'
# Dataset$datatype <- 'original data'
# sampleData <- rbind(Dataset[Dataset$A==1,], CF_Dataset[CF_Dataset$A==0,])


# pdf(paste0("./Repository", "/Densityplot_", d, "nodes", s, "edges_",k, ".pdf"), width = 14, height = 3, onefile=FALSE)
# fullplot <- ggplot(sampleData, aes(x=fullPred, fill=datatype)) + geom_density(alpha=.5) +
#   labs(x=expression(~hat(Y)~(Full)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         legend.title = element_blank(),
#         axis.title=element_text(size = 20))
# unplot <- ggplot(sampleData, aes(x=unPred, fill=datatype)) + geom_density(alpha=.5) +
#   labs(x=bquote(hat(Y)~(Unaware)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         axis.title=element_text(size = 20),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# cfRelaxplot <- ggplot(sampleData, aes(x=cfRelaxPred, fill=datatype)) + geom_density(alpha=.5) +
#   labs(x=bquote(hat(Y)~(FairRelax)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         axis.title=element_text(size = 20),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# tcfplot <- ggplot(sampleData, aes(x=tcfPred, fill=datatype)) + geom_density(alpha=.5) +
#   labs(x=bquote(hat(Y)~(Oracle)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         axis.title=element_text(size = 20),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# cfplot <- ggplot(sampleData, aes(x=cfPred, fill=datatype)) + geom_density(alpha=.5) +
#   labs(x=bquote(hat(Y)~(Fair)), y='density', color="") +
#   scale_fill_manual(values = c("#F4A582","#92C5DE")) +
#   theme(legend.position = "none",
#         legend.text=element_text(size=20),
#         axis.title=element_text(size = 20),
#         legend.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank())
# ggarrange(fullplot, unplot, cfRelaxplot, tcfplot, cfplot, nrow=1, common.legend = TRUE)
# dev.off()
