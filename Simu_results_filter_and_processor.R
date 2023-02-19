rm(list=ls())
options(warn = -1)
library(reshape)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
# number of nodes
d = args[1]
# number of edges
s = args[2]
filename = paste0("./Repository")


############# RMSE ###############
rawRMSE <- read.csv(paste0(filename, "/", d, "nodes", s, "edges", "/RMSE.csv", sep=""))
RMSE <- na.omit(rawRMSE)
# cutoff = which(RMSE$Fair<5)
cutoff = which(RMSE$Fair<20)
RMSE <- RMSE[cutoff,]
RMSE_mean <- colMeans(RMSE)
RMSE_sd <- apply(RMSE, 2, sd)
RMSE_results_file = paste0(filename, '/RMSE_avg_cleaned.csv')
RMSE_results <- data.frame(Full=c(paste(round(RMSE_mean[1],digits = 3), "±", round(RMSE_sd[1],digits = 3), sep = '')),
                           Unaware=c(paste(round(RMSE_mean[2],digits = 3), "±", round(RMSE_sd[2],digits = 3), sep = '')),
                           FairRelax=c(paste(round(RMSE_mean[3],digits = 3), "±", round(RMSE_sd[3],digits = 3), sep = '')),
                           Oracle=c(paste(round(RMSE_mean[4],digits = 3), "±", round(RMSE_sd[4],digits = 3), sep = '')),
                           Fair=c(paste(round(RMSE_mean[5],digits = 3), "±", round(RMSE_sd[5],digits = 3), sep = '')))
#write_csv(results, results_file, row_names =paste(d,"nodes",s,"edges",sep=''))
write.table(RMSE_results, RMSE_results_file, sep = ",", append = T, quote = FALSE,
            col.names = !file.exists(RMSE_results_file), row.names=paste(d,"nodes",s,"edges", sep=''))

### boxplot
# pdf(paste0(filename, "/RMSE_boxplot_", d, "nodes", s, "edges", "_cleaned.pdf"), width = 7, height = 5)
# meltRMSE <- melt(RMSE)
# plotRMSE <- ggplot(meltRMSE, aes(factor(variable), value)) + geom_boxplot() +
#   labs(x='models', y='RMSE', color="") + #  theme_classic()
#   theme(legend.text=element_text(size=10),
#         axis.title=element_text(face = "bold",size = 10))
# plotRMSE
# dev.off()

########### Unfairness ##############
rawUnfairness <- read.csv(paste0(filename, "/", d, "nodes", s, "edges", "/Unfairness.csv", sep=""))
Unfairness <- na.omit(rawUnfairness)
# cutoff = which(Unfairness$Full<1.5)
cutoff = which(Unfairness$Full<2.0)
Unfairness <- Unfairness[cutoff,]
Unfairness_mean <- colMeans(Unfairness)
Unfairness_sd <- apply(Unfairness, 2, sd)
Unfairness_results_file = paste0(filename, '/Unfairness_avg_cleaned.csv')
Unfairness_results <- data.frame(Full=c(paste(round(Unfairness_mean[1],digits = 3), "±", round(Unfairness_sd[1],digits = 3), sep = '')),
                                 Unaware=c(paste(round(Unfairness_mean[2],digits = 3), "±", round(Unfairness_sd[2],digits = 3), sep = '')),
                                 FairRelax=c(paste(round(Unfairness_mean[3],digits = 3), "±", round(Unfairness_sd[3],digits = 3), sep = '')),
                                 Oracle=c(paste(round(Unfairness_mean[4],digits = 3), "±", round(Unfairness_sd[4],digits = 3), sep = '')),
                                 Fair=c(paste(round(Unfairness_mean[5],digits = 3), "±", round(Unfairness_sd[5],digits = 3), sep = '')))
#write_csv(results, results_file, row_names =paste(d,"nodes",s,"edges",sep=''))
write.table(Unfairness_results, Unfairness_results_file, sep = ",", append = T, quote = FALSE,
            col.names = !file.exists(Unfairness_results_file), row.names=paste(d,"nodes",s,"edges", sep=''))

### boxplot
# pdf(paste0(filename, "/Unfairness_boxplot_", d, "nodes", s, "edges", "_cleaned.pdf"), width = 7, height = 5)
# meltUnfairness <- melt(Unfairness)
# plotUnfairness <- ggplot(meltUnfairness, aes(factor(variable), value))  + geom_boxplot() +
#  # coord_cartesian(ylim = c(0, 1)) +
#   labs(x='models', y='Unfairness', color="") + #  theme_classic()
#   theme(legend.text=element_text(size=10),
#         axis.title=element_text(face = "bold",size = 10))
# plotUnfairness
# dev.off()

