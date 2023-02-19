rm(list=ls())
options(warn = -1)
library(reshape)
library(ggplot2)
library(RColorBrewer)

# number of nodes
dd = c(10,20,30,40)
# number of edges
ss = c(20,40,60,80)

filename = paste0("./Repository")

############# RMSE ###############
df_RMSE=as.data.frame(matrix(nrow=0, ncol = 6))
for(i in 1:length(dd))
{
  d=dd[i]
  s=ss[i]
  rawRMSE <- read.csv(paste0(filename, "/", d, "nodes", s, "edges", "/RMSE.csv", sep=""))
  RMSE <- na.omit(rawRMSE)
  # cutoff = which(RMSE$Fair<5)
  cutoff = which(RMSE$Fair<20)
  RMSE <- RMSE[cutoff,]
  meltRMSE <- melt(RMSE)
  graph <- rep(paste(d, "nodes", s, "edges", sep = ''), nrow(meltRMSE))
  meltRMSE$Graph <- graph 
  df_RMSE=rbind(df_RMSE, meltRMSE)
}

### boxplot
pdf(paste0(filename, "/RMSE_Boxplot_cleaned.pdf"), width = 8.5, height = 5)
plotRMSE <- ggplot(df_RMSE, aes(x=factor(variable), y=value, fill=Graph)) + geom_boxplot() +
  labs(x='', y='RMSE', color="") + #  theme_classic()
  #coord_cartesian(ylim = c(0, 2.0)) + 
  coord_cartesian(ylim = c(0, 8)) + 
  scale_fill_manual(values = c("#F4A582","#FDDBC7","#D1E5F0","#92C5DE")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size = 15),
        axis.title=element_text(size = 20), 
        axis.text.x = element_text(size=20))
plotRMSE
dev.off()


############# Unfairness ###############
df_Unfairness = as.data.frame(matrix(nrow=0, ncol = 6))
for(i in 1:length(dd))
{
  d=dd[i]
  s=ss[i]
  rawUnfairness <- read.csv(paste0(filename, "/", d, "nodes", s, "edges", "/Unfairness.csv", sep=""))
  Unfairness <- na.omit(rawUnfairness)
  # cutoff = which(Unfairness$Full<1.5)
  cutoff = which(Unfairness$Full<2.0)
  Unfairness <- Unfairness[cutoff,]
  meltUnfairness <- melt(Unfairness)
  graph <- rep(paste(d, "nodes", s, "edges", sep = ''), nrow(meltUnfairness))
  meltUnfairness$Graph <- graph 
  df_Unfairness = rbind(df_Unfairness, meltUnfairness)
}

### boxplot
pdf(paste0(filename, "/Unfairness_Boxplot_cleaned.pdf"), width = 8.5, height = 5)
plotUnfairness <- ggplot(df_Unfairness, aes(x=factor(variable), y=value, fill=Graph)) + geom_boxplot() +
  labs(x='', y='Unfairness', color="") + 
  #coord_cartesian(ylim = c(0, 0.5)) +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_fill_manual(values = c("#F4A582","#FDDBC7","#D1E5F0","#92C5DE")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=15),
        axis.title=element_text(size = 20), 
        axis.text.x = element_text(size=20))
plotUnfairness
dev.off()





