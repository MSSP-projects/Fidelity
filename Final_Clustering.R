# Title: Clustering Analysis based on time series distance
# Author: Shuting Li, Ada Yu 

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(TSdist)
library(lubridate)
library(tibble)
library(factoextra)
library(NbClust)
library(pheatmap)

source("ChangePointsAnalysis.R")

## DATA PREPROCESSING
##########################################################
# First step: subset to warahouse used at least once in 2021 based on change point analysis
actual_weekly214 <- actual_weekly %>% 
  mutate(Start_Week = as.Date(Start_Week),
         End_Week = as.Date(End_Week)) %>%
  filter(NEWID %in% seg_information$NEWID)

execution_weekly214 <- execution_weekly %>% 
  mutate(Start_Week = as.Date(Start_Week),
         End_Week = as.Date(End_Week)) %>%
  filter(NEWID %in% seg_information$NEWID)

seg_information <- seg_information %>% 
  mutate(Start_Week=as.Date(Start_Week))

# Second step: Calculate the time series distance between actual and execution credit for each warehouse after the last changepoint to 2021-08-01.
j <- 1
dis_ac_exe <- rep(NA,214)
acct_ave <- rep(NA,214)
exec_ave <- rep(NA,214)

for (i in 1:214){
  actual_0 <- actual_weekly214 %>% filter(NEWID == as.character(seg_information[i,2]) & Start_Week >= seg_information[i,5])  
  exe_0 <- execution_weekly214 %>% filter(NEWID == as.character(seg_information[i,2]) & Start_Week >= seg_information[i,5]) 
  
  actual <- actual_0[,3] 
  exe <- exe_0[,3]
  acct_ave[j]<-mean(unlist(actual))
  exec_ave[j]<-mean(unlist(exe))
  dis_ac_exe[j] <- DTWDistance(actual,exe,window.type="sakoechiba",window.size=1)/nrow(actual_0)
  j <- j+1
}

dis_ac_exe_final <- data.frame(NEWID=seg_information$NEWID,actual=acct_ave, executed=exec_ave,distance=dis_ac_exe)

set.seed(1)
# pheatmap(log(dis_ac_exe_final[,-1]+1))
fitres <- pheatmap(log(dis_ac_exe_final[,-1]+1),kmeans_k = 4)
#Setp 3: add clustering info to original dataset
df_ac_exe_cluster <- cbind(dis_ac_exe_final, cluster = fitres$kmeans$cluster)
# Final cluster result
cluster_info <- df_ac_exe_cluster %>% select(NEWID,cluster)


a <- merge(actual_weekly214,df_ac_exe_cluster,by="NEWID")

b <- a %>% 
  group_by(cluster,week) %>%
  mutate(group_week= sum(weekly_sum)) %>%
  distinct(week,cluster,group_week)

ggplot(b, aes(x=week, y=group_week,group=cluster,color=cluster)) +
  geom_line()  


c <- merge(execution_weekly214,df_ac_exe_cluster,by="NEWID")

d <- c %>% 
  group_by(cluster,week) %>%
  mutate(group_week= sum(weekly_sum)) %>%
  distinct(week,cluster,group_week)

ggplot(d, aes(x=week, y=group_week,group=cluster,color=cluster)) +
  geom_line()


cluster_info <- df_ac_exe_cluster %>% select(NEWID,cluster)
