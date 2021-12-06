# Title: Clustering Visualization based on time series distance
# Author: Shuting Li, Ada Yu 

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
source("Clustering.R")

# DATA VISUALIZATION

##Compare Actual Credit or Execution Credit for All Groups
### Actual credits
actual_week_sum <- merge(actual_weekly214,df_ac_exe_cluster,by="NEWID")

actual_group <- actual_week_sum %>% 
  group_by(cluster,week) %>%
  mutate(group_week= sum(weekly_sum)) %>%
  distinct(week,cluster,group_week)

ggplot(actual_group, aes(x=week, y=group_week,group=as.factor(cluster),color=as.factor(cluster))) +
  geom_line() +
  labs(title = "Weekly actual credits sum by different groups",
       x = "Week",
       y = "Sum of weekly actual credits",
       color = "Group labels")
  
### Execution credits
exe_week_sum <- merge(execution_weekly214,df_ac_exe_cluster,by="NEWID")

exe_group <- exe_week_sum %>% 
  group_by(cluster,week) %>%
  mutate(group_week= sum(weekly_sum)) %>%
  distinct(week,cluster,group_week)

ggplot(exe_group, aes(x=week, y=group_week,group=as.factor(cluster),color=as.factor(cluster))) +
  geom_line() +
  labs(title = "Weekly execution credits sum by different groups",
       x = "Week",
       y = "Sum of weekly Execution credits",
       color = "Group labels")


## Compare Actual & Execution Credit for each Group
actual_exe_group <- inner_join(actual_group,exe_group, by=c("week","cluster"), suffix = c("_actual", "_exe"))
actual_exe_group_long <- reshape2::melt(actual_exe_group, id.vars = c("week","cluster"))

ggplot(actual_exe_group_long)+
  aes(x=week, y=value, color=variable)+
  geom_line()+
  geom_smooth(method = "lm", se=F, alpha=0.01)+
  facet_grid(~cluster)+
  labs(title = "Weekly actual & execution credits sum by different groups",
       x = "Week",
       y = "Sum of weekly credits",
       color = "Credit labels")

