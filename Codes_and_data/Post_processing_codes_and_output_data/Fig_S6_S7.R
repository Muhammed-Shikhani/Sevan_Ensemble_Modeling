setwd("/home/shikhani/Documents/Sevan_plotting_draft/revison/")

library(lubridate)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
library(tidyr)
library(xtable)
library(dplyr)
library(reshape2)
# Set config file & models
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

ncdf <- 'ensemble_output_calib.nc'

###############
nc_data <- load_var(ncdf,var = "temp")

my_depths <- c("datetime",as.numeric(unlist(strsplit(names(nc_data$Simstrat)[-1],split = "_"))[seq(2,336,2)]))
my_depths <-my_depths[1:86]
my_depths[2:86] <- as.numeric(my_depths[2:86])
nc_data <-lapply(nc_data, setNames, my_depths )
mydata <- nc_data


df_list <- lapply(1:length(mydata), 
                  function(x) (pivot_longer(mydata[[x]],-"datetime")))

names(df_list) <- names(nc_data)
df_4_mean <- df_list[-6]

my_mean <- rowMeans(sapply(df_4_mean, "[[", "value"),na.rm = T)
my_mean[is.nan(my_mean)] <- NA
df_list$Ensemble_Mean <- df_list$GLM
df_list$Ensemble_Mean$value <- my_mean

df <- df_list$FLake
df$GLM <- df_list$GLM$value
df$GOTM <- df_list$GOTM$value
df$Simstrat <- df_list$Simstrat$value
df$MyLake <- df_list$MyLake$value
df$Obs <- df_list$Obs$value
df$Ensemble_Mean <- df_list$Ensemble_Mean$value
names(df)[2:3] <- c("Depth", "Flake") 
head(df)


df_obs <- df


df_obs$Depth_Category <- cut(as.numeric(as.character(df_obs$Depth)), 
                             breaks = c(0, 0.1, 5, 20, max(as.numeric(as.character(df_obs$Depth), na.rm = TRUE))),
                             labels = c("0.1", "1-5", "5-20", "30-bottom"),
                             include.lowest = TRUE)

df_obs$Season <- factor(ifelse(month(df_obs$datetime) %in% c(12, 1, 2), "Winter",
                               ifelse(month(df_obs$datetime) %in% c(3, 4, 5), "Spring",
                                      ifelse(month(df_obs$datetime) %in% c(6, 7, 8), "Summer", "Fall"))),
                        levels = c("Winter", "Spring", "Summer", "Fall"))

df_long <- melt(df_obs, id.vars = c("datetime", "Depth_Category", "Obs", "Season"), 
                measure.vars = c("Flake", "GLM", "GOTM", "Simstrat", "MyLake", "Ensemble_Mean"),
                variable.name = "Model", value.name = "Model_Value")

plot <- ggplot(df_long, aes(x = Model_Value, y = Obs, colour = Depth_Category, shape = Season)) +
  geom_point(size = 3) +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # 1:1 
  facet_wrap(~Model, scales = "free") +  
  labs(title = "",
       x = "Model Output",
       y = "Observed Data") +
  theme_bw() +
  scale_colour_manual(values = c("0.1" = "#56B4E9", "1-5" = "#009E73", "5-20" = "#C40233", "30-bottom" = "#D55E00")) +  
  scale_shape_manual(values = c(15, 16, 17, 18))  

ggsave("scatter_calibirated.png", plot = plot, width = 10, height = 8, dpi = 300)

####################




ncdf <- 'ensemble_output_validate.nc'

###############
nc_data <- load_var(ncdf,var = "temp")

my_depths <- c("datetime",as.numeric(unlist(strsplit(names(nc_data$Simstrat)[-1],split = "_"))[seq(2,336,2)]))
my_depths <-my_depths[1:86]
my_depths[2:86] <- as.numeric(my_depths[2:86])
nc_data <-lapply(nc_data, setNames, my_depths )
mydata <- nc_data


df_list <- lapply(1:length(mydata), 
                  function(x) (pivot_longer(mydata[[x]],-"datetime")))

names(df_list) <- names(nc_data)
df_4_mean <- df_list[-6]

my_mean <- rowMeans(sapply(df_4_mean, "[[", "value"),na.rm = T)
my_mean[is.nan(my_mean)] <- NA
df_list$Ensemble_Mean <- df_list$GLM
df_list$Ensemble_Mean$value <- my_mean

df <- df_list$FLake
df$GLM <- df_list$GLM$value
df$GOTM <- df_list$GOTM$value
df$Simstrat <- df_list$Simstrat$value
df$MyLake <- df_list$MyLake$value
df$Obs <- df_list$Obs$value
df$Ensemble_Mean <- df_list$Ensemble_Mean$value
names(df)[2:3] <- c("Depth", "Flake") 
head(df)


df_obs <- df


df_obs$Depth_Category <- cut(as.numeric(as.character(df_obs$Depth)), 
                             breaks = c(0, 0.1, 5, 20, max(as.numeric(as.character(df_obs$Depth), na.rm = TRUE))),
                             labels = c("0.1", "1-5", "5-20", "30-bottom"),
                             include.lowest = TRUE)

df_obs$Season <- factor(ifelse(month(df_obs$datetime) %in% c(12, 1, 2), "Winter",
                               ifelse(month(df_obs$datetime) %in% c(3, 4, 5), "Spring",
                                      ifelse(month(df_obs$datetime) %in% c(6, 7, 8), "Summer", "Fall"))),
                        levels = c("Winter", "Spring", "Summer", "Fall"))

df_long <- melt(df_obs, id.vars = c("datetime", "Depth_Category", "Obs", "Season"), 
                measure.vars = c("Flake", "GLM", "GOTM", "Simstrat", "MyLake", "Ensemble_Mean"),
                variable.name = "Model", value.name = "Model_Value")

plot <- ggplot(df_long, aes(x = Model_Value, y = Obs, colour = Depth_Category, shape = Season)) +
  geom_point(size = 3) +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # 1:1 
  facet_wrap(~Model, scales = "free") +  
  labs(title = "",
       x = "Model Output",
       y = "Observed Data") +
  theme_bw() +
  scale_colour_manual(values = c("0.1" = "#56B4E9", "1-5" = "#009E73", "5-20" = "#C40233", "30-bottom" = "#D55E00")) +  
  scale_shape_manual(values = c(15, 16, 17, 18))  

ggsave("scatter_valid.png", plot = plot, width = 10, height = 8, dpi = 300)
