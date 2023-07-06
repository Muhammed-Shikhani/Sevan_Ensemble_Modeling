setwd("/media/shikhani/Samsung_T5/Cordex_work/calib_october_sw/")
library(lubridate)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
library(tidyr)
library(xtable)
library(dplyr)
# Set config file & models
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

ncdf <- 'output/ensemble_output.nc'



my_fit <- calc_fit(ncdf = ncdf,
                   model = model,
                   var = "temp")


###############
nc_data <- load_var(ncdf,var = "temp")

my_depths <- c("datetime",as.numeric(unlist(strsplit(names(nc_data$Simstrat)[-1],split = "_"))[seq(2,336,2)]))

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

rmse(df_list$Ensemble_Mean$value, df_list$Obs$value, na.rm = T)

obs_depth <-unique(  df_list$Obs$name[which(!is.na(df_list$Obs$value) & year(df_list$Obs$datetime)== 2013)])



res_all <- lapply(df_list[c(1:5,7)], function(x){
  rmse(x$value, df_list$Obs$value, na.rm = T)
  
  
})

res_depth <- list()
for(i in 1:length(obs_depth)){
  res_depth[[i]] <- lapply(df_list[c(1:5,7)], function(x){
    rmse(x$value[which(x$name==obs_depth[i])], df_list$Obs$value[which(df_list$Obs$name==obs_depth[i])], na.rm = T)
    
    
  })
}
names(res_depth) <- obs_depth
res_depth$all <- res_all
rmse_df <- data.frame(do.call(rbind, res_depth))



bias_all <- lapply(df_list[c(1:5,7)], function(x){
  
  bias_vectrs <- data.frame(x$value, df_list$Obs$value)
  bias_vectrs <- bias_vectrs[!is.na(bias_vectrs[,1]) & !is.na(bias_vectrs[,2]),]
  
  mean(bias_vectrs[,1])- mean(bias_vectrs[,2])
  
})




bias_depth <- list()
for(i in 1:length(obs_depth)){
  bias_depth[[i]] <- lapply(df_list[c(1:5,7)], function(x){
    
    
    bias_vectrs <- data.frame(x$value[which(x$name==obs_depth[i])], df_list$Obs$value[which(df_list$Obs$name==obs_depth[i])])
    bias_vectrs <- bias_vectrs[!is.na(bias_vectrs[,1]) & !is.na(bias_vectrs[,2]),]
    
    mean(bias_vectrs[,1])- mean(bias_vectrs[,2])  
  })
}
names(bias_depth) <- obs_depth
bias_depth$all <- bias_all
bias_df <- data.frame(do.call(rbind, bias_depth))

my_fit
rmse_df
bias_df
rmse_df_t <- t(round(do.call(rbind.data.frame, rmse_df),2))
rmse_df_t

bias_df_t <- t(round(do.call(rbind.data.frame, bias_df),2))
bias_df_t

df_all <- data.frame(Depth =rownames(bias_df))
for(i in 1:ncol(rmse_df_t)){
  df_all[,i+1] <- paste(rmse_df_t[,i], bias_df_t[,i],sep="|")
  
}
names(df_all)[-1] <- names(rmse_df)
df_all
df_all$Depth[2] <- "05"


df_ordered <- df_all %>%
  arrange(Depth)

df_ordered
table_merged <- xtable(df_ordered)

print(xtable(table_merged, type = "latex", include.rownames = FALSE))

