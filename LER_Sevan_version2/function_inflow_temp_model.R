inflow_temp_model <- function(list){
  library(segmented)
  df <- data.frame(tas=list$tas$Data)
  
  lm_data <- read.csv("lm_data.csv")
  my_lm <- lm(water_temp~ tas,data = lm_data)
  my_seg<-segmented(my_lm, npsi = 1)
  new_tmp <-predict(my_seg, newdata = df)
  my_inflow <- read.csv("LakeEnsemblR_inflow_standard.csv")
  my_inflow$Water_Temperature_celsius[which(as.Date(my_inflow$datetime)%in%as.Date(list$tas$Dates$start))] <-  new_tmp
  write.csv(my_inflow,"LakeEnsemblR_inflow_standard.csv", quote = F, row.names = F)
  
}