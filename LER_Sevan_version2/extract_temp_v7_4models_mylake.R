library(lubridate)
library(ggplot2)
library(reshape2)
library(gotmtools)
library(LakeEnsemblR)
setwd("/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/")
dir.data <- "/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/4models_output/"
nc_files <- list.files("/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/4models_output/", pattern = ".nc")
length(nc_files)
#model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
model <- c("FLake", "GLM", "GOTM", "Simstrat")

my_vars <-  c("Surface_Temperature", "Bottom_Temperature")

nc_files_unlist <- unlist(strsplit(nc_files,split='_', fixed=TRUE))
rcp_seq <- seq(10,length(nc_files_unlist ),10)
my_rcps <- unique(nc_files_unlist[rcp_seq])
my_rcps <- unique(unlist(strsplit(my_rcps,split='.', fixed=TRUE))[seq(1,length(unlist(strsplit(my_rcps,split='.', fixed=TRUE))),2)])  
c <- 0
var_list <- list()
for (v in 1:length(my_vars)) {
  model_list <- list()
  for(k in 1:length(model)){
    rcp_list <- list()
    for( j in 1:length(my_rcps)){
      files_rcp <- nc_files[which(grepl(my_rcps[j], nc_files))]
      
      if(my_rcps[j] == "historical"){
        my_dates <- seq.Date(as.Date("1970-01-01"), as.Date("2005-12-30"),1)}else{
          my_dates <- seq.Date(as.Date("2006-01-01"), as.Date("2100-12-30"),1)}
      
      data <-  data.frame(DateTime=my_dates,as.data.frame(matrix(data=NA,nrow=length(my_dates),ncol=length(files_rcp)))) 
      for (ii in 1:length(files_rcp)) {
        files_rcp_unlist <- unlist(strsplit(files_rcp[ii],split='_', fixed=TRUE))
        
        cordex_name <- paste(files_rcp_unlist[7],files_rcp_unlist[8],files_rcp_unlist[9],sep = "_" )
        ncdf<- paste0(dir.data,files_rcp[ii])
        
        
        if(my_vars[v]== "Surface_Temperature"){
          gc()
          nc_data <- load_var(ncdf,var = "temp")
          data[,ii+1] <- nc_data[[k]][,3]
        }
        
        if(my_vars[v]== "Bottom_Temperature"){
          gc()
          nc_data <- load_var(ncdf,var = "temp")
          if(k==1){
            data[,ii+1] <- nc_data[[k]][,18]
          }else{
            data[,ii+1] <- nc_data[[k]][,(ncol( nc_data[[k]])-8)]
          }
        }
        
        
        # if(my_vars[v]== "Ice"){
        #   nc_data <- load_var(ncdf,var = "ice")
        #   data[,ii+1] <- nc_data[[k]]
        # }
        c <- c+1
        
        names(data)[ii+1] <- cordex_name
        print(paste(c,model[k],cordex_name, sep=" "))
        gc()
        
      }
      rcp_list[[j]] <- data
    }
    names(rcp_list) <- my_rcps
    model_list[[k]] <- rcp_list
    
  }
  
  names(model_list) <- model
  var_list[[v]] <- model_list
}
names(var_list) <- my_vars




save( var_list, file="cordex_LER_results_4models.rda")
############


dir.data <- "/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/MyLake_output/"
nc_files <- list.files("/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/MyLake_output/", pattern = ".nc")
length(nc_files)
model <- c("MyLake")

my_vars <-  c("Surface_Temperature", "Bottom_Temperature")

nc_files_unlist <- unlist(strsplit(nc_files,split='_', fixed=TRUE))
rcp_seq <- seq(7,length(nc_files_unlist ),7)
my_rcps <- unique(nc_files_unlist[rcp_seq])
my_rcps <- unique(unlist(strsplit(my_rcps,split='.', fixed=TRUE))[seq(1,length(unlist(strsplit(my_rcps,split='.', fixed=TRUE))),2)])  
c <- 0
var_list <- list()
for (v in 1:length(my_vars)) {
  model_list <- list()
  for(k in 1:length(model)){
    rcp_list <- list()
    for( j in 1:length(my_rcps)){
      files_rcp <- nc_files[which(grepl(my_rcps[j], nc_files))]
      
      if(my_rcps[j] == "historical"){
        my_dates <- seq.Date(as.Date("1970-01-01"), as.Date("2005-12-30"),1)}else{
          my_dates <- seq.Date(as.Date("2006-01-01"), as.Date("2100-12-30"),1)}
      
      data <-  data.frame(DateTime=my_dates,as.data.frame(matrix(data=NA,nrow=length(my_dates),ncol=length(files_rcp)))) 
      for (ii in 1:length(files_rcp)) {
        files_rcp_unlist <- unlist(strsplit(files_rcp[ii],split='_', fixed=TRUE))
        
        cordex_name <- paste(files_rcp_unlist[4],files_rcp_unlist[5],files_rcp_unlist[6],sep = "_" )
        ncdf<- paste0(dir.data,files_rcp[ii])
        
        
        if(my_vars[v]== "Surface_Temperature"){
          gc()
          nc_data <- load_var(ncdf,var = "temp")
          data[,ii+1] <- nc_data[[k]][,3]
        }
        
        if(my_vars[v]== "Bottom_Temperature"){
          gc()
          nc_data <- load_var(ncdf,var = "temp")
          if(k==1){
            data[,ii+1] <- nc_data[[k]][,18]
          }else{
            data[,ii+1] <- nc_data[[k]][,(ncol( nc_data[[k]])-8)]
          }
        }
        
        
        # if(my_vars[v]== "Ice"){
        #   nc_data <- load_var(ncdf,var = "ice")
        #   data[,ii+1] <- nc_data[[k]]
        # }
        c <- c+1
        
        names(data)[ii+1] <- cordex_name
        print(paste(c,model[k],cordex_name, sep=" "))
        gc()
        
      }
      rcp_list[[j]] <- data
    }
    names(rcp_list) <- my_rcps
    model_list[[k]] <- rcp_list
    
  }
  
  names(model_list) <- model
  var_list[[v]] <- model_list
}
names(var_list) <- my_vars




save( var_list, file="cordex_LER_results_mylake.rda")
############
########

########
