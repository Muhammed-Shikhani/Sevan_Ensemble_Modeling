library(yaml)
library(LakeEnsemblR)
library(gotmtools)
setwd( "/home/shikhani/Documents/LER_Sevan_trial2/")
dir.data <- "/home/shikhani/Documents/LER_Sevan_trial2/rdata_example_LER/"
rdata_files <- list.files( "/home/shikhani/Documents/LER_Sevan_trial2/rdata_example_LER/")
#rdata_files <- rdata_files[7]
length(rdata_files)
sim_folder <- "/home/shikhani/Documents/LER_Sevan_trial2/"
yaml_file <- file.path(sim_folder,'LakeEnsemblR.yaml')
yaml_vlaues <- read_yaml(yaml_file)
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
model <- c("MyLake")
source("function_make_met_file.R")
source("function_inflow_temp_model.R")

rdata_files_unlist <- unlist(strsplit(rdata_files,split='_', fixed=TRUE))
rcp_seq <- seq(2,length(rdata_files_unlist ),7)
my_rcps <- unique(rdata_files_unlist[rcp_seq])

for(j in 1:length(my_rcps)){
  
  
  files_rcp <- rdata_files[which(grepl(my_rcps[j], rdata_files))]
  
  
  if(my_rcps[j] == "historical"){
    my_dates <- seq.Date(as.Date("1970-01-01"), as.Date("2005-12-30"),1)}else{
      my_dates <- seq.Date(as.Date("2006-01-01"), as.Date("2100-12-30"),1)}
  
  
  
  
  for (ii in 1:length(files_rcp)) {
    start.time <- Sys.time()
    files_rcp_unlist <- unlist(strsplit(files_rcp[ii],split='_', fixed=TRUE))
    cordex_name <- paste(files_rcp_unlist[1],files_rcp_unlist[3],files_rcp_unlist[4],my_rcps[j],sep = "_" )
    
    met_cordex <-get(load(paste0(dir.data,files_rcp[ii])))
    
    make_met_file(met_cordex)
    inflow_temp_model(met_cordex)
    
    start_date =paste0(as.character(my_dates[1]), " 00:00:00")  
    stop_date =paste0( as.character(my_dates[length(my_dates)]), " 00:00:00")  
    input_yaml(file = "LakeEnsemblR.yaml", label = 'time', key = 'start', value = start_date)
    input_yaml(file = "LakeEnsemblR.yaml", label = 'time', key = 'stop', value = stop_date)
    input_yaml(file = "LakeEnsemblR.yaml", label = 'inflows', key = 'use', value = 'true')
    input_yaml(file = "LakeEnsemblR.yaml", label = 'outflows', key = 'use', value = 'true')
    input_yaml(file = "LakeEnsemblR.yaml", label = 'output', key = 'file', value = paste("ensemble_output_MyLake", cordex_name, sep = "_"))
    config_file <- 'LakeEnsemblR.yaml'
    export_config(config_file = config_file, model = model)
    
    
    run_ensemble(config_file = config_file, model = model,parallel = F,verbose = F)
    
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(cordex_name)
    print(time.taken)
  }
  print(my_rcps[j])
}
