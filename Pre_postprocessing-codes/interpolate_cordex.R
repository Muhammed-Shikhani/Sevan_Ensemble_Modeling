options(java.parameters = "-Xmx10000m")

library(transformeR)
library(loadeR.ECOMS)
library(loadeR)
library(downscaleR)
library(lubridate)
library(visualizeR)
library(RNetCDF) 
library(sp)
library(ncdf4)
library(raster)
library(drought4R)
library(loadeR.2nc)

lakename <- "Lake Sevan"
setwd("/media/shikhani/My Passport/nc_fixed/")
dir.data <- "/media/shikhani/My Passport/nc_fixed/"
dir.Rdata <- "/media/shikhani/Samsung_T5/cordex/rdata_fixed/"
files_all <- list.files("/media/shikhani/My Passport/nc_fixed/", ".nc", recursive = F)
t=0
x =0
lake <- list(x = 45.3, y = 40.4)

#1 d
files_all_unlist <- unlist(strsplit(files_all,split='_', fixed=TRUE))
dom_seq <- seq(2,length(files_all_unlist),9)
my_domains <- unique(files_all_unlist[dom_seq])
#my_domains <-my_domains[2]
for(d in 1:length(my_domains)){
  
  files_dom <- files_all[which(grepl(my_domains[d], files_all))]
  
  #2 p
  files_dom_unlist <- unlist(strsplit(files_dom,split='_', fixed=TRUE))
  rcp_seq <- seq(4,length(files_dom_unlist),9)
  my_rcps <- unique(files_dom_unlist[rcp_seq])
  # my_rcps <- my_rcps[1]
  
  for(p in 1:length(my_rcps)){
    
    files_rcp <- files_dom[which(grepl(my_rcps[p], files_dom))]
    
    #3  g
    files_rcp_unlist <- unlist(strsplit(files_rcp,split='_', fixed=TRUE))
    gcm_seq <- seq(3,length(files_rcp_unlist),9)
    my_gcms <- unique(files_rcp_unlist[gcm_seq])
    
    
    for(g in 1:length(my_gcms)){
      
      
      files_gcm <- files_rcp[which(grepl(my_gcms[g], files_rcp))]
      #4 r
      files_gcm_unlist <- unlist(strsplit(files_gcm,split='_', fixed=TRUE))
      rcm_seq <- seq(6,length(files_gcm_unlist),9)
      my_rcms <- unique(files_gcm_unlist[rcm_seq])
      
      for(r in 1:length(my_rcms)){
        start.time2 <- Sys.time()
        files_rcm <- files_gcm[which(grepl(my_rcms[r], files_gcm))]
        #5 v
        files_rcm_unlist <- unlist(strsplit(files_rcm,split='_', fixed=TRUE))
        var_seq <- seq(1,length(files_rcm_unlist),9)
        my_vars <- unique(files_rcm_unlist[var_seq])
        #    file.copy(from = paste0("/media/shikhani/My Passport/Linux/All_nc/", files_rcm) , to ="/media/shikhani/My Passport/Linux/All_nc/my_copy/" , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
        
        year_list <- list()
        var_list <- list()
        for (v in 1:length(my_vars)){
          c1<-0
          files_var <- files_rcm[which(grepl(my_vars[v] , files_rcm))]
          
          for (y in files_var){
            c1 <- c1+1
            
            start.time <- Sys.time()
            data_nc <- loadGridData(y, var = my_vars[v])
            year_list[[c1]] <-  interpGrid(data_nc,  new.coordinates = lake, method = "bilinear",  bilin.method = "akima")
             print(y)
            x= x+1
            print(paste0(round(x*100/length(files_all),3), "% completed"))
            print(summary(year_list[[c1]]$Data))
            
            end.time <- Sys.time()
            time.taken <- end.time - start.time
            print(time.taken)
          }
          var_list[[v]] <- bindGrid(year_list, dimension = c("time"))
        }
        names(var_list) <- my_vars 
        data_rcp <-  var_list
        
        list_name <- paste(my_domains[d],my_rcps[p],my_gcms[g],my_rcms[r], "fixed_interpolated_v2.RData",sep = "_")
        save(data_rcp, file= paste(dir.Rdata, list_name, sep = "/") )
        t=t+1
        print(paste(t, list_name, sep = "  "))
        
        
        end.time2 <- Sys.time()
        time.taken2 <- end.time2 - start.time2
        print(time.taken2)
       # gc()
      }
    }
    
  }
}