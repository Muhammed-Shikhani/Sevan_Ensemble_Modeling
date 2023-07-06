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
setwd("/media/shikhani/My Passport/Linux/All_nc/")
#dir.data <- "/media/shikhani/My Passport/Linux/All_nc/"
#dir.Rdata <- "/media/shikhani/Samsung_T5/cordex/rdata_fixed/"
files_all <- list.files("/media/shikhani/My Passport/Linux/All_nc/", ".nc", recursive = F)
t=0
x =0

#d=1
#p=1
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
    #my_gcms <-my_gcms[2:3]
    
    for(g in 1:length(my_gcms)){
      
      
      files_gcm <- files_rcp[which(grepl(my_gcms[g], files_rcp))]
      #4 r
      files_gcm_unlist <- unlist(strsplit(files_gcm,split='_', fixed=TRUE))
      rcm_seq <- seq(6,length(files_gcm_unlist),9)
      my_rcms <- unique(files_gcm_unlist[rcm_seq])
      
      for(r in 1:length(my_rcms)){
        
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
         
            system(paste("cdo remapbil,grid.txt", y ,  paste0("/media/shikhani/Samsung_T5/cordex/draft/",y)))
            file.copy(from = paste0("//media/shikhani/Samsung_T5/cordex/draft/", y) , to ="/media/shikhani/My Passport/nc_fixed/" , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
            print(y)
            x= x+1
            print(paste0(round(x*100/length(files_all),3), "% completed"))
            file.remove(paste0("/media/shikhani/Samsung_T5/cordex/draft/",y))
            end.time <- Sys.time()
            time.taken <- end.time - start.time
            print(time.taken)
          }
        }
      }
    }
  }
}
