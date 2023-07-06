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


setwd("/home/shikhani/Documents/CORDEX_2021/rdata/rdata_raw/")
dir.Rdata <- "/home/shikhani/Documents/CORDEX_2021/rdata/rdata_BiasCorrected_hist_used/"

ERA5 <- get(load("/home/shikhani/Documents/CORDEX_2021/ERA5_daily_Lake_Sevan_1979_2020_interpolated_allvars_updatedps.rda"))


rdata_files <- list.files("/home/shikhani/Documents/CORDEX_2021/rdata/rdata_raw/",pattern = ".RData")
length(rdata_files)
rdata_files_unlist <- unlist(strsplit(rdata_files,split='_', fixed=TRUE))
dom_seq <- seq(1,length(rdata_files_unlist ),7)
my_domains <- unique(rdata_files_unlist[dom_seq])

t= 0
for(d in 1:length(my_domains)){
  
  files_dom <- rdata_files[which(grepl(my_domains[d], rdata_files))]

  files_dom_unlist <- unlist(strsplit(files_dom,split='_', fixed=TRUE))

  my_seq <-  c(seq(3,length(files_dom_unlist),7),seq(4,length(files_dom_unlist),7))
  my_seq <- my_seq[order(my_seq)]
  gcm_seq <- seq(3,length(files_dom_unlist),7)
  rcm_seq <- seq(4,length(files_dom_unlist),7)
  my_combos <- unique(paste(files_dom_unlist[gcm_seq],files_dom_unlist[rcm_seq], sep = "_"))
  
  
  for(c in 1:length(my_combos)){
 
    my_climate <- files_dom[which(grepl(my_combos[c], files_dom))]
    files_climate_unlist <- unlist(strsplit(my_climate,split='_', fixed=TRUE))
    rcp_seq <- seq(2,length(files_climate_unlist),7)
    my_rcps <- unique(files_climate_unlist[rcp_seq])
    
    for(p in 1:length(my_rcps)){
      files_rcp <- my_climate[which(grepl(my_rcps[p], my_climate))]
      
      for(l in 1:length(files_rcp)){
        
        start.time <- Sys.time()
       
        data_cordex <- get(load(files_rcp[l]))
        
        
        if("cc"%in% names(data_cordex)){
          names(data_cordex)[which(names(data_cordex)=="cc")] <- "clt"
        }else{
          data_cordex$clt$Data <- data_cordex$clt$Data/100
          attr(data_cordex$clt$Variable,"units") <- "1" 
        }
        if(my_domains[d]== "CAS-22"){
          data_cordex$vas$Data <- data_cordex$vas$Data+runif(n = length(data_cordex$vas$Data), min = 1, max = 10)
        }
        data_cordex$tas$Data <- data_cordex$tas$Data-273.15
        attr(data_cordex$tas$Variable,"units") <- "degC"
        data_cordex$pr$Data <- data_cordex$pr$Data*86400
        attr(data_cordex$pr$Variable,"units") <- "mm d**-1"
        cordex_names <- names(data_cordex)
        data_cordex <- data_cordex[match(names(ERA5), names(data_cordex))]
        
        if(my_rcps[p]=="historical"){
             data_hist <- data_cordex
            data_bc <- lapply(1:length(data_cordex), function(v)  {
            pre <- FALSE
            if (names(data_cordex)[v] == "pr") pre <- TRUE
            biasCorrection(y = ERA5[[v]],
                           x = data_cordex[[v]],
                           method = "eqm",
                           precipitation = pre,
                           wet.threshold = 1,
                           join.members = TRUE)})
        }else{
          
          if(!2100%in%unique(year(data_cordex$uas$Dates$start))){
            add <- NULL
            for(i in 1:length(names(data_cordex))){
              add[[i]] <- subsetGrid(data_cordex[[i]], years = c(2099))
              add[[i]]$Dates$start <-paste0(as.character( ymd_hms(add[[i]]$Dates$start) %m+% years(1)), " GMT")
              add[[i]]$Dates$end <-paste0(as.character(ymd_hms(add[[i]]$Dates$end, tz = "GMT") %m+% years(1)), " GMT")
              data2add <- rep(NA,length(add[[i]]$Data)) 
              attributes(data2add) <- attributes(add[[i]]$Data)
              add[[i]]$Data <- data2add
            }
            names(add) <- names(data_cordex)
            addgrid <- NULL
            for(j in 1:length(names(data_cordex))){
              addgrid[[j]] <- bindGrid(data_cordex[[j]], add[[j]], dimension = "time")
            }
            names(addgrid) <- names(data_cordex)
            data_cordex <- addgrid
          }
          
          # data_hist <- get(load(files_rcp[1]))
          # if("cc"%in% names(data_hist )){
          #   names(data_hist )[which(names(data_hist )=="cc")] <- "clt"
          # }else{
          #   data_hist $clt$Data <- data_hist$clt$Data/100
          #   attr(data_hist$clt$Variable,"units") <- "1" 
          # }
          # if(my_domains[d]== "CAS-22"){
          #   data_cordex$vas$Data <- data_cordex$vas$Data+runif(n = length(data_cordex$vas$Data), min = 1, max = 10)
          # }
          # data_hist$tas$Data <- data_hist$tas$Data-273.15
          # attr(data_hist$tas$Variable,"units") <- "degC"
          # data_hist$pr$Data <- data_hist$pr$Data*86400
          # attr(data_hist$pr$Variable,"units") <- "mm d**-1"
          # data_hist <- data_hist[match(names(ERA5), names(data_hist))]

          data_bc <- lapply(1:length(data_cordex), function(v)  {
            pre <- FALSE
            if (names(data_cordex)[v] == "pr") pre <- TRUE
            biasCorrection(y = ERA5[[v]],
                           x =data_hist[[v]],
                           newdata = data_cordex[[v]],
                           method = "eqm",
                           precipitation = pre,
                           wet.threshold = 1,
                           join.members = TRUE)})
        }
        names(data_bc) <- names(data_cordex)
        
        if(my_domains[d] == "CAS-22"){
          data_bc$vas$Data <- data_bc$vas$Data- data_bc$vas$Data
        }
        
        
        list_name <- paste(c(unlist(strsplit(files_rcp[l],split='_', fixed=TRUE))[1:4],"BiasCorrcted_all_ERA5.rda"), collapse ="_")
        save(data_bc, file= paste(dir.Rdata, list_name, sep = "/") )
        t=t+1
        print(paste(t, files_rcp[l], sep="  "))
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(time.taken)
      }
    }
  }
}
