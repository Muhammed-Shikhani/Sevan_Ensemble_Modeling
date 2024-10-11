library(lubridate)
library(reshape2)
library(plyr)
library(ggplot2)
setwd("/home/shikhani/Documents/LER_Sevan_cordex/")

output_data <- get(load("cordex_LER_results_allmodels.rda"))


output_annual <- output_data

for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    for(p in 1:length(output_data[[1]][[1]])){
     
        if(v <4){
          output_annual[[v]][[m]][[p]] <- aggregate(  output_annual[[v]][[m]][[p]][,-1], by=list(year( output_annual[[v]][[m]][[p]][,1])),na.rm=T, mean)
        }else{
          output_annual[[v]][[m]][[p]] <- aggregate(   output_annual[[v]][[m]][[p]][,-1], by=list(year( output_annual[[v]][[m]][[p]][,1])),na.rm=T, max) 
        }

      names( output_annual[[v]][[m]][[p]])[1] <- "date"
    
    }
  }
}
save( output_annual, file="cordex_LER_resultss_anual.rda")
