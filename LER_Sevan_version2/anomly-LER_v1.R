library(lubridate)
library(reshape2)
library(plyr)
library(ggplot2)
setwd( "/home/shikhani/Documents/LER_Sevan_version2/")

output_data <- get(load("cordex_results_trial4.rda"))

output_annual <- output_data

for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    for(p in 1:length(output_data[[1]][[1]])){
      output_annual[[v]][[m]][[p]] <- aggregate( output_annual[[v]][[m]][[p]][,-1], by=list(year( output_annual[[v]][[m]][[p]][,1])),na.rm=T, mean)
      names( output_annual[[v]][[m]][[p]])[1] <- "date"
    }
  }
}


save(output_annual,file= "cordex_results_annual_trial2.rda")

output_refernce <- output_annual


for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    for(p in 1:length(output_data[[1]][[1]])){
      output_refernce[[v]][[m]] <-  colMeans( output_annual[[v]][[m]][[1]][,-1])
    }
  }
}

output_anomaly <- output_annual
for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    for(p in 1:length(output_data[[1]][[1]])){
      output_anomaly[[v]][[m]][[p]][,-1] <-  output_anomaly[[v]][[m]][[p]][,-1]-output_refernce[[v]][[m]]
      
    }
  }
}

save(output_anomaly,file= "cordex_results_anomaly_trial2.rda")



# For each date and each scenario calculate the mean and sd of the anomaly

output_melted <- output_anomaly

for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    model_df <- data.frame()
    for(p in 1:length(output_data[[1]][[1]])){
      output_melted[[v]][[m]][[p]] <- melt( output_melted[[v]][[m]][[p]],id="date")
      output_melted[[v]][[m]][[p]]$Scenario <- names(output_melted[[v]][[m]])[p]
      model_df <- rbind(model_df, output_melted[[v]][[m]][[p]])
    }
    output_melted[[v]][[m]] <- model_df 
    
  }
}

output_plot <- output_melted
for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    df <- ddply(output_plot[[v]][[m]], c('date', 'Scenario'), function(x){
      df2 <- data.frame(mean = mean(x$value, na.rm=T),
                        sd = sd(x$value, na.rm = T))
    })
    output_plot[[v]][[m]] <- df
  }
}

output_plot <- output_melted
for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    df <- ddply(output_plot[[v]][[m]], c('date', 'Scenario'), function(x){
      df2 <- data.frame(mean = mean(x$value, na.rm=T),
                        sd = sd(x$value, na.rm = T))
    })
    output_plot[[v]][[m]] <- df
    my_title <- paste(strsplit(names(output_data),split='_', fixed=TRUE)[[v]][1],strsplit(names(output_data),split='_', fixed=TRUE)[[1]][2], sep=" ")
   
     p1 <- ggplot(output_plot[[v]][[m]], aes(date, mean))+
      geom_hline(yintercept = 0)+
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Scenario), alpha = 0.3)+
      geom_line(aes(colour = Scenario))+
      ggtitle(my_title)+
       scale_color_manual(values=c("#F8766D" ,"#00BFC4" ,"#C77CFF"))+
      scale_fill_manual(values=c("#F8766D","#00BFC4" ,"#C77CFF"))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_classic(base_size = 26)+
      theme(panel.background = element_rect(colour = 'black'))+
      ylab("anomaly °C")
   
      print(p1)
  }
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue(4)
