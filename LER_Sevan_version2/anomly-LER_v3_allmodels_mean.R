library(lubridate)
library(reshape2)
library(plyr)
library(ggplot2)
setwd("/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/")

output_data <- get(load("cordex_LER_results_4models.rda"))
my_lake_data <- get(load("/home/shikhani/Documents/LER_Sevan_trial2/Output_LER/cordex_LER_results_mylake.rda"))

output_data$Surface_Temperature$MyLake <- my_lake_data$Surface_Temperature

for (x in 1:length(output_data)) {
  output_data[[x]]$MyLake <- my_lake_data[[x]]$MyLake
}

for(i in 1:length(output_data)){
  output_data[[i]]$Ensemble_Mean <- output_data[[i]]$FLake
  for(j in 1:length(output_data[[i]][[1]])){
    for(k in 2:ncol(output_data[[i]][[1]][[1]])){
      output_data[[i]]$Ensemble_Mean[[j]][,k] <- rowMeans(data.frame(output_data[[i]][[1]][[j]][,k],output_data[[i]][[2]][[j]][,k],output_data[[i]][[3]][[j]][,k],output_data[[i]][[4]][[j]][,k]), na.rm = T)
    }
  }
}



output_annual <- output_data

for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    for(p in 1:length(output_data[[1]][[1]])){
      output_annual[[v]][[m]][[p]] <- aggregate( output_annual[[v]][[m]][[p]][,-1], by=list(year( output_annual[[v]][[m]][[p]][,1])),na.rm=T, mean)
      names( output_annual[[v]][[m]][[p]])[1] <- "date"
    }
  }
}


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

pdf("results_LER.pdf")


# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# gg_color_hue(4)




output_plot <- output_melted
for (v in 1:length(output_data)) {
  for(m in 1:length(output_data[[1]])){
    df <- ddply(output_plot[[v]][[m]], c('date', 'Scenario'), function(x){
      df2 <- data.frame(mean = mean(x$value, na.rm=T),
                        sd = sd(x$value, na.rm = T))
    })
    output_plot[[v]][[m]] <- df
    my_title <- paste(names(output_plot[[v]])[m],paste(strsplit(names(output_data),split='_', fixed=TRUE)[[v]][1],strsplit(names(output_data),split='_', fixed=TRUE)[[1]][2], sep=" "), sep=" ")
    
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

dev.off()


output_facet <- output_plot
for (v in 1:length(output_data)) {
  for (m in 1:length(output_data[[1]])) {
    output_facet[[v]][[m]]$model <- names(output_facet[[v]])[m]
  }
  output_facet[[v]]<- dplyr::bind_rows(output_facet[[v]])
  
}



for (v in 1:length(output_data)) {
    my_title <-paste(strsplit(names(output_data),split='_', fixed=TRUE)[[v]][1],strsplit(names(output_data),split='_', fixed=TRUE)[[1]][2], sep=" ")
    
    output_facet[[v]]$model=factor(output_facet[[v]]$model,
                                   levels=c("FLake","GLM","GOTM","Simstrat","MyLake","Ensemble_Mean"))
    
    p2 <- ggplot(output_facet[[v]], aes(date, mean))+
      geom_hline(yintercept = 0)+
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Scenario), alpha = 0.3)+
      geom_line(aes(colour = Scenario))+
      facet_wrap(.~model, ncol=2)+
      ggtitle(my_title)+
      scale_color_manual(values=c("#F8766D" ,"#00BFC4" ,"#C77CFF"))+
      scale_fill_manual(values=c("#F8766D","#00BFC4" ,"#C77CFF"))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_classic(base_size = 26)+
      theme(panel.background = element_rect(colour = 'black'))+ylab("anomaly °C")
    
    print(p2)
  }
