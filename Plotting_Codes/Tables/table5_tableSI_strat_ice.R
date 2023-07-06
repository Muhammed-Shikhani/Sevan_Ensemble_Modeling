library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(LakeEnsemblR)
library(RColorBrewer)
library(xtable)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/")




output_data <- get(load("CORDEX_LER_daily_named.rda"))[c(1,2,4)]
output_annual <- get(load("cordex_LER_annual_named.rda"))[1]

for (v in 1:length(output_data)) {
  for (m in 1:length(output_data[[1]])) {
    for(p in 1:length(output_data[[1]][[1]])){
      output_data[[v]][[m]][[p]][1,-1] <-output_data[[v]][[2]][[p]][2,-1]
      output_data[[v]][[m]][[p]][nrow(output_data[[v]][[m]][[p]]),-1] <-output_data[[v]][[m]][[p]][(nrow(output_data[[v]][[m]][[p]])-1),-1]
      
    }
  }
}





StratVarsVec <- c(2,4,6,8,10,13,14,18,20,21,24)
output_strat <- rep(list(output_annual$Surface_Temperature),length(StratVarsVec))

for(m in 1:length(output_data[[1]])){
  for(p in 1:length(output_data[[1]][[m]])){
    for (x in 2:ncol(output_data[[1]][[m]][[p]]) ) {
      
      strat_analysis <-  analyse_strat( Ts=output_data[[1]][[m]][[p]][,x], Tb = output_data[[2]][[m]][[p]][,x], dates = output_data[[1]][[m]][[p]][,1], drho = 0.1,H_ice = output_data[[3]][[m]][[p]][,x])[,StratVarsVec]
      
      for(y in 1:length(StratVarsVec)){
        output_strat[[y]][[m]][[p]][,x] <- strat_analysis[,y]
      }
    }
  }
}


for (v in 1:length(output_strat)) {
  for (m in 1:length(output_strat[[v]])) {
    for(p in 1:length(output_strat[[1]][[1]])){
      output_strat[[v]][[m]][[p]][nrow(output_strat[[v]][[m]][[p]]),-1] <-output_strat[[v]][[m]][[p]][(nrow(output_strat[[v]][[m]][[p]])-1),-1]
      
    }
  }
}

names(output_strat) <- names(strat_analysis)
save(output_strat, file="Strat_ice_Sevan_CORDEX_LER_01.rda")
####################################

output_strat <- get(load("Strat_ice_Sevan_CORDEX_LER_01.rda"))
my_units <- c("°C","°C","°C","°C","day","doy","doy","day","doy","doy","m")




ice_list <- output_strat[8:11]
my_units <- c("day","doy","doy","m")





for(m in 1:length(ice_list [[1]])){
  for(p in 1:length(ice_list [[1]][[m]])){
    
    ice_list[[1]][[m]][[p]][is.na(ice_list[[1]][[m]][[p]])] <- 0
  }
}




ice_melted <- setNames(reshape2::melt(ice_list , id.vars = c("date")),
                       c("date", "gc_rc", "value", "period", "model", "variable"))

ice_ag <- ice_melted %>% group_by(date, period, variable,model) %>%
  summarise(mean = mean(value,na.rm=TRUE),
            median = median(value,na.rm=TRUE),
            sd = sd(value,na.rm=TRUE),
            q25 = quantile(value, 0.25,na.rm=TRUE),
            q75 = quantile(value, 0.75,na.rm=TRUE),
            q05 = quantile(value, 0.05,na.rm=TRUE),
            q95 = quantile(value, 0.95,na.rm=TRUE))



historical_period <- "1995-2005"
future_period <- "2089-2099"
desired_models <- c("rcp26", "rcp45", "rcp85")

filtered_table <- ice_ag  %>%
  filter(date %in% c(1995:2005) | date %in% c(2089:2099) )



# Calculate average quantities per model, variable, and period
averages <- filtered_table %>%
  group_by(model, variable, period) %>%
  summarise(mean = mean(mean),
            median = mean(median),
            sd = mean(sd),
            q25 = mean(q25),
            q75 = mean(q75),
            q05 = mean(q05),
            q95 = mean(q95))

# Print the resulting averages
print(averages)

mean_ice <- averages %>% filter(variable %in% c("HiceMax","MaxIceDur"))
greater_than_10cm <- ice_ag %>%
  filter(variable == "HiceMax", period == "rcp85", mean > 0.1)

greater_than_10cm






strat_list <- output_strat[5:7]
my_units <- c("day","doy","doy")


strat_melted <- setNames(reshape2::melt(strat_list , id.vars = c("date")),
                         c("date", "gc_rc", "value", "period", "model", "variable"))


mean_hist <- strat_melted %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))

strat_anomaly <- strat_melted %>% 
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)

#strat_anomaly <- strat_anomaly[-which(is.na(strat_anomaly$anomaly)),]
strat_ag <- strat_anomaly %>% 
  group_by(date, period, variable, model)  %>%
  summarise(mean = mean(anomaly, na.rm = TRUE),
            median = median(anomaly, na.rm = TRUE),
            sd = sd(anomaly, na.rm = TRUE),
            q25 = quantile(anomaly, 0.25, na.rm = TRUE),
            q75 = quantile(anomaly, 0.75, na.rm = TRUE),
            q05 = quantile(anomaly, 0.05, na.rm = TRUE),
            q95 = quantile(anomaly, 0.95, na.rm = TRUE))



filtered_table <- strat_ag  %>%
  filter(date %in% c(1995:2005) | date %in% c(2089:2099) )



# Calculate average quantities per model, variable, and period
averages <- filtered_table %>%
  group_by(model, variable, period) %>%
  summarise(mean = mean(mean),
            median = mean(median),
            sd = mean(sd),
            q25 = mean(q25),
            q75 = mean(q75),
            q05 = mean(q05),
            q95 = mean(q95))

# Print the resulting averages
print(averages)

mean_strat<- averages %>% filter(variable %in% c("MaxStratDur","StratEnd","StratStart"))
##################
table_si <- rbind(mean_strat %>%arrange(period), mean_ice%>%arrange(period))

for(i in c(4:10)){
  table_si[,i] <- signif(table_si[,i], digits =2)
  
}

# Define a function to add correct units
add_units <- function(variable) {
  if (variable == "MaxStratDur") {
    return("Max Stratification Duration (day)")
  } else if (variable == "StratEnd") {
    return("Stratification End (day)")
  } else if (variable == "StratStart") {
    return("Stratification Start (day)")
  } else if (variable == "HiceMax") {
    return("Max Ice Height (m)")
  } else if (variable == "MaxIceDur") {
    return("Max Ice Duration (day)")
  } else {
    return(variable)
  }
}

# Add correct units to variables
table_si$variable <- sapply(table_si$variable, add_units)

correct_rcp <- function(rcp) {
  if (grepl("historical", rcp)) {
    return("Historical")
  } else if (grepl("rcp26", rcp)) {
    return("RCP 2.6")
  } else if (grepl("rcp45", rcp)) {
    return("RCP 4.5")
  } else if (grepl("rcp85", rcp)) {
    return("RCP 8.5")
  } else {
    return(rcp)
  }
}

# Correct RCP names and add units to variables
table_si$period <- sapply(table_si$period, correct_rcp)


table_si 

table_si_tex<- xtable(table_si)


# Print the Overleaf table code
print(xtable(table_si_tex, type = "latex", include.rownames = FALSE))




my_table <- table_si %>%filter(model =="Ensemble_Mean")
my_table[,-1]

print(xtable(my_table[,-1] , type = "latex", include.rownames = FALSE))
