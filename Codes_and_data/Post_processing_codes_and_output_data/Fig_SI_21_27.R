library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(LakeEnsemblR)
library(RColorBrewer)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/SI_FIgures/")




output_data <- get(load("/home/shikhani/Documents/Sevan_plotting_draft/CORDEX_LER_daily_named.rda"))[c(1,2,4)]
output_annual <- get(load("/home/shikhani/Documents/Sevan_plotting_draft/cordex_LER_annual_named.rda"))[1]

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

output_strat <- get(load("/home/shikhani/Documents/Sevan_plotting_draft/Strat_ice_Sevan_CORDEX_LER_01.rda"))
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
  summarise(mean = mean(value),
            median = median(value),
            sd = sd(value),
            q25 = quantile(value, 0.25,na.rm=TRUE),
            q75 = quantile(value, 0.75,na.rm=TRUE),
            q05 = quantile(value, 0.05,na.rm=TRUE),
            q95 = quantile(value, 0.95,na.rm=TRUE))





ice_melted %>% filter(model  == "Ensemble_Mean" & variable == "MaxIceDur") %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = ice_ag %>%  filter(model  == "Ensemble_Mean" & variable == "MaxIceDur"),
                       aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_colour_manual(values = c("historical"= "gray24", "rcp26"  = "steelblue1", "rcp45"  = "springgreen4", "rcp85"  = "firebrick2"))


# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)



ice_vars <- unique(ice_melted$variable)
my_labs <- unique(ice_melted$variable)

for (v in 1:length(ice_vars)) {
  
  
  p22 <-  ice_melted %>% filter(variable == ice_vars[v] ) %>% ggplot() +
    geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
    geom_point(data = ice_ag %>%  filter(variable == ice_vars[v]),
               aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
    scale_colour_manual(values = c("historical"= "gray24", "rcp26"  = "steelblue1", "rcp45"  = "springgreen4", "rcp85"  = "firebrick2")) +
    theme_classic(base_size = 26)+
    theme(panel.background = element_rect(colour = 'black')) +
    facet_wrap(.~model, ncol=3)+
 #   guides(colour = guide_legend(override.aes = list(size = 8), title="Scenario"))+
    #   xlab("Date")+ylab(as.factor(paste(titles[v],my_units[v],sep="~")))
    labs(y = my_labs[v], x="Date")
  
  ggsave(paste0(ice_vars[v],'test01.png'), p22,  dpi = 300,width = 600,height = 400, units = 'mm')
}






p55 <- ice_melted %>% filter(model  == "Ensemble_Mean" ) %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = ice_ag %>%  filter(model  == "Ensemble_Mean"),
             aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_colour_manual(values = c("historical"= "gray24", "rcp26"  = "steelblue1", "rcp45"  = "springgreen4", "rcp85"  = "firebrick2"))+ 
  facet_wrap(.~variable, ncol=1, scales = "free_y",strip.position = "left" 
             ,labeller = as_labeller(c( HiceMax  = "Max Ice Height (m)", MaxIceDur = "Max Ice Duration (day)", IceOn = "Ice Onset (doy)",IceOff="Ice End (doy)" ) )) +
  theme_classic(base_size = 26) +
  theme(panel.background = element_rect(colour = 'black'),
        strip.text = element_text(size = 20)) +
  labs(y = NULL, x = "Date")



ggsave('Sevan_ice_mean01.png', p55,  dpi = 300,width = 300,height = 400, units = 'mm')
write.csv(ice_ag,"ice_ag_01.csv",quote = F, row.names = F)
write.csv(ice_melted,"ice_Sevan_01.csv",quote = F, row.names = F)
#####################

######################





strat_list <- output_strat[5:7]
strat_list$Schmidt_Stability <- get(load("/home/shikhani/Documents/Sevan_plotting_draft/cordex_LER_annual_named.rda"))[5][[1]]
my_units <- c("day","doy","doy","J/m2")


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


  
  strat_ag %>% filter(model  == "Ensemble_Mean" & variable == "MaxStratDur" ) %>% ggplot() +
  geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
              alpha = 0.4) +
  geom_line(aes(x = date, y = median, col = period)) +
  theme_classic(base_size = 26)+
  theme(panel.background = element_rect(colour = 'black')) +
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  scale_fill_manual("Period", values = mycol2, guide = "none")+
  facet_wrap(.~model, ncol=3)+
  guides(colour = guide_legend(override.aes = list(size = 8), title="Scenario"))+
  labs(y = my_labs[v], x="Date")


strat_melted %>% filter(model  == "Ensemble_Mean" & variable == "MaxStratDur" ) %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = strat_ag %>%  filter(model  == "Ensemble_Mean" & variable == "MaxStratDur" ),
             aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_colour_manual(values = c("historical"= "gray24", "rcp26"  = "steelblue1", "rcp45"  = "springgreen4", "rcp85"  = "firebrick2"))


# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)



strat_vars <- unique(strat_melted$variable)
my_labs <- unique(strat_melted$variable)

for (v in 1:length(strat_vars)) {
  
  
  p22 <-  strat_ag %>% filter(variable == strat_vars[v]  ) %>% ggplot() +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
                alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
                alpha = 0.4) +
    geom_line(aes(x = date, y = median, col = period)) +
    theme_classic(base_size = 26)+
    theme(panel.background = element_rect(colour = 'black')) +
    scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
    scale_fill_manual("Period", values = mycol2, guide = "none")+
    facet_wrap(.~model, ncol=3)+
    guides(colour = guide_legend(override.aes = list(size = 8), title="Scenario"))+
    #   guides(colour = guide_legend(override.aes = list(size = 8), title="Scenario"))+
    #   xlab("Date")+ylab(as.factor(paste(titles[v],my_units[v],sep="~")))
    labs(y = my_labs[v], x="Date")
  
  ggsave(paste0(strat_vars[v],'test01.png'), p22,  dpi = 300,width = 600,height = 400, units = 'mm')
}


strat_ag$variable <- factor(strat_ag$variable, levels=c("StratStart","StratEnd" ,"MaxStratDur" ,"Schmidt_Stability"  ))

p55 <- strat_ag %>% filter(model == "Ensemble_Mean") %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
              alpha = 0.4) +
  geom_line(aes(x = date, y = median, col = period)) +
  theme_classic(base_size = 26) +
  theme(panel.background = element_rect(colour = 'black'),
        strip.text = element_text(size = 16)) +  # Decreased size from 20 to 16
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  scale_fill_manual("Period", values = mycol2, guide = "none")+ 
  facet_wrap(. ~ variable, ncol = 1, scales = "free_y", strip.position = "left", 
             labeller = as_labeller(c(MaxStratDur = "Max Stratification Duration (day)", StratStart = "Stratification Onset (day)", StratEnd = "Stratification End (day)", Schmidt_Stability = "Schmidt Stability J/m2"))) +
  labs(y = NULL, x = "Date")


ggsave('Sevan_strat_mean01.png', p55, dpi = 300, width = 300, height = 450, units = 'mm')

write.csv(strat_ag,"stratification_sevan_stats_01.csv",quote = F, row.names = F)
