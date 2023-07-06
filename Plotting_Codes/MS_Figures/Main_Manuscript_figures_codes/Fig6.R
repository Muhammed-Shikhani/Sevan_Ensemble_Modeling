library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(LakeEnsemblR)
library(RColorBrewer)
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
  summarise(mean = mean(value),
            median = median(value),
            sd = sd(value),
            q25 = quantile(value, 0.25,na.rm=TRUE),
            q75 = quantile(value, 0.75,na.rm=TRUE),
            q05 = quantile(value, 0.05,na.rm=TRUE),
            q95 = quantile(value, 0.95,na.rm=TRUE))



write.csv(ice_ag,"ice_ag_01.csv",quote = F, row.names = F)
write.csv(ice_melted,"ice_Sevan_01.csv",quote = F, row.names = F)
#####################

######################





strat_list <- output_strat[5:7]
strat_list$Schmidt_Stability <- get(load("cordex_LER_annual_named.rda"))[5][[1]]
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


write.csv(strat_ag,"stratification_sevan_stats_01.csv",quote = F, row.names = F)
#################
###################
#plotting


strat_ag <- read.csv("stratification_sevan_stats_01.csv")
ice <- read.csv("ice_Sevan_01.csv")
ice_ag <- read.csv("ice_ag_01.csv")

strat_ag$variable <- factor(strat_ag$variable, levels=c("StratStart","StratEnd" ,"MaxStratDur" ,"Schmidt_Stability"  ))


# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)



p1 <- strat_ag %>% filter(model == "Ensemble_Mean"& variable %in% c("StratStart")) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
              alpha = 0.4) +
  geom_line(aes(x = date, y = median, col = period)) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(colour = 'black'),
        axis.text.x = element_blank()) +
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  scale_fill_manual("Period", values = mycol2, guide = "none")+ 
  facet_wrap(. ~ variable, ncol = 2, scales = "free_y", strip.position = "left", 
             labeller = as_labeller(c( StratStart = "Stratification Onset Anomaly (doy)"))) +
  labs(y = NULL, x = NULL)+
  annotate("text", x=2098, y=20, label= "(A)",size = 5)





p2 <- strat_ag %>% filter(model == "Ensemble_Mean"& variable %in% c("StratEnd")) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
              alpha = 0.4) +
  geom_line(aes(x = date, y = median, col = period)) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(colour = 'black'),
        axis.text.x = element_blank()) +
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  scale_fill_manual("Period", values = mycol2, guide = "none")+ 
  facet_wrap(. ~ variable, ncol = 2, scales = "free_y", strip.position = "left", 
             labeller = as_labeller(c( StratEnd = "Stratification End Anomaly (doy)"))) +
  labs(y = NULL, x = NULL)+
  annotate("text", x=2098, y=40, label= "(B)",size = 5)




p3 <- ice %>% filter(model  == "Ensemble_Mean" & variable %in% c("MaxIceDur")) %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = ice_ag %>%  filter(model  == "Ensemble_Mean"& variable %in% c("MaxIceDur")),
             aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  facet_wrap(.~variable, ncol=1, scales = "free_y",strip.position = "left" 
             ,labeller = as_labeller(c( MaxIceDur = "Max Ice Duration (day)") )) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(colour = 'black')) +
  labs(y = NULL, x = NULL)+
  annotate("text", x=2098, y=155, label= "(C)",size = 5)



p4 <- ice %>% filter(model  == "Ensemble_Mean" & variable %in% c("HiceMax")) %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = ice_ag %>%  filter(model  == "Ensemble_Mean"& variable %in% c("HiceMax")),
             aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_color_manual("Period", values = mycol, guide = "none",labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  facet_wrap(.~variable, ncol=1, scales = "free_y",strip.position = "left" 
             ,labeller = as_labeller(c( HiceMax= "Max Ice thickness (m)") )) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(colour = 'black')) +
  labs(y = NULL, x = NULL)+
  annotate("text", x=2098, y=0.42, label= "(D)",size = 5)+ ylim(0,0.45)





pp <- p1+p2+p3+p4+ plot_layout(ncol=2)
pp
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



p_legend <- ice %>% filter(model  == "Ensemble_Mean" & variable %in% c("HiceMax")) %>%
  ggplot() +
  geom_point(aes(x = date, y =  value, group = interaction(gc_rc, period), col = period), alpha = 0.25)+
  geom_point(data = ice_ag %>%  filter(model  == "Ensemble_Mean"& variable %in% c("HiceMax")),
             aes(x = date, y = median, group = period, col = period),size = 1.5, stroke =1.5)+
  scale_color_manual("Scenario", values = mycol,labels=c("Historical", "RCP2.6", "RCP4.5", "RCP8.5")) +
  facet_wrap(.~variable, ncol=1, scales = "free_y",strip.position = "left" 
             ,labeller = as_labeller(c( HiceMax= "Max Ice thickness (m)") )) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(colour = 'black')) +
  labs(y = NULL, x = NULL)+
  annotate("text", x=2098, y=0.42, label= "(D)",size = 5)+ ylim(0,0.45)



mylegend <-g_legend(p_legend)


final_plot <- plot_grid(pp, mylegend, ncol = 2, rel_widths = c(0.85, 0.15))

final_plot <- final_plot + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

final_plot

ggsave('Fig8_strat_ice_01tt.png', final_plot,  dpi = 300,width = 300,height = 225, units = 'mm')

