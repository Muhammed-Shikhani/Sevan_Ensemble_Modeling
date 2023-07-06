library(lubridate)
library(reshape2)
library(ggplot2)
library(ggsci)          
library(patchwork)
library(dplyr)

setwd("/home/shikhani/Documents/Sevan_plotting_draft/")

output_annual <- get(load("cordex_LER_annual_named.rda"))[1:2]


## my go at preparing the data using tidyr an plotting

# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))


# calculate the mean of the historic period for each model, run, and variable
mean_hist <- dat_l %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))



dat_anomaly_withmean <- dat_l  %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)



# calculate mean sd and quantiles for each variable and period

dat_an_ag_mean <- dat_anomaly_withmean %>% group_by(date, period, variable,model) %>%
  summarise(mean = mean(anomaly),
            median = median(anomaly),
            sd = sd(anomaly),
            q25 = quantile(anomaly, 0.25,na.rm=TRUE),
            q75 = quantile(anomaly, 0.75,na.rm=TRUE),
            q05 = quantile(anomaly, 0.05,na.rm=TRUE),
            q95 = quantile(anomaly, 0.95,na.rm=TRUE))


dat_an_ag_mean$model <- factor(dat_an_ag_mean$model, levels = c("Ensemble_Mean", "FLake", "GLM", "GOTM", "MyLake", "Simstrat"))



dat_last10 <- dat_anomaly_withmean  %>% filter(date >= 2090)


dat_last10_mean<- dat_last10 %>% group_by(period, variable,model) %>%
  summarise(mean = mean(anomaly))


dat_last10_hist <- dat_anomaly_withmean  %>% filter(date >= 1995 & date < 2006)


dat_last10_mean_hist<- dat_last10_hist %>% group_by(period, variable,model) %>%
  summarise(mean = mean(anomaly))


dat_last10_mean  %>% filter(variable == "Surface_Temperature" &
                              period %in% c("rcp85"))


dat_discuss <- rbind(dat_last10_mean_hist, dat_last10_mean)
as.data.frame(dat_discuss)

p1<- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Surface_Temperature" &
                                       period %in% c("historical", "rcp85")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Surface_Temperature" &
                                                period %in% c("historical", "rcp85")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none", axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(x = NULL, y =NULL)+ggtitle("Surface Temperature ")+
  annotate("text", x=1980, y=8, label= "(A)  RCP 8.5",size = 5)




p2<- dat_last10  %>% filter(variable == "Surface_Temperature" &
                              period %in% c("rcp85")) %>% ggplot()+ 
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Surface_Temperature" &
                                                          period %in% c("rcp85"))
                       ,aes(x=mean+2.5, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)
#p1+p2+plot_layout(widths = c(3.5, 1))



p3<- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Surface_Temperature" &
                                       period %in% c("historical", "rcp45")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Surface_Temperature" &
                                                period %in% c("historical", "rcp45")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none", axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  labs(x = NULL, y =NULL)+
  annotate("text", x=1980, y=8, label= "(B)  RCP 4.5",size = 5)




p4<- dat_last10  %>% filter(variable == "Surface_Temperature" &
                              period %in% c("rcp45")) %>% ggplot() +
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Surface_Temperature" &
                                                          period %in% c("rcp45"))
                       ,aes(x=mean+2, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)







p5<- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Surface_Temperature" &
                                       period %in% c("historical", "rcp26")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Surface_Temperature" &
                                                period %in% c("historical", "rcp26")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none")+
  labs(x = NULL, y =NULL)+
  annotate("text", x=1980, y=8, label= "(C)  RCP 2.6",size = 5)




p6<- dat_last10  %>% filter(variable == "Surface_Temperature" &
                              period %in% c("rcp26")) %>% ggplot() +
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Surface_Temperature" &
                                                          period %in% c("rcp26"))
                       ,aes(x=mean+2, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)



# dat_anomaly_withmean %>% group_by(date,period, variable,model) %>%
#   summarise(mean = mean(anomaly))%>% filter(variable == "Surface_Temperature" &
#                                               period %in% c("rcp85")) %>% filter(date == 2099)
# dat_anomaly_withmean %>% group_by(date,period, variable,model) %>%
#   summarise(mean = mean(anomaly))%>% filter(variable == "Surface_Temperature" &
#                                               period %in% c("rcp85")) %>% filter(date == 2050)


################






p7 <- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Bottom_Temperature" &
                                        period %in% c("historical", "rcp85")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Bottom_Temperature" &
                                                period %in% c("historical", "rcp85")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none", axis.ticks.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())+
  labs(x = NULL, y =NULL)+ggtitle("Bottom Temperature ")+
  annotate("text", x=1980, y=8, label= "(D)  RCP 8.5",size = 5)




p8 <- dat_last10  %>% filter(variable == "Bottom_Temperature" &
                               period %in% c("rcp85")) %>% ggplot() +
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Bottom_Temperature" &
                                                          period %in% c("rcp85"))
                       ,aes(x=mean+3, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)




p9 <- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Bottom_Temperature" &
                                        period %in% c("historical", "rcp45")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Bottom_Temperature" &
                                                period %in% c("historical", "rcp45")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none", axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())+
  labs(x = NULL, y =NULL)+
  annotate("text", x=1980, y=8, label= "(E)  RCP 4.5",size = 5)




p10 <- dat_last10  %>% filter(variable == "Bottom_Temperature" &
                                period %in% c("rcp45")) %>% ggplot() +
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Bottom_Temperature" &
                                                          period %in% c("rcp45"))
                       ,aes(x=mean+2, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)







p11 <- dat_anomaly_withmean %>% filter(model  != "Ensemble_Mean" & variable == "Bottom_Temperature" &
                                         period %in% c("historical", "rcp26")) %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = date, y =  anomaly,  group= interaction(gc_rc,model,period), colour=model), alpha=0.2)+
  geom_line( data = dat_an_ag_mean %>% filter(variable == "Bottom_Temperature" &
                                                period %in% c("historical", "rcp26")),
             aes(x=date, y=mean, group= interaction(model,period), colour=model), size = 1.2)+
  scale_color_jama()+
  scale_y_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0),legend.position="none",axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())+
  labs(x = NULL, y =NULL)+
  annotate("text", x=1980, y=8, label= "(F)  RCP 2.6",size = 5)




p12 <- dat_last10  %>% filter(variable == "Bottom_Temperature" &
                                period %in% c("rcp26")) %>% ggplot() +
  geom_boxplot(aes(anomaly, colour=model,fill=model, group=model))+ 
  coord_flip()+ 
  scale_color_jama()+scale_fill_jama()+
  scale_x_continuous(limits = c(-2, 8),breaks=seq(-2,8,2), labels=seq(-2,8,2))+
  theme_bw()+geom_text(data=dat_last10_mean  %>% filter(variable == "Bottom_Temperature" &
                                                          period %in% c("rcp26"))
                       ,aes(x=mean+2, y=c(-0.37,-0.25,-0.10,0,0.25,0.37), label=model, colour=model), size=5, angle = 90)+
  guides( y = "none",fill="none", x = "none")+
  labs(x = NULL, y = NULL)+  theme_void()+theme(plot.margin = margin(0, 0, 0, 0),legend.position="none")+  geom_vline(xintercept = 0)


## my go at preparing the data using tidyr an plotting

# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))


# calculate the mean of the historic period for each model, run, and variable
mean_hist <- dat_l %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))



dat_anomaly_withmean <- dat_l  %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)



data_e <- dat_anomaly_withmean[which(dat_anomaly_withmean$period== "rcp85"),]
data_e <- data_e %>% filter(variable == "Surface_Temperature")


ensemble_means <- data_e %>%
  group_by(gc_rc, model) %>%
  summarise(ensemble_mean = mean(anomaly, na.rm = TRUE))




# p<- ggplot(ensemble_means, aes(x = gc_rc, y = ensemble_mean, color = model, shape = ifelse(model == "Ensemble_Mean", "triangle", "circle"), size = ifelse(model == "Ensemble_Mean", "triangle", "circle"))) +
#   geom_point() +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
#   xlab("GCM-RCM Combination") +
#   ylab("Mean Anomaly (K)") +
#   scale_color_jama() +
#   scale_size_manual(values = c(triangle = 3, circle = 2)) +
#   guides(shape = "none", size = "none")+annotate("text", x=3, y=3.75, label= "(M)",size = 5)+
#   scale_x_discrete(labels = function(x) stringr::str_wrap(gsub("_", " ", x), width = 2)) +
#   theme(legend.position ="bottom", legend.justification = c(0.5, 1), legend.box = "horizontal")
old_ensemble_means_gc_rc <- ensemble_means$gc_rc
ensemble_means$gc_rc<-unlist(lapply(ensemble_means$gc_rc, function(x) gsub("^(.*?_.*?)(_)", "\\1 \\2", x)))


p<- ggplot(ensemble_means, aes(x = gc_rc, y = ensemble_mean, color = model, shape = model, size=model)) +
  geom_point() +
  theme_bw() +
  xlab("GCM-RCM Combination") +
  ylab("Mean Anomaly (K)") +
  scale_color_jama() +
  scale_shape_manual(values=c("triangle", "circle", "circle", "circle", "circle", "circle"))+
  scale_size_manual(values = c(4, 2.5,2.5,2.5,2.5,2.5)) +
  guides( size = "none")+
  annotate("text", x=3, y=3.75, label= "(G)",size = 5)+
  scale_x_discrete(labels = function(x) stringr::str_wrap(gsub("_", "_", x), width = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(size = 12),  # Increase axis labels' size
    legend.text = element_text(size = 10),  # Increase legend text size
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.position = "top",
    legend.justification = c(0.5, 1),
    legend.box = "horizontal"
  )+ guides(
    color = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1,override.aes = list(size = 5)))

p


ppp<-p1+p2+p7+p8+p3+p4+p9+p10+p5+p6+p11+p12+plot_layout(widths = c(3.5, 1), nrow = 3, ncol=4)
ppp


p_lab <- 
  ggplot() + 
  annotate(geom = "text", x=1,y=1,label = "Anomaly (K)", angle = 90, size=8) +
  coord_cartesian(clip = "off")+
  theme_void()

my_plot <- p_lab +ppp+ plot_layout(widths=c(25,500))
p_all <- my_plot / p+plot_layout(heights = c(4, 1), nrow = 2, ncol=1)
p_all

ggsave('Fig6_ensemble_allmodels_boxplot_GCMRCM2.png', p_all,  dpi = 300,width =450,height = 400, units = 'mm')




dat_anomaly_withmean %>% group_by(date,period, variable,model) %>%
  summarise(mean = mean(anomaly))%>% filter(variable == "Bottom_Temperature" &
                                              period %in% c("rcp85")) %>% filter(date == 2099)
dat_anomaly_withmean %>% group_by(date,period, variable,model) %>%
  summarise(mean = mean(anomaly))%>% filter(variable == "Bottom_Temperature" &
                                              period %in% c("rcp85")) %>% filter(date == 2050)


