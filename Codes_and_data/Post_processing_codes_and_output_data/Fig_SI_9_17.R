library(lubridate)
library(reshape2)
library(dplyr)
library(patchwork)
library(ggplot2)
library(RColorBrewer)
library(ggsci)          

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

dat_anomaly_withmean$domain  <- unlist(strsplit(unlist(strsplit(as.character(dat_anomaly_withmean$gc_rc),split = "_"))[seq(1,nrow(dat_anomaly_withmean)*3,3)], "-"))[seq(1,nrow(dat_anomaly_withmean)*2,2)]
dat_anomaly_withmean$gcm  <- unlist(strsplit(as.character(dat_anomaly_withmean$gc_rc),split = "_"))[seq(2,nrow(dat_anomaly_withmean)*3,3)]
dat_anomaly_withmean$rcm  <- unlist(strsplit(as.character(dat_anomaly_withmean$gc_rc),split = "_"))[seq(3,nrow(dat_anomaly_withmean)*3,3)]

# calculate mean sd and quantiles for each variable and period

dat_an_ag_mean <- dat_anomaly_withmean %>% group_by(date, period, variable,model) %>%
  summarise(mean = mean(anomaly),
            median = median(anomaly),
            sd = sd(anomaly),
            q25 = quantile(anomaly, 0.25,na.rm=TRUE),
            q75 = quantile(anomaly, 0.75,na.rm=TRUE),
            q05 = quantile(anomaly, 0.05,na.rm=TRUE),
            q95 = quantile(anomaly, 0.95,na.rm=TRUE))


dat_text <- data.frame(
  label = c("(A)","","","","",""),
  rcm  = c("CLMcom-ETH-COSMO-crCLIM-v1-1","GERICS-REMO2015" ,  "SMHI-RCA4"                   
           , "CLMcom-ETH-COSMO-crCLIM-v1-1", "ICTP-RegCM4-7"  , "IITM-RegCM4-4" )
)

p1 <- dat_anomaly_withmean %>% filter( model  != "Ensemble_Mean"  & period == "rcp85"  ) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(y = anomaly, x = variable , fill = gcm, group = interaction(gcm, variable))) +
  facet_wrap(.~rcm, ncol=1)+
  scale_fill_rickandmorty() +
  theme_bw()+
  labs(x = NULL, y =NULL)+
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0), axis.ticks.x = element_blank(),axis.text.x = element_blank())+ 
  guides(fill=guide_legend(title="GCM")) + geom_text(
    data    = dat_text,
    mapping = aes(x = 0.5, y = 6, label = label)
  )




p3 <- dat_anomaly_withmean %>% filter( model  != "Ensemble_Mean"  & period == "rcp85"  ) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(y = anomaly, x = variable , fill = domain)) +
  scale_fill_rickandmorty() +
  scale_x_discrete(labels=c("Bottom_Temperature" = "Bottom Temperature", "Surface_Temperature"= "Surface Temperature"))+
  theme_bw()+
  labs(x = NULL, y =NULL)+
  annotate("text", x=0.5, y=8, label= "(B)",size = 5)+ 
  theme(text = element_text(size=20),plot.margin = margin(0, 0, 0, 0), axis.ticks.x = element_blank())+ 
  guides(fill=guide_legend(title="Domain"))


p_lab <- 
  ggplot() + 
  annotate(geom = "text", x=1,y=1,label = "Anomaly (K)", angle = 90, size=8)+ 
  coord_cartesian(clip = "off")+
  theme_void()
ppp<-p1+p3+plot_layout( nrow = 2, ncol=1, heights = c(80,20))
my_plot <- p_lab +ppp+ plot_layout(widths=c(25,200))
ggsave('Variability_climate_v2.png', my_plot,  dpi = 300,width =300,height = 300, units = 'mm')
