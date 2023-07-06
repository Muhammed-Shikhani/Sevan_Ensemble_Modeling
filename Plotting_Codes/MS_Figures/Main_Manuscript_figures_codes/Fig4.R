library(lubridate)
library(reshape2)
library(dplyr)
library(patchwork)
library(ggplot2)
library(RColorBrewer)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/")

output_annual <- get(load("cordex_LER_annual_named.rda"))[1:2]


## my go at preparing the data using tidyr an plotting

# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))




air_df <- read.csv("airtemp_mdelted.csv")
names(air_df) <- names(dat_l)[1:4]
air_df$model <- "Climate_Model"
air_df$variable <- "Air_Temperature"
names(air_df)
names(air_df)[3] <- "anomaly"
head(air_df)



# calculate the mean of the historic period for each model, run, and variable
mean_hist <- dat_l %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))



dat_anomaly_withmean <- dat_l  %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)

dat_anomaly_withmean <- rbind(dat_anomaly_withmean, air_df)


# calculate mean sd and quantiles for each variable and period

dat_an_ag_mean <- dat_anomaly_withmean %>% group_by(date, period, variable,model) %>%
  summarise(mean = mean(anomaly),
            median = median(anomaly),
            sd = sd(anomaly),
            q25 = quantile(anomaly, 0.25,na.rm=TRUE),
            q75 = quantile(anomaly, 0.75,na.rm=TRUE),
            q05 = quantile(anomaly, 0.05,na.rm=TRUE),
            q95 = quantile(anomaly, 0.95,na.rm=TRUE))




# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)



# just the last 10 years of data
dat_last10 <- dat_anomaly_withmean  %>% filter(date >= 2090 | period == "historical")
dat_last10 <- dat_last10 %>% filter(date >= 1994)
df_label <- dat_last10 %>% filter(gc_rc == "MNA-44_ICHEC-EC-EARTH_SMHI-RCA4" & date%in%c(2004,2099)& model=="Ensemble_Mean"& variable== "Bottom_Temperature")

df_label$period<- factor(df_label$period,
                           levels = c("historical","rcp26" ,"rcp45" ,"rcp85" ),ordered = T)

dat_last10$period <- gsub("historical", "Historical", gsub("rcp26", "RCP 2.6", gsub("rcp45", "RCP 4.5", gsub("rcp85", "RCP 8.5", dat_last10$period))))
df_label$period <- gsub("historical", "Historical", gsub("rcp26", "RCP 2.6", gsub("rcp45", "RCP 4.5", gsub("rcp85", "RCP 8.5", df_label$period))))
df_label <- df_label[c(1,2,4,3),]
                        



d99<- dat_an_ag_mean %>% filter(date %in% c(2005,2099) & model %in% c("Ensemble_Mean","Climate_Model") )
###################

p1 <- dat_anomaly_withmean %>% filter(variable == "Air_Temperature") %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = date, y =  anomaly, group = interaction(gc_rc, period), col = period), alpha = 0.25) +
  geom_line(data = dat_an_ag_mean %>% filter( variable == "Air_Temperature"),
            aes(x = date, y = mean, group = period, col = period),size = 1.2)+
  scale_y_continuous(limits = c(-2, 7),breaks=seq(-2,6,2), labels=seq(-2,6,2)) +
  theme_minimal(base_size = 17) + theme(plot.margin = margin(0, 0, 0, 0),
                                        legend.position="none", axis.ticks.x = element_blank(),
                                        axis.text.x = element_blank()) +
  labs(x = NULL, y =NULL) +
  scale_color_manual("Period", values = mycol, guide = "none") +
  annotate("text", x=1990, y=6.5, label= "(A)  Air Temperature",size = 5)

p2 <- dat_last10 %>% filter(variable == "Air_Temperature") %>% ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(y = anomaly, col = period, fill = period)) +
  geom_text(data = df_label, aes(x = (c(-0.28,-0.09,0.28,0.09)-0.07),
                                 y = c(2, 3, 6.5, 4),
                                 label = period , angle = 90,
                                 color = period) , size = 5) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0), legend.position="none") +
  scale_y_continuous(limits = c(-2, 7)) +
  guides(x =  guide_axis(angle = 90), y = "none",fill="none")+
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = mycol, guide = "none") +
  scale_fill_manual("Period", values = mycol2, guide = "none") +
  annotate("text", x=-0.5, y=6.5, label= "(B)",size = 5)




p3 <- dat_anomaly_withmean %>% filter(model  == "Ensemble_Mean" & variable == "Surface_Temperature") %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = date, y =  anomaly, group = interaction(gc_rc, period), col = period), alpha = 0.25) +
  geom_line(data = dat_an_ag_mean %>% filter(model == "Ensemble_Mean" & variable == "Surface_Temperature"),
            aes(x = date, y = mean, group = period, col = period),size = 1.2)+
  scale_y_continuous(limits = c(-2, 7),breaks=seq(-2,6,2), labels=seq(-2,6,2)) +
  theme_minimal(base_size = 17) + theme(plot.margin = margin(0, 0, 0, 0),
                                        legend.position="none", axis.ticks.x = element_blank(),
                                        axis.text.x = element_blank()) +
  labs(x = NULL, y=NULL)  +
  scale_color_manual("Period", values = mycol, guide = "none")  +
  annotate("text", x=1992, y=6.5, label= "(C)  Surface Temperature",size = 5)

p4 <- dat_last10 %>% filter(variable == "Surface_Temperature") %>% ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(y = anomaly, col = period, fill = period)) +
  geom_text(data = df_label, aes(x = (c(-0.28,-0.09,0.28,0.09)-0.07),
                                 y = c(2, 3, 6, 4),
                                 label = period , angle = 90,
                                 color = period) , size = 5) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0), legend.position="none") +
  scale_y_continuous(limits = c(-2, 7)) +
  guides(x =  guide_axis(angle = 90), y = "none",fill="none")+
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = mycol, guide = "none") +
  scale_fill_manual("Period", values = mycol2, guide = "none")+
  annotate("text", x=-0.5, y=6.5, label= "(D)",size = 5)


p5<- dat_anomaly_withmean %>% filter(model  == "Ensemble_Mean" & variable == "Bottom_Temperature") %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = date, y =  anomaly, group = interaction(gc_rc, period), col = period), alpha = 0.25) +
  geom_line( data = dat_an_ag_mean %>% filter(model == "Ensemble_Mean" & variable == "Bottom_Temperature"),
            aes(x = date, y = mean, group = period, col = period),size = 1.2)+
  scale_y_continuous(limits = c(-2, 7),breaks=seq(-2,6,2), labels=seq(-2,6,2)) +
  theme_minimal(base_size = 17) + theme(plot.margin = margin(0, 0, 0, 0),
                                        legend.position="none") +
  labs( y = NULL, x="Date") +
  scale_color_manual("Period", values = mycol, guide = "none")  +
  annotate("text", x=1990, y=6.5, label= "(E)  Bottom Temperature",size = 5)

p6 <- dat_last10 %>% filter(variable == "Bottom_Temperature")%>% ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(y = anomaly, col = period, fill = period)) +
  geom_text(data = df_label, aes(x = (c(-0.28,-0.09,0.28,0.09)-0.07),
                                 y = c(1, 2, 4, 2),
                                 label = period , angle = 90,
                                 color = period) , size = 5) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0), legend.position="none") +
  scale_y_continuous(limits = c(-2, 7)) +
  guides(x =  guide_axis(angle = 90), y = "none",fill="none")+
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = mycol, guide = "none") +
  scale_fill_manual("Period", values = mycol2, guide = "none") +
  annotate("text", x=-0.5, y=6.5, label= "(F)",size = 5)


pp <- p1+p2+p3+p4+p5+p6+plot_layout(widths = c(3.5, 1))
p_lab <- 
  ggplot() + 
  annotate(geom = "text", x=1,y=1,label = "Anomaly (K)", angle = 90, size=8) +
  coord_cartesian(clip = "off")+
  theme_void()

my_plot <- p_lab +pp+ plot_layout(widths=c(20,1000))
my_plot


ggsave('Fig5_warming_air_ensemblemean_noendinfor.png', my_plot,  dpi = 300,width = 200,height = 300, units = 'mm')

