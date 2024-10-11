library(lubridate)
library(plyr)
library(reshape2)
library(dplyr)
library(patchwork)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
# set wd to the folder the script is saved in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# clean up
rm(list = ls())
graphics.off()
cat("\f")

output_annual <- get(load("cordex_LER_annual.rda"))[1:2]

## my go at preparing the data using tidyr an plotting



# reshape all the data to a long format data.frame


dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))

# calculate the mean of the historic period for each model, run, and variable
mean_hist <- dat_l %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))

dat_anomaly <- dat_l %>% filter( model != "Ensemble_Mean") %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)

# calculate mean sd and quantiles for each variable and period
dat_an_ag <- dat_anomaly %>% group_by(date, period, variable) %>%
  summarise(mean = mean(anomaly),
            median = median(anomaly),
            sd = sd(anomaly),
            q25 = quantile(anomaly, 0.25),
            q75 = quantile(anomaly, 0.75),
            q05 = quantile(anomaly, 0.05),
            q95 = quantile(anomaly, 0.95))




dat_anomaly_withmean <- dat_l  %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)


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

# plot all variables with the interquantile range
ggplot(dat_an_ag) + geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
                                alpha = 0.333) +
  geom_line(aes(x = date, y = median, col = period)) +
  facet_wrap(.~variable, scales = "free") + theme_pubr() + grids() +
  scale_color_manual("Period", values = mycol) +
  scale_fill_manual("Period", values = mycol2)



dat_an_ag %>% filter(variable == "Surface_Temperature" ) %>% ggplot() +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = date, ymin = q05, ymax = q95, fill = period),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25, ymax = q75, fill = period),
              alpha = 0.4) +
  geom_line(aes(x = date, y = median, col = period)) +
  theme_minimal(base_size = 17) + theme(plot.margin = margin(0, 0, 0, 0),
                                        legend.position="none", axis.ticks.x = element_blank(),
                                        axis.text.x = element_blank()) +
  labs(x = NULL) + ylab("Surface temperature anamoly (K)") +
  scale_color_manual("Period", values = mycol, guide = "none") +
  scale_fill_manual("Period", values = mycol2, guide = "none")


strat_vars <- unique(dat_an_ag$variable)
titles <- c("Bottom Temperature" ,"Surface Temperature" )
#my_units <- c("day", "J m ~ ^-2","doy","doy")


my_labs <- c("Bottom Temperature Anomaly (K)" ,"Surface Temperature Anomaly (K)" )

for (v in 1:length( strat_vars )) {

  dat_an_ag_mean$model=factor(dat_an_ag_mean$model,
                              levels=c("FLake","GLM","GOTM","Simstrat", "MyLake","Ensemble_Mean"))
  
  p22 <-  dat_an_ag_mean %>% filter(variable == strat_vars[v] ) %>% ggplot() +
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
    #   xlab("Date")+ylab(as.factor(paste(titles[v],my_units[v],sep="~")))
    labs(y = my_labs[v], x="Date")
  
  ggsave(paste0(strat_vars[v],'.png'), p22,  dpi = 300,width = 600,height = 400, units = 'mm')
}
