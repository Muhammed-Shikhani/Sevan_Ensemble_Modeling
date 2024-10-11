setwd("/home/shikhani/Documents/Sevan_plotting_draft/revision2/")

library(gridExtra)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
library(lubridate)
# Set config file & models
model <- c("FLake", "GLM", "GOTM", "Simstrat","MyLake")
ncdf <- 'ensemble_output_calib.nc'

fractions <- read.csv("/home/shikhani/Documents/Sevan_plotting_draft/revision2/ice_fractions_Sevan.csv")
fractions$Date <- as.POSIXct(fractions$Date, format = "%Y-%m-%d", tz = "UTC")
fractions$ice_fraction[which(fractions$ice_fraction<0.015)] <- 0
fractions <- fractions[which(year(fractions$Date) < 2014),]
fractions$ice_fraction[-which(month(fractions$Date)%in% c(10,11,12,1,2,3,4))] <- NA

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

gc()

pt <- plot_ensemble(ncdf = ncdf, model = c("FLake", "GLM", "GOTM", "Simstrat","MyLake"),
                    depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+
  theme(legend.position="bottom",legend.key.size = unit(2,"cm"),
        legend.text=element_text(size=35))+ guides(color=guide_legend(override.aes=list(size=c(NA, NA,NA, NA,NA, NA,4),linetype = c(1, 1, 1,1,1,1,NA),linewidth = c(2.5, 2.5, 2.5,2.5,2.5,2.5,NA)) ))




mylegend <-g_legend(pt)

gc()
p1 <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p1$layers[[4]]$aes_params$size <- 3
gc()

p2 <- plot_ensemble(ncdf = ncdf, model = model, depth =5, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p2$layers[[4]]$aes_params$size <- 3
gc()

p3 <- plot_ensemble(ncdf = ncdf, model = model, depth =10, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p3$layers[[4]]$aes_params$size <- 3
gc()

p4 <- plot_ensemble(ncdf = ncdf, model = model, depth =20, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p4$layers[[4]]$aes_params$size <- 3
gc()

p5 <- plot_ensemble(ncdf = ncdf, model = model, depth =77, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p5$layers[[4]]$aes_params$size <- 3
gc()



p6 <- plot_ensemble(ncdf = ncdf, model = model, var = "ice_height") +
  theme_classic(base_size = 35) +
  theme(legend.position = "none") +
  geom_point(data = fractions, aes(x = Date, y = ice_fraction), size=1.75) +
  scale_y_continuous(name = "Ice Height (m)",
                     sec.axis = sec_axis(~., name = "Ice coverd area fraction ")) +
  theme(
    axis.text.y.right = element_text( ),  # Color for secondary y-axis labels
    axis.title.y.right = element_text(angle = 90, size = 25),  # Color for secondary y-axis title
    plot.title = element_text(size = 30),  # Adjust title font size
    axis.line.y.right = element_line( size = 1),  # Color for secondary y-axis line
    axis.ticks.y.right = element_line( size = 1),  # Color for secondary y-axis ticks
    axis.ticks.length = unit(0.25, "cm")  # Optional: Adjust tick length if desired
  )+
  labs(title ="Ice height vs observed ice area fraction")  # Remove plot title

p6$layers[[4]]$aes_params$size <- 3
gc()
pp <- grid.arrange(arrangeGrob(p1, p2 ,p3 ,p4 ,p5,p6,
                               nrow=3),
                   mylegend, nrow=2,heights=c(8, 1))

ggsave('Fig3_rev2_v2.png', pp,  dpi = 300,width = 550,height = 600, units = 'mm')
gc()
############################
#validation


ncdf <- 'ensemble_output_validate.nc'


fractions <- read.csv("/home/shikhani/Documents/Sevan_plotting_draft/revision2/ice_fractions_Sevan.csv")
fractions$Date <- as.POSIXct(fractions$Date, format = "%Y-%m-%d", tz = "UTC")
fractions$ice_fraction[which(fractions$ice_fraction<0.015)] <- 0
fractions <- fractions[which(year(fractions$Date) %in% c(2014:2016)),]
fractions$ice_fraction[-which(month(fractions$Date)%in% c(10,11,12,1,2,3,4))] <- NA


pt <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+
  theme(legend.position="bottom",legend.key.size = unit(2,"cm"),legend.text=element_text(size=35))+ guides(color=guide_legend(override.aes=list(size=c(NA, NA,NA, NA,NA, NA,4),linetype = c(1, 1, 1,1,1,1,NA),linewidth = c(2.5, 2.5, 2.5,2.5,2.5,2.5,NA)) ))


mylegend <-g_legend(pt)

gc()
p1 <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p1$layers[[4]]$aes_params$size <- 3
gc()

p2 <- plot_ensemble(ncdf = ncdf, model = model, depth =5, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p2$layers[[4]]$aes_params$size <- 3
gc()

p3 <- plot_ensemble(ncdf = ncdf, model = model, depth =10, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p3$layers[[4]]$aes_params$size <- 3
gc()

p4 <- plot_ensemble(ncdf = ncdf, model = model, depth =20, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p4$layers[[4]]$aes_params$size <- 3
gc()

p5 <- plot_ensemble(ncdf = ncdf, model = model, depth =77, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")+ theme(plot.title = element_text(size = 30))
p5$layers[[4]]$aes_params$size <- 3
gc()



p6 <- plot_ensemble(ncdf = ncdf, model = model, var = "ice_height") +
  theme_classic(base_size = 35) +
  theme(legend.position = "none") +
  geom_point(data = fractions, aes(x = Date, y = ice_fraction), size=1.75) +
  scale_y_continuous(name = "Ice Height (m)",
                     limits = c(0, 0.3),
                     sec.axis = sec_axis(~., name = "Ice coverd area fraction ")) +
  theme(plot.title = element_text(size = 30),
    axis.text.y.right = element_text( ),  # Color for secondary y-axis labels
    axis.title.y.right = element_text(angle = 90, size = 25),  # Color for secondary y-axis title
    axis.line.y.right = element_line(size = 1),  # Color for secondary y-axis line
    axis.ticks.y.right = element_line( size = 1),  # Color for secondary y-axis ticks
    axis.ticks.length = unit(0.25, "cm")  # Optional: Adjust tick length if desired
  )+
  labs(title ="Ice height vs observed ice area fraction")  # Remove plot title

p6$layers[[4]]$aes_params$size <- 3
gc()
pp <- grid.arrange(arrangeGrob(p1, p2 ,p3 ,p4 ,p5,p6,
                               nrow=3),
                   mylegend, nrow=2,heights=c(8, 1))

ggsave('FigS3_rev2_v2.png', pp,  dpi = 300,width = 550,height = 600, units = 'mm')
gc()


##################
fractions <- read.csv("/home/shikhani/Documents/Sevan_plotting_draft/revision2/ice_fractions_Sevan.csv")
fractions$Date <- as.POSIXct(fractions$Date, format = "%Y-%m-%d", tz = "UTC")
fractions <- fractions[which(year(fractions$Date) < 2014),]




s_ice <- load_var(ncdf = 'ensemble_output_calib.nc',var = "ice_height" )
s_ice$GLM$ice_height[which(is.na(s_ice$GLM$ice_height))] <- 0

plot(s_ice$Simstrat$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
cor(s_ice$Simstrat$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
cor(s_ice$GLM$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
cor(s_ice$FLake$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
cor(s_ice$MyLake$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
cor(s_ice$GOTM$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
plot(s_ice$GOTM$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
plot(s_ice$MyLake$ice_height[-nrow(s_ice$FLake)], fractions$ice_fraction)
abline(0,1)
summary(lm(s_ice$MyLake$ice_height[-nrow(s_ice$FLake)]~fractions$ice_fraction))
summary(lm(s_ice$Simstrat$ice_height[-nrow(s_ice$FLake)]~fractions$ice_fraction))
summary(lm(s_ice$GLM$ice_height[-nrow(s_ice$FLake)]~fractions$ice_fraction))
