setwd("/home/shikhani/projects/LER_Sevan/calib_valid_LER_Sevan_copy/")

library(gridExtra)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake", "GLM", "GOTM", "Simstrat","MyLake")
ncdf <- 'output/ensemble_output.nc'


#################
calib_start <- "2008-01-01 00:00:00"
calib_end <- "2014-01-01 00:00:00"
valid_start <- "2014-01-01 00:00:00"
valid_end <- "2017-01-01 00:00:00"

input_yaml_multiple(file = "LakeEnsemblR.yaml" ,key1 = "time", key2 = "start", value = calib_start)
input_yaml_multiple(file = "LakeEnsemblR.yaml" ,key1 = "time", key2 = "stop", value = calib_end)
export_config(config_file = config_file, model = model)



run_ensemble(config_file = config_file, model = model)
heatmap_calib <- plot_heatmap(ncdf = ncdf, model = model, var = 'temp')

ggsave('/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/heatmap_calibiration.png', heatmap_calib,  dpi = 300,width = 250,height = 300, units = 'mm')


p1 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
                    model = c("FLake", "GLM", "GOTM", "Simstrat","MyLake"),
                    var = "temp", depth = 0.1)

p1
calib_fit <- calc_fit(ncdf = ncdf,
                      model = model,
                      var = "temp")

calib_fit 
calib_fit_df <- cbind(rownames(do.call(rbind.data.frame,lapply(calib_fit,round,2))),do.call(rbind.data.frame,lapply(calib_fit,round,2)))
names(calib_fit_df)[1] <- "model"
rownames(calib_fit_df) <-NULL
write.csv(calib_fit_df , "/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/calib_fit.csv",quote = F, row.names = F )

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

gc()

pt <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+
  theme(legend.position="bottom",legend.key.size = unit(2,"cm"),legend.text=element_text(size=35))+ guides(color=guide_legend(override.aes=list(size=c(NA, NA,NA, NA,NA, NA,4),linetype = c(1, 1, 1,1,1,1,NA),linewidth = c(2.5, 2.5, 2.5,2.5,2.5,2.5,NA)) ))


mylegend <-g_legend(pt)

gc()
p1 <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p1$layers[[4]]$aes_params$size <- 3
gc()

p2 <- plot_ensemble(ncdf = ncdf, model = model, depth =20, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p2$layers[[4]]$aes_params$size <- 3
gc()

p3 <- plot_ensemble(ncdf = ncdf, model = model, depth =77, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p3$layers[[4]]$aes_params$size <- 3
gc()

p4 <- plot_ensemble(ncdf = ncdf, model = model,var ="ice_height")+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Ice Height (m)")
p4$layers[[4]]$aes_params$size <- 3

pp <- grid.arrange(arrangeGrob(p1, p2 ,p3 ,p4 ,
                               nrow=2),
                   mylegend, nrow=2,heights=c(8, 1))
pp
ggsave('/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/perfomance_calibiration.png', pp,  dpi = 300,width = 500,height = 400, units = 'mm')
gc()
############################
input_yaml_multiple(file = "LakeEnsemblR.yaml" ,key1 = "time", key2 = "start", value = valid_start)
input_yaml_multiple(file = "LakeEnsemblR.yaml" ,key1 = "time", key2 = "stop", value = valid_end)
export_config(config_file = config_file, model = model)


run_ensemble(config_file = config_file, model = model)
heatmap_calib <- plot_heatmap(ncdf = ncdf, model = model, var = 'temp')

ggsave('/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/heatmap_calibiration.png', heatmap_calib,  dpi = 300,width = 250,height = 300, units = 'mm')

valid_fit <- calc_fit(ncdf = ncdf,
                      model = model,
                      var = "temp")

valid_fit 

valid_fit_df <- cbind(rownames(do.call(rbind.data.frame,lapply(valid_fit,round,2))),do.call(rbind.data.frame,lapply(valid_fit,round,2)))
names(valid_fit_df)[1] <- "model"
rownames(valid_fit_df) <-NULL
write.csv(valid_fit_df , "/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/valid_fit.csv",quote = F, row.names = F )


gc()

pt <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+
  theme(legend.position="bottom",legend.key.size = unit(2,"cm"),legend.text=element_text(size=35))+ guides(color=guide_legend(override.aes=list(size=c(NA, NA,NA, NA,NA, NA,4),linetype = c(1, 1, 1,1,1,1,NA),linewidth = c(2.5, 2.5, 2.5,2.5,2.5,2.5,NA)) ))


mylegend <-g_legend(pt)

gc()
p1 <- plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p1$layers[[4]]$aes_params$size <- 3
gc()

p2 <- plot_ensemble(ncdf = ncdf, model = model, depth =20, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p2$layers[[4]]$aes_params$size <- 3
gc()

p3 <- plot_ensemble(ncdf = ncdf, model = model, depth =77, var = 'temp')+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Temp (°C)")
p3$layers[[4]]$aes_params$size <- 3
gc()

p4 <- plot_ensemble(ncdf = ncdf, model = model,var ="ice_height")+
  theme_classic(base_size = 35)+ theme(legend.position="none")+ ylab( "Ice Height (m)")
p4$layers[[4]]$aes_params$size <- 3

pp2 <- grid.arrange(arrangeGrob(p1, p2 ,p3 ,p4 ,
                               nrow=2),
                   mylegend, nrow=2,heights=c(8, 1))
ggsave('/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig4_model_performance/perfomance_validiation.png', pp2,  dpi = 300,width = 500,height = 400, units = 'mm')
gc()
