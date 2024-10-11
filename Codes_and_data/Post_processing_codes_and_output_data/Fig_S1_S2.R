setwd("/home/shikhani/Documents/Sevan_plotting_draft/revison/")

library(gridExtra)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
# Set config file & models
model <- c("FLake", "GLM", "GOTM", "Simstrat","MyLake")
ncdf <- 'ensemble_output_calib.nc'

heatmap_calib <- plot_heatmap(ncdf = ncdf, model = model, var = 'temp')+
  theme_classic(base_size = 15)+
  theme(panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"),  # Customize horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  geom_hline(yintercept = -20, color = "black", linewidth=1)+
  geom_hline(yintercept = -40, color = "black", linewidth=1)+
  geom_hline(yintercept = -60, color = "black", linewidth=1)+
  geom_hline(yintercept = -80, color = "black", linewidth=1)+
  geom_hline(yintercept = 0, color = "black", linewidth=1)




ggsave('heatmap_calibiration.png', heatmap_calib,  dpi = 300,width = 250,height = 300, units = 'mm')


###########



ncdf <- 'ensemble_output_validate.nc'

heatmap_calib <- plot_heatmap(ncdf = ncdf, model = model, var = 'temp')+
  theme_classic(base_size = 15)+
  theme(panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"),  # Customize horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  geom_hline(yintercept = -20, color = "black", linewidth=1)+
  geom_hline(yintercept = -40, color = "black", linewidth=1)+
  geom_hline(yintercept = -60, color = "black", linewidth=1)+
  geom_hline(yintercept = -80, color = "black", linewidth=1)+
  geom_hline(yintercept = 0, color = "black", linewidth=1)




ggsave('heatmap_validation.png', heatmap_calib,  dpi = 300,width = 250,height = 300, units = 'mm')
