setwd("/home/shikhani/Documents/from_robert10/Calibruns_LHC/Calibruns_LHC")
library(LakeEnsemblR)
library(ggplot2)
library(gotmtools)
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
export_config(config_file = config_file, model = model)
run_ensemble(config_file = config_file, model = model)
# Extract names of all the variables in netCDF
ncdf <- 'output/ensemble_output.nc'
vars <- gotmtools::list_vars(ncdf)
vars # Print variables
p1 <- plot_heatmap(ncdf)
p1
# Change the theme and increase text size for saving
p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(limits = c(0, 30),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))); p1
# Save as a png file
print(p1)
ggsave('output/heatmap_test_new.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')