library(lubridate)
library(LakeEnsemblR)
library(glmtools)
setwd("E:\\new_ewembi/EWEMBI_1979_2016/")

my.nml <- read_nml("glm3.nml")
my.met <- read.csv("EWEMBI_RAW/EWEMBI_orig.csv")
my.bthy <- data.frame (Depth_meter=my.nml$morphometry$H, Area_meterSquared=rev(my.nml$morphometry$A))
head(my.bthy)
my.bthy$Depth_meter <- my.bthy$Depth_meter-1821
my.bthy$Depth_meter[1]- my.bthy$Depth_meter[length(my.bthy$Depth_meter )]
write.csv(my.bthy, "LakensembleR/LakeEnsemblR_bathymetry_standard.csv", quote = F, row.names = F)

my.temp <- read.csv("point.3_databaseformat.csv")
head(my.temp)
names(my.temp) <- c("datetime","Depth_meter","Water_Temperature_celsius")
my.temp$datetime <- as.character(my.temp$datetime)
my.temp$datetime <- ymd_hms(my.temp$datetime)
my.temp$datetime <- my.temp$datetime- hours(12)
my.temp$datetime <-format(my.temp$datetime, "%Y-%m-%d 00:00:00")
head(my.temp)

write.csv(my.temp, "LakensembleR/LakeEnsemblR_wtemp_profile_standard.csv", quote = F, row.names = F)


head(my.met)
my.met$ps <- my.met$ps*100
my.met <- my.met[,-11]
names(my.met) <- c("datetime","Ten_Meter_Uwind_vector_meterPerSecond","Ten_Meter_Vwind_vector_meterPerSecond", "Sea_Level_Barometric_Pressure_pascal","Air_Temperature_celsius", "Relative_Humidity_percent", "Precipitation_millimeterPerDay","Shortwave_Radiation_Downwelling_wattPerMeterSquared","Longwave_Radiation_Downwelling_wattPerMeterSquared","Cloud_Cover_decimalFraction", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond"  )
write.csv(my.met, "LakensembleR/LakeEnsemblR_meteo_standard.csv", quote = F, row.names = F)


my.inflow <- read.csv("sevan_input/inflow_zero.csv")
my.inflow$Sal <- rep(0, nrow(my.inflow))
names(my.inflow) <- c("datetime","Flow_metersCubedPerSecond","Water_Temperature_celsius" , "Salinity_practicalSalinityUnits" )
write.csv(my.inflow, "LakeEnsemblR_inflow_standard.csv", quote = F, row.names = F)





# Load libraries
library(gotmtools)
library(LakeEnsemblR)

# Load LakeEnsemblR
library(LakeEnsemblR)

# Copy template folder
template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
#dir.create("example") # Create example folder
file.copy(from = template_folder, to = "LakensembleR", recursive = TRUE)
setwd("E:\\new_ewembi/EWEMBI_1979_2016/LakensembleR/") # Change working directory to example folder

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
#model <- c("FLake", "GLM", "Simstrat")
#model <- c( "GLM", "Simstrat")

# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
export_config(config_file = config_file, model = model)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model)





library(ggplot2)

## Plot model output using gotmtools/ggplot2
# Extract names of all the variables in netCDF
ncdf <- 'output/ensemble_output.nc'
vars <- gotmtools::list_vars(ncdf)
vars # Print variables

p1 <- plot_heatmap(ncdf)
p1
# Change the theme and increase text size for saving
p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(limits = c(0, 21),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))
# Save as a png file
ggsave('output/ensemble_heatmap.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')
##########################

pre_cal <- calc_fit(ncdf = "output/ensemble_output.nc", model = model)


cali_res <- cali_ensemble(config_file = config_file, num = 500, cmethod = "LHC",
                          parallel = TRUE, model = model)





res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(cali_res))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)


# we can plot the results of the latin hypercube calibration for e.g. GOTM using plot_LHC
plot_LHC(config_file = config_file, model = "GOTM", res_files = cali_res$GOTM,
         qual_met = "nse")
# or we can plot it for all models using
plot_LHC(config_file = config_file, model = model, res_files = unlist(cali_res),
         qual_met = "nse", best = "high")

# define path to the LakeEnsemblR netcdf output
ens_out <- "output/ensemble_output.nc"

# Plot depth and time-specific results
#p <- plot_ensemble(ncdf = ens_out, model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'), depth = 0.9,var = 'temp', date = as.POSIXct("2010-06-13", tz = "UTC"), boxwhisker = TRUE, residuals=TRUE)


p <- plot_ensemble(ncdf = ens_out, model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'), depth = 0.1,
                   var = 'temp', boxwhisker = TRUE, residuals = TRUE)
p

calc_fit(ncdf = "output/ensemble_output.nc",
         model = c("FLake", "GLM",  "GOTM", "Simstrat", "MyLake"),
         var = "temp")

plist <- plot_resid(ncdf = ens_out,var = "temp",model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'))
plist
analyse_df <- analyse_ncdf(ncdf = ens_out, spin_up = NULL, drho = 0.1, model = model)

# Example plot the summer stratification period
strat_df <- analyse_df$strat

p1 <- ggplot(strat_df, aes(model, TotStratDur)) +
  geom_col() +
  ylab("Total stratification duration [days]") +
  xlab("") +
  theme_classic()
p1
ggsave("output/model_ensemble_stratification.png", p1,  dpi = 300, width = 284, height = 284, units = "mm")