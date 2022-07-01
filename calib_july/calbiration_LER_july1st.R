setwd("/home/shikhani/Documents/calib_july/")
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
model <- c("FLake", "GLM", "GOTM", "Simstrat")

export_config(config_file = config_file, model = model)
run_ensemble(config_file = config_file, model = model)
ncdf <- 'output/ensemble_output.nc'
vars <- gotmtools::list_vars(ncdf)
 vars # Print variables

 # p1 <- plot_heatmap(ncdf);p1
# p1 <- p1 +
#   theme_classic(base_size = 24) + 
#   scale_colour_gradientn(limits = c(0, 30),
#                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))); p1
#ggsave('output/heatmap_test.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')
 
resLHC = cali_ensemble(config_file = config_file, num = 300, cmethod = "LHC", parallel = T, model = model)
resLHCall <- resLHC
save(resLHC , file = 'cal_LHC_June30.RData')
res_LHC_pp <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resLHC ))
best_p <- setNames(lapply(model, function(m)res_LHC_pp[[m]][which.min(res_LHC_pp[[m]]$rmse), ]), model)
print(best_p)
save(best_p, file = 'bestp_LHC_june30.RData')


best_p <- setNames(lapply(model, function(m)res_LHC_pp[[m]][which.min(res_LHC_pp[[m]]$rmse), ]), model)
print(best_p)
best_p$GLM$light.kw <- NULL
names(best_p$GOTM)[10] <- "k_min"
for(m in 1:length(best_p)){
  for (v in 8:length(best_p[[m]])) {
    if(v %in% c(8,9)){
      key1 = "scaling_factors"
    }else{ key1 = "model_parameters"}
    input_yaml_multiple(file = "LakeEnsemblR.yaml" ,key1 = key1, key2 = names(best_p)[m], key3 = names(best_p[[m]])[v], value = best_p[[m]][[v]])
  }
}


export_config(config_file = config_file, model = model)
run_ensemble(config_file = config_file, model = model)
calc_fit(ncdf = ncdf,
         model = model,
         var = "temp")

plot_ensemble(ncdf = ncdf, model = model, depth =77, var = 'temp')
plot_ensemble(ncdf = ncdf, model = model, depth = 0.1, var = 'temp')
plot_ensemble(ncdf = ncdf, model = model,var ="ice_height")

