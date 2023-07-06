setwd("/media/shikhani/Samsung_T5/Cordex_work/calib_october_sw/")

library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

export_config(config_file = config_file, model = model)
ncdf <- 'output/ensemble_output.nc'


resLHC = cali_ensemble(config_file = config_file, num = 1000, cmethod = "LHC", parallel = T, model = model)
resLHCall <- resLHC
#save(resLHC , file = 'cal_LHC_July5_ssonly_new_good.RData')
res_LHC_pp <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resLHC ))
best_p <- setNames(lapply(model, function(m)res_LHC_pp[[m]][which.min(res_LHC_pp[[m]]$rmse), ]), model)
print(best_p)
save(best_p, file = 'bestp_LHC_5october_all1000.RData')

