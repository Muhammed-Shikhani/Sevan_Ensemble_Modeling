# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)
library(tidyverse)
library(glmtools)

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("GOTM")

export_config(config_file = config_file, model = model)

resMCMC = cali_ensemble(config_file = config_file, num = 5000, cmethod = "LHC", parallel = F, model = model)

save(resMCMC, file = 'cal_LHC_5000_GOTM.RData')

res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resMCMC))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)
# save(resMCMC, file = 'cal_MCMC_5000_GOTM.RData')
# 
# best_par <- setNames(lapply(model, function(m)resMCMC[[m]]$bestpar), model)
# print(best_par)
# 
# pairs(resMCMC$GOTM)
# plot(resMCMC$GOTM)
# summary(resMCMC$GOTM)
