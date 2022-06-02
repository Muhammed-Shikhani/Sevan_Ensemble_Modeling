# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)
library(tidyverse)
library(glmtools)

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("GLM")

export_config(config_file = config_file, model = model)
eg_nml <- read_nml(nml_file = 'GLM/glm3.nml')
eg_nml <- set_nml(eg_nml, 'deep_mixing', 1)
write_nml(eg_nml, file = 'GLM/glm3.nml')

resMCMC = cali_ensemble(config_file = config_file, num = 500, cmethod = "LHC", parallel = F, model = model)

save(resMCMC, file = 'cal_LHC_100_GLM.RData')

res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resMCMC))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)

# save(resMCMC, file = 'cal_MCMC_5000_GLM.RData')
# 
# best_par <- setNames(lapply(model, function(m)resMCMC[[m]]$bestpar), model)
# print(best_par)
# 
# pairs(resMCMC$GLM)
# plot(resMCMC$GLM)
# summary(resMCMC$GLM)
