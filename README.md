# Sevan_Ensemble_Modeling
This repository contains the setup and workflow for the project "Combining a Multi-Lake Model Ensemble and a Multi-Domain CORDEX Climate Data Ensemble for Assessing Climate Change Impacts on Lake Sevan" by Shikhani et al., 2024, published in Water Resources Research.
Project Overview

The project integrates CORDEX climate data from three different domains (MNA, WAS, CAS) with five 1D lake hydrodynamic models available through the LakeEnsemblR open-source platform. This modeling framework is used to evaluate the impact of climate change on Lake Sevan.
CORDEX Data

The CORDEX data used in this project can be freely retrieved from the ESGF portal. These datasets were used to force the models along with ERA5 reanalysis data. However, to avoid conflicts with already published data, ERA5 and CORDEX data are not uploaded to this repository.
Repository Structure

This repository contains: 
    -  Model Setup and Input Data: Configuration files for the LakeEnsemblR models and all the required input data.    
   -  Calibration and Validation Outputs: The results from the calibration and validation of the models.
   -  Annual Aggregated Outputs: Processed output files providing annual aggregated results.
   -  Preprocessing and Bias Correction Codes: Scripts used to preprocess, regrid, and bias-correct the CORDEX and ERA5 data.
   - Workflow Codes: Codes implementing the workflow for running the ensemble of lake models.
   -  Figure Generation Codes: Scripts used to generate figures for the manuscript and supplementary information.


Data Usage

Please note that the ERA5 and CORDEX data used to force the models are not included in this repository. Users should obtain these datasets directly from their respective sources.
Citation

If you use the models or codes from this repository, please consider citing the following paper:

Shikhani et al., Combining a Multi-Lake Model Ensemble and a Multi-Domain CORDEX Climate Data Ensemble for Assessing Climate Change Impacts on Lake Sevan, 2024, Water Resources Research.
