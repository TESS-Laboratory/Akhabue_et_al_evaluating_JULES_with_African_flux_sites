# Evaluation of JULES LSM against 16 flux tower observations across Africa

This repository contains the analysis workflow used to evaluate the JULES land surface model against flux-tower observations across African ecosystems and bioclimatic regions. 
It includes scripts for processing observational and environmental data, comparing simulated and observed carbon and water fluxes, and assessing model performance across sites and environmental gradients.


________________________________________________________________________________________________________________________________________________________________________________________________________________

# Analysis and processing files are as follows
## Scripts
African_flux_meta_data.R
Processes metadata for African flux-tower sites and summarises site locations, ecosystem types, data availability and record lengths.

Albedo_JULES_sites.ipynb
Uses Google Earth Engine to extract satellite-derived albedo information for the flux-tower sites, including MODIS and Sentinel-2 products. 
The notebook also explores approaches for estimating bare-soil albedo using vegetation masks.

Alternate_lollipop_2.R
Calculates site-level mean annual temperature, mean annual precipitation, precipitation anomalies and aridity index from climate data. 
It produces lollipop-style figures showing the distribution of available flux-tower site-years across the climatic gradients and ecosystem types.

Annual_GPP.R
Calculates calendar-year observed and JULES-simulated gross primary productivity (GPP) using matched daily observations and a minimum annual data-coverage threshold. 
It evaluates interannual model performance using metrics including correlation, RMSE, bias, coefficient of variation, Nash–Sutcliffe efficiency and Kling–Gupta efficiency.

Clim_data.R
Processes temperature and precipitation data for the flux-tower sites to derive long-term mean annual temperature, mean annual precipitation and related climate summaries. 
It also generates figures describing the climatic distribution of the sites, including climate-gradient and Whittaker biome-space plots.

Diagnostics_hydrological_year.R
Provides diagnostic checks for determining rainy-season onset and hydrological-year boundaries from long-term precipitation climatologies. 
It produces site-level diagnostic plots and summary statistics used to assess the selected hydrological-year definition.

EG_Vs_PerformanceMetrics.R
Combines JULES performance statistics with site-level climatic variables and explores how bias, RMSE and correlation vary along environmental gradients. 
It generates comparisons for GPP, ecosystem respiration and evapotranspiration against mean annual temperature, precipitation and aridity.

ERA5_data_processing.R
Processes ERA5-Land meteorological data extracted for each flux-tower site, harmonising timestamps, variable names and units. 
It derives additional meteorological variables required for JULES forcing, including wind speed and vapour pressure deficit.

File_1000_runs_cs.R
Extracts soil-carbon variables from JULES NetCDF outputs following the model spin-up procedure. 
It visualises soil-carbon trajectories across soil layers and sites to support assessment of the initialised model state.

Flux_partitioning.ipynb
Applies the Python flux-partitioning functions to separate net ecosystem exchange into gross primary productivity and ecosystem respiration. 
The notebook tests and visualises the partitioning and then applies the workflow across multiple flux-tower sites.

Flux_partitioning_2.ipynb
Contains additional development and testing of the flux-partitioning workflow using individual site data. 
It was used to inspect daytime and nighttime GPP and respiration estimates before applying the workflow more broadly.

For_partitioning.R
Prepares the gap-filled flux and meteorological data for flux partitioning in Python. 
It combines the required variables and exports consistently named NEE, air temperature, radiation and vapour pressure deficit fields for each site.

Gap_filling_process.R
Post-processes ensemble gap-filling predictions for carbon dioxide, sensible heat and latent heat fluxes and uses them to replace missing observations. 
It subsequently combines the gap-filled and partitioned flux variables and derives evapotranspiration from latent heat.

gapfilling.py
Defines the Python functions used for machine-learning gap filling of flux observations using ensembles of XGBoost models. 
It includes artificial-gap sampling, model training and validation functions for evaluating gap-filling performance.

gapfilling.ipynb
Implements the gap-filling workflow using meteorological predictors and ensembles of XGBoost models. 
Artificial data gaps are used to evaluate predictive performance before estimates are generated for missing flux observations.

Hydrological_Annual_GPP.R
Calculates observed and JULES-simulated annual GPP using site-specific hydrological years. 
It applies matched-day and data-coverage criteria and evaluates interannual variability and model performance.

hydrological_year.R
Determines site-specific hydrological-year start dates from 1991–2020 ERA5-Land hourly precipitation. 
Daily precipitation climatologies and cumulative-anomaly methods are used to identify the seasonal rainfall cycle and derive hydrological-year boundaries.

Joining ERA5 with flux data.R
Joins the processed ERA5-Land meteorological variables with cleaned flux-tower observations using corresponding timestamps. 
The resulting site datasets provide combined meteorological and observational information for subsequent processing and JULES simulations.

jules_obs_flux_comparison.R
Compares daily JULES simulations with flux-tower observations for GPP, ecosystem respiration, sensible heat, latent heat and evapotranspiration. 
It calculates overall and yearly performance statistics, including correlation, RMSE and bias, and produces site-level comparison plots.

jules_obs_flux_comparison2.R
Provides an alternative presentation of the JULES–observation comparison in which results are grouped by flux variable across all sites. 
It calculates correlation, RMSE and bias and generates cross-site figures for each evaluated carbon or water flux.

O2 flux_data_cleaning.R
Cleans and harmonises flux-tower datasets obtained from multiple sources and supplied in different formats, units and naming conventions. 
It standardises timestamps and variables and applies site-specific quality-control and preprocessing steps.

partitioning2.py
Defines the functions used to partition net ecosystem exchange into GPP and ecosystem respiration. 
It implements nighttime respiration-based and daytime light-response approaches used by the flux-partitioning notebooks.

per_flux_GLMM.R
Fits mixed-effects models to investigate how JULES performance for GPP, ecosystem respiration and evapotranspiration varies with climatic conditions and precipitation anomalies. 
The script also performs model diagnostics and produces summaries and visualisations of the fitted environmental effects.

Plot_dump_files.R
Extracts total soil carbon from JULES spin-up NetCDF files across successive spin cycles. 
It produces site-level soil-carbon trajectories that are used to assess whether the model has approached equilibrium during spin-up.

Site_map.R
Creates the map of African flux-tower sites used in the model evaluation, distinguishing sites by ecosystem type. 
It also supports the combined site-location and Whittaker climate-space figure.

Summary for data files.Rmd
Documents the characteristics, inconsistencies and site-specific processing decisions associated with the original flux datasets.
It also records the intended sequence of the main data-cleaning and preprocessing scripts.

covariance_env_gradients.R
Examines relationships among the environmental gradients used in the analysis, including mean annual temperature and aridity index. 
It calculates correlations and produces diagnostic plots used to assess covariance among climatic predictors.

fsmc_lai_temp_precip.R
Extracts and visualises JULES soil-moisture stress, leaf area index, temperature and precipitation variables across the study sites. 
It examines both grid-box and plant-functional-type-level outputs to help diagnose simulated vegetation and environmental behaviour.

trunk.tar.gz
Compressed archive of the final Rose suite configuration used to run the JULES simulations for this study. 
It contains the suite configuration, metadata, and associated files required to reproduce the model setup.


## Project and reproducibility files
Plots/
Contains selected figures generated from the analysis, including the flux-site climate-space and environmental-gradient figures.

renv/
Contains the supporting files required to activate and configure the project-specific renv environment. 
Together with renv.lock, these files allow the R package environment to be reconstructed.

renv.lock
Records the R packages and package versions associated with the project so that the computational environment can be reproduced.

.Rprofile
Automatically activates the project-specific renv environment when the R project is opened.

.gitignore
Specifies local or machine-specific files that Git should not track or include in the repository.

evaluating_JULES_with_African_flux_sites.Rproj
Defines the RStudio project used to organise and run the R-based analyses in this repository.

README.md
Provides an overview of the repository, its purpose, analysis workflow and information needed to understand and reproduce the research.

