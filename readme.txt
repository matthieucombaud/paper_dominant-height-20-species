"Climate change altered the dynamics of stand dominant height in forests during the past century - analysis of 20 European tree species"
R scripts and functions accompanying the paper
https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/OZLVLX 

Data preparation
****************

Raw data are not provided in this depository since they are either publicly available elsewhere or are not public
(see data availability statement of the paper).

Target pipeline to prepare data  is "R/target_data_part1.R".
R functions used in the pipeline used to prepare data are in "R/functions".
Python script to prepare FYRE climate data are "netcf_precipitation.py" and "netcf_temperature.py".

Model calibration
*****************

Script to launch model calibration for each species is "R/script_calibration_hdom_climate.R"
Required R functions for the calibration are in "R/functions".
cpp optimization function is "cpp/likelihood_final_beta.cpp".

Model calibration outputs are in "calibration_output". In the output name:
- "no_batch" indicate than calibration was done on the full sample
- "with_batches" indicate than calibration was a 5-fold calibration

Model analysis
*************

Target pipeline to analyse model calibration outputs is "R/target_analysis_part1_new.R"
R functions used in the pipeline used to analyze model calibration outputs are in "R/functions".


For additional information, please refer to the paper and contact Matthieu COMBAUD at m.combaud@gmail.com.
