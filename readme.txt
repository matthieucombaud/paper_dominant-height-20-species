R scripts and functions accompanying the paper "Climate change altered the dynamics of stand dominant height in forests during the past century - analysis of 20 European tree species"

Data preparation
****************

Raw data are not provided in this depository since they are either publicly available elsewhere or are not public
(see data availability statement of the paper).

Target pipeline to prepare data  is "R/target_data_part1.R".
R functions used in the pipeline used to prepare data are in "R/functions".
Python script to prepare FYRE climate data are in the "python" directory.

Model calibration
*****************

Script to launch model calibration for each species is "R/script_calibration_hdom_climate.R"
Required R functions for the calibration are in "R/functions".
cpp optimization function is "cpp/likelihood_final_beta.cpp".

Model calibration outputs are in "calibration_results". In the output name:
- "no_batch" indicates than calibration was done on the full sample
- "with_batches" indicates than calibration was a 5-fold calibration

Model analysis
*************

Target pipeline to analyse model calibration outputs is "R/target_analysis_part1_new.R"
R functions used in the pipeline used to analyze model calibration outputs are in "R/functions".
Script to produce the figures and tables is in the directory "R".


For additional information, please refer to the paper and contact Matthieu COMBAUD at m.combaud@gmail.com.
