R scripts and functions accompanying the paper "Climate change altered the dynamics of stand dominant height in forests during the past century - analysis of 20 European tree species"
***************************

Required R functions for data preparation, model calibration and analysis are in "R/functions".

Target pipeline to prepare data  is "R/target_data_part1.R".
Target pipeline to calibrate models and analyze data is "R/target_calibration_nlminb.uncons.R".

Python script to prepare FYRE climate data are in the "python" directory.

cpp optimization function to opimize likelihood is "cpp/likelihood_base.cpp".

Model calibration outputs are in "calibration_results", in the rds format. In the file name, the number is the identifier is the species identifier and "batch" means that this outpu was used to compute model optimism (cf. paper main text).
To access parameter values for species xx, use the following line
> output <- readRDS("calibration_results/model_xx")
> output $best.model$param

Please note that parameters apply to standardize variables.
To access normalization constant, use the following line
> db.normalization <- readRDS("normalization.intercept/list_normalization_constant")

Correspondance between species name and species unique identifier is provided in db_species_name.csv.

R script to prepare the manuscript tables and figures are in script_paper1_submission_3.Rmd

Raw data are not provided in this depository since they are either publicly available elsewhere or are not public
(see data availability statement of the paper).

********************
For additional information, please refer to the paper and contact Matthieu COMBAUD at m.combaud@gmail.com.
