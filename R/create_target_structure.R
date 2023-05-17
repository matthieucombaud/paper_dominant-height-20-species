library(targets)

# create .yaml file
tar_config_set(script = "target_data_part1.R", store = "store_data_part1", project = "project_data_part1")
tar_config_set(script = "target_analysis_part1_new.R", store = "store_analysis_part1_new", project = "project_analysis_part1_new")


# create infrastructure to prepare data
Sys.setenv(TAR_PROJECT = "project_data_part1")
use_targets()


Sys.setenv(TAR_PROJECT = "project_analysis_part1_new")
use_targets()


# launch targets for creating data
Sys.setenv(TAR_PROJECT = "project_data_part1")
tar_make()


# to open target objects from other projects
# tar_read(,)