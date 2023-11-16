library(targets)
library(stringr)
library(data.table)
library(future)
library(psych) # for the "logistic" function

options(future.globals.maxSize = 1e10)

Sys.setenv(TAR_PROJECT = "project_calibration_nlminb.uncons")

tar_option_set(memory = "transient")
future::plan(future::multisession, workers = 6)


lapply(grep("R$", list.files("R/functions", recursive = TRUE), value = TRUE), function(x) source(file.path("R/functions", x)))

# load objects ----
list_data.inference <- tar_read(list_db_species_for_inference_keep, store = "store_data_part1") # calibration data
db_climate.monthly <- tar_read(db_climate_pure_stands, store = "store_data_part1")
db.radiation <- tar_read(db.radiation, store = "store_data_part1")
db.envi <- tar_read(db.envi.pure.stand, store = "store_data_part1")
list.db.normalization <- tar_read(list_normalization_constant, store = "store_data_part1")
db.stand <- tar_read(db.pure.stand.calib, store = "store_data_part1")


#*******************************************
# for debugging
# tar_load(c(
#   "include_variable",
#   "output_folder",
#   "vif.threshold",
#   "n.core",
#   "param.range.bio",
#   "likelihood_file_base",
#   "likelihood_folder",
#   "list_data.inference",
#   "qualitative_variables",
#   "rel.tol" ,
#   "iter.max",
#   "eval.max",
#   "method"
# ))
# 
# include_variable = include_variable
# output_folder = output_folder
# vif.threshold = vif.threshold
# n.core = n.core
# range.limit = param.range.bio
# likelihood_file = likelihood_file_base
# likelihood_folder = likelihood_folder
# list_data.inference = list_data.inference
# qualitative_variables = qualitative_variables
# rel.tol = rel.tol
# iter.max = iter.max
# eval.max = eval.max
# method = method

#*******

# Replace the target list below with your own:
list(

  tar_target(list_species, c("02", "03", "04", "05", "09", "10", "11", "12V", "14", "17C", "51", "52", "53CO","54","57A", "61", "62", "63", "64", "73")
),
  
  # variables to include in the calibration
  tar_target(include_variable_non_climatic,
             c("ROCHE.Calc",
               "soil_type",
               "humus_type",
               "sgdd_1_12",
               "pH",
               "CN",
               "P2O5",
               "soil_depth",
               "affroc_perc",
               "cailloux_perc",
               "slope",
               "expo_NS",
               "expo_EW",
               "SWHC",
               "radiation_annual"
             )),
  
  tar_target(include_variable_climatic,
             c("Tmean_9_11","Tmean_12_2","Tmean_3_5","Tmean_6_8",
               "Tmean_9_2","Tmean_3_8",
               "Tmean_9_8",
               "cwb_9_11","cwb_12_2","cwb_3_5","cwb_6_8",
               "cwb_9_2","cwb_3_8",
               "cwb_9_8", 
               "precipitation_9_11","precipitation_12_2","precipitation_3_5","precipitation_6_8",
               "precipitation_9_2","precipitation_3_8",
               "precipitation_9_8"
             )),
  
  tar_target(include_variable.unchecked, c(include_variable_non_climatic, include_variable_climatic)),
  tar_target(include_variable, sapply(include_variable.unchecked, function(x){ # remove empty spaces
    return(str_trim(x))
  })),
  
  tar_target(qualitative_variables, c(
    "ROCHE.Calc",
    "soil_type",
    "humus_type"
  )),
  
  # calibration param
  tar_target(vif.threshold, 2),
  tar_target(rel.tol, 10^-8),
  tar_target(iter.max, 10000),
  tar_target(eval.max, 10000),
  tar_target(method, "nlminb.uncons"),
  
  # where to find the likelihood function
  tar_target(likelihood_folder, "cpp"),
  tar_target(likelihood_file_base, "likelihood_base"),

  # output folder
  tar_target(output_folder, paste0("output/calibration_results/calibration_2023-11-03_nlminb.uncons")),
  
  # core number
  tar_target(n.core, 6),

  # model ----
  
  tar_target(param.range.bio, list(
    A0 = c(2, 0, 1000),
    C0 = c(0.05, 0, 1000),
    beta0 = c(0, -1000, 1000),
    alpha = c(0, -1000, 1000),
    gamma = c(0, -1000, 1000),
    sigma = c(1, 0, 1000),
    delta = c(0.5, -1000, 1000)
  )),
  
  tar_target(model_02, calibrate.pure_2(
    sp = "02",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_03, calibrate.pure_2(
    sp = "03",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),

  tar_target(model_04, calibrate.pure_2(
    sp = "04",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_05, calibrate.pure_2(
    sp = "05",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_09, calibrate.pure_2(
    sp = "09",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_10, calibrate.pure_2(
    sp = "10",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_11, calibrate.pure_2(
    sp = "11",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_12V, calibrate.pure_2(
    sp = "12V",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_14, calibrate.pure_2(
    sp = "14",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),

  tar_target(model_17C, calibrate.pure_2(
    sp = "17C",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_51, calibrate.pure_2(
    sp = "51",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_52, calibrate.pure_2(
    sp = "52",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_53CO, calibrate.pure_2(
    sp = "53CO",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_54, calibrate.pure_2(
    sp = "54",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_57A, calibrate.pure_2(
    sp = "57A",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_61, calibrate.pure_2(
    sp = "61",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_62, calibrate.pure_2(
    sp = "62",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_63, calibrate.pure_2(
    sp = "63",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_64, calibrate.pure_2(
    sp = "64",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model_73, calibrate.pure_2(
    sp = "73",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list_data.inference = list_data.inference,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  
  ## list models and param ----
  tar_target(list_model, 
             list(
               `02` = model_02,
               `03` = model_03,
               `04` = model_04,
               `05` = model_05,
               `09` = model_09,
               `10` = model_10,
               `11` = model_11,
               `12V` = model_12V,
               `14` = model_14, 
               `17C` = model_17C,
               `51` = model_51,
               `52` = model_52,
               `53CO` = model_53CO,
               `54` = model_54,
               `57A` = model_57A,
               `61` = model_61,
               `62` = model_62,
               `63` = model_63,
               `64` = model_64,
               `73` = model_73
             )),
  
  tar_target(list_db_param,
             get_list_db_param(
               list.species = list_species,
               list.model = list_model
             )),

  tar_target(list.c.var.base,
             get.list.c.var.pure(
               list.species = list_species,
               list.model = list_model
             )),

  # batch ----
  
  tar_target(list.data.batch, # divide db into 5 batches for each species
             divide.stands(
               nb.batch = 5,
               list_data.inference = list_data.inference,
               list.species = list_species
             )),
  
  tar_target(model.batch_02, calibrate.pure_3(
    sp = "02",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_03, calibrate.pure_3(
    sp = "03",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_04, calibrate.pure_3(
    sp = "04",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_05, calibrate.pure_3(
    sp = "05",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_09, calibrate.pure_3(
    sp = "09",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_10, calibrate.pure_3(
    sp = "10",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_11, calibrate.pure_3(
    sp = "11",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_12V, calibrate.pure_3(
    sp = "12V",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_14, calibrate.pure_3(
    sp = "14",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_17C, calibrate.pure_3(
    sp = "17C",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_51, calibrate.pure_3(
    sp = "51",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_52, calibrate.pure_3(
    sp = "52",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_53CO, calibrate.pure_3(
    sp = "53CO",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_54, calibrate.pure_3(
    sp = "54",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_57A, calibrate.pure_3(
    sp = "57A",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_61, calibrate.pure_3(
    sp = "61",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_62, calibrate.pure_3(
    sp = "62",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_63, calibrate.pure_3(
    sp = "63",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_64, calibrate.pure_3(
    sp = "64",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),
  
  tar_target(model.batch_73, calibrate.pure_3(
    sp = "73",
    include_variable = include_variable,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    n.core = n.core,
    range.limit = param.range.bio,
    likelihood_file = likelihood_file_base,
    likelihood_folder = likelihood_folder,
    list.data.batch = list.data.batch,
    qualitative_variables = qualitative_variables,
    rel.tol = rel.tol,
    iter.max = iter.max,
    eval.max = eval.max,
    method = method
  )),

  tar_target(list_model.batch, 
             list(
               `02` = model.batch_02,
               `03` = model.batch_03,
               `04` = model.batch_04,
               `05` = model.batch_05,
               `09` = model.batch_09,
               `10` = model.batch_10,
               `11` = model.batch_11,
               `12V` = model.batch_12V,
               `14` = model.batch_14, 
               `17C` = model.batch_17C,
               `51` = model.batch_51,
               `52` = model.batch_52,
               `53CO` = model.batch_53CO,
               `54` = model.batch_54,
               `57A` = model.batch_57A,
               `61` = model.batch_61,
               `62` = model.batch_62,
               `63` = model.batch_63,
               `64` = model.batch_64,
               `73` = model.batch_73
             )),

# Analysis ----

  tar_target(last_month, 8),
  tar_target(ref.period, 1891:1920),
  tar_target(list_seasonal.windows_climatic.month, list( # in terms of climatic month
    c(1:3),
    c(4:6),
    c(7:9),
    c(10:12),
    c(1:6),
    c(7:12),
    c(1:12)
  )),
  tar_target(comparison.age, 70),
  tar_target(comparison_year.observation, 2020),
  tar_target(initial.height, 1.3),

  # prepare db.envi without climate
  tar_target(db.envi.no.clim, 
             unique(db.envi[, -c(include_variable_climatic, "sgdd_1_12", "climatic_year"), with = FALSE]) # "sgdd_1_12" was not listed as a climatic variable...
  ),
  
  ## prepare actual and reference climate in the good format ----
  tar_target(db.climate.ref,
             prepare.virtual.climate(
               db_climate.monthly = db_climate.monthly,
               db.radiation = db.radiation,
               month.varying = "none",
               variables.varying = "none",
               list_seasonal.windows_climatic.month = list_seasonal.windows_climatic.month,
               ref.period = ref.period,
               last_month = last_month
             )),
  
  tar_target(db.climate.actual,
             prepare.virtual.climate(
               db_climate.monthly = db_climate.monthly,
               db.radiation = db.radiation,
               month.varying = 1:12,
               variables.varying = c("Tmean", "precipitation"),
               list_seasonal.windows_climatic.month = list_seasonal.windows_climatic.month,
               ref.period = ref.period,
               last_month = last_month
             )),
  
  ## prepare partial impact analysis ----
  
  tar_target(
    list_direct.partial.impact,
    get_direct.partial.impact(
      list.species = list_species,
      list_data.inference = list_data.inference,
      list_model = list_model,
      comparison.age = comparison.age, 
      quantile.focus = c(0.05, 0.95),
      qualitative_variables = qualitative_variables,
      initial.height = initial.height
    )),
  
  
  ## species Hp dynamics under reference and actual climates ----
  tar_target(
    db.dynamics_climate.actual,
    compute_height_base_2(
      list.species = list_species,
      db.stand = db.stand[, .(sp, stand, year_observation, year_first)][, ":="(year_observation = comparison_year.observation, age = comparison.age)], # db with columns stand, sp, year_observation, year_first
      db.envi = merge(db.envi.no.clim, db.climate.actual, by = c("stand")), # unnormalized data
      cat.var_names.gen = qualitative_variables,
      list.db.param = list_db_param,
      list.db.normalization = list.db.normalization,
      initial.height = initial.height
    )
  ),
  
  tar_target(
    db.dynamics_climate.ref,
    compute_height_base_2(
      list.species = list_species,
      db.stand = db.stand[, .(sp, stand, year_observation, year_first)][, ":="(year_observation = comparison_year.observation, age = comparison.age)], # db with columns stand, sp, year_observation, year_first
      db.envi = merge(db.envi.no.clim, db.climate.ref, by = c("stand")), # unnormalized data
      cat.var_names.gen = qualitative_variables,
      list.db.param = list_db_param,
      list.db.normalization = list.db.normalization,
      initial.height = initial.height
    )
  ),
  
  ## CC impact ----
  
  tar_target(
    db_CC.impact,
    compute_CC.impact(
      db.dynamics_climate.ref = db.dynamics_climate.ref,
      db.dynamics_climate.actual = db.dynamics_climate.actual,
      comparison.age = comparison.age
    )
  ),
  
  
  # Supplementary materials ----
  
  ## Model prediction ----
  
  tar_target(
    db.Hp_pred,
    compute_height_base_2(
      list.species = list_species,
      db.stand = db.stand,
      db.envi = db.envi,
      cat.var_names.gen = qualitative_variables,
      list.db.param = list_db_param,
      list.db.normalization = list.db.normalization,
      initial.height = initial.height
    )
  ),

  ## Optimism ----
  tar_target(
    db_optimism,
    compute.optimism(
      list.species = list_species,
      list_data.inference = list_data.inference,
      list_model.batch = list_model.batch,
      initial.height = 1.3
    )
  ),


  NULL
)