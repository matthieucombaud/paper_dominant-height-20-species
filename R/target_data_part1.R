# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
Sys.setenv(TAR_PROJECT = "project_data_part1")

# packages ----
  # to be used outside targets
  packages.needed = c("psych","nlme","lme4","gstat","terra","dplyr", "RPostgreSQL",
                      "tidyr", "ggplot2", "data.table", "concaveman","TMB","doSNOW","foreach","readr",
                      "stringr",
                      "visNetwork","sf","raster","fasterize", "ggpubr")
  lapply(packages.needed, library, character.only = TRUE)
  
  # inside targets
  tar_option_set(
    packages = packages.needed, # packages that your targets need to run
    format = "rds" # default storage format
    # Set other options as needed.
  )

# functions ----
lapply(grep("R$", list.files("R/functions", recursive = TRUE), value = TRUE), function(x) source(file.path("R/functions", x)))


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.


# Replace the target list below with your own:
list(

  # set directory names
  tar_target(data_source,"data_source"), # source data
  tar_target(data_prepared,"data_prepared"), # prepared data
  

  # prepare IFN database ----
  
    ## import ----
    tar_target(zip_file, paste0(data_source,"/export_dataifn_2005_2020.zip"), format = "file"), # put a "format=file" to take into account change in the .zip
    
    tar_target(data,import_all_data(
      zip.file=zip_file,
      db_name=c("ARBRE","COUVERT","ECOLOGIE","FLORE","PLACETTE"), # initial names names of the databases
      new.names=c("db.tree","db.cover","db.ecology","db.flora","db.stand") # new names names of the databases
    )),
  
    ## get species correspondance
    tar_target(db_species_name, get_species_names(
      zip_file = zip_file,
      file_metadata = "espar-cdref13.csv",
      saving.dir = paste0(data_prepared,"/species_names")
      )),
  
    ## filtering first visit and forest stand
    tar_target(data_first_visit,lapply(data,keep_first_visit)), # keep first visit
    tar_target(forest.IDP,identify_forest_IDP(data$db.stand)),
    tar_target(data_first_visit_forest,lapply(data_first_visit,keep_IDP,selected.IDP=forest.IDP)), #keep forest IDP

    # prepare db.tree and db.dendro
    tar_target(coefficientsRDI_file, paste0(data_source,"/dendro/valeursCoefficientsRdi.txt"), format = "file"),
    tar_target(list.tree.dendro,prepare_db_tree.dendro(
      db.tree=data_first_visit_forest$db.tree,
      coefficientsRDI=as.data.table(read.delim(coefficientsRDI_file))
    )),
    tar_target(db.tree,prepare_db_tree_height_age(db.tree=list.tree.dendro$db.tree)),
    tar_target(db.dendro,list.tree.dendro$db.dendro),
    
    # prepare db.cover
    tar_target(db.cover,prepare_db_cover(db.cover=data_first_visit_forest$db.cover)),
    
    # prepare db.stand
    tar_target(db.stand,prepare_db_stand(db.stand=data_first_visit_forest$db.stand)),
    
    # prepare db.ecology
    tar_target(db.ecology,prepare_db_ecology(db.ecology=data_first_visit_forest$db.ecology)),
    
    # prepare db.flora
    tar_target(taxref_file, paste0(data_source,"/TAXREF_v13_2019/TAXREFv13.txt"), format = "file"),
    tar_target(indicator_values_file, paste0(data_source,"/bioindication/PredictionVI.xlsm"), format = "file"),
    tar_target(db.bioindication,prepare_db_flora(
      db.flora=data_first_visit_forest$db.flora,
      taxref=as.data.table(read_delim(taxref_file, delim = "\t",escape_double = FALSE,trim_ws = TRUE)), # we import only some columns
      VI=as.data.table(readxl::read_excel(indicator_values_file, sheet = "Valeurs indicatrices")) # indicator values per species (VI stands for "valeurs indicatrices")
    )),
    
    # prepare year database (year of observation)
    tar_target(db.year,db.stand[,.(IDP,year_observation=YEAR)]),
  
    # prepare altitude database
    tar_target(altitude_file, paste0(data_source,"/IFN_confidential_data/foret_placettes_inrae.csv"), format = "file"),
    tar_target(db.altitude,prepare_db_altitude(file=altitude_file)),
    
    # prepare radiation db,
    tar_target(radiation_file, paste0(data_source,"/climate/radiation/Rad_db_ifn.csv"), format = "file"),
    tar_target(db.radiation,prepare_db_radiation(file=radiation_file)),
    
    # merge database and filter over living trees for which we have height and age data
    tar_target(db.ifn.complete_old_names,merge_db_ifn(db.list=list(db.year,
                                                                   db.tree,
                                                                   db.stand[,-c("YEAR","VISIT_INDEX")],
                                                                   db.ecology[,-c("YEAR","VISIT_INDEX")],
                                                                   db.dendro[,-c("YEAR")],
                                                                   db.bioindication[,-c("YEAR")],
                                                                   db.cover[,-c("YEAR")],
                                                                   db.altitude,
                                                                   db.radiation)
    )),
    
    tar_target(db.ifn.complete,rename_ifn(db=db.ifn.complete_old_names)),
    
    # select even-age forest
    tar_target(stand.living,filter_tree_age_height_info(db.ifn.complete)), # identify stand with living trees and height & age measure
    tar_target(stand.regular.forest,select_regular_forest(db.ifn.complete,stand.living)), # select even-aged forest
    
    # select pure stand
    tar_target(stand.pure.stands,select_pure_stand(db.ifn.complete,stand.regular.forest)),
  
  # prepare climate data ----
    
    # get climate info from FYRE
    tar_target(list_files_temperature_fyre,paste0(data_source,"/climate/fyre/data_prepared/temperatures/tas_",c(1:25),".csv"),format="file"),
    tar_target(db_temperature_fyre,average_climate_ensemble_member(list_file=list_files_temperature_fyre)),
   
    tar_target(list_files_precipitation_fyre,paste0(data_source,"/climate/fyre/data_prepared/precipitations/Ptot_",c(1:25),".csv"),format="file"), 
    tar_target(db_precipitation_fyre,average_climate_ensemble_member(list_file=list_files_precipitation_fyre)),

    # get climate info from Safran (useful for the most recent years, since FYRE stops in 2012)
    tar_target(safran_zip,paste0(data_source,"/climate/safran/projet_d_extraction_876_20220131.zip"), format = "file"),
    tar_target(db_safran,read_and_format_safran(safran_zip = safran_zip,safran_file = "siclima_extraction_876_20220131.csv")), # not ' format = "file" ' because this is just an internal path in the zip, not a real file
    tar_target(db_temperature_safran,db_safran$temperature),
    tar_target(db_precipitation_safran,db_safran$precipitation),

    # correct the bias between Fyre and Safran db for temperature and precipitation
    tar_target(db_temperature_safran_corrected,unbiase_safran(db_safran=db_temperature_safran,db_fyre=db_temperature_fyre)),
    tar_target(db_precipitation_safran_corrected,unbiase_safran(db_safran=db_precipitation_safran,db_fyre=db_precipitation_fyre)),

    # # combine climate info from FYRE and from safran into a single db for temperature and a single database for precipitation
    tar_target(db_precipitation,combine_fyre_safran(db_fyre=db_precipitation_fyre,db_safran=db_precipitation_safran_corrected,reduce="safran")),
    tar_target(db_temperature,combine_fyre_safran(db_fyre=db_temperature_fyre,db_safran=db_temperature_safran_corrected,reduce="safran")),

    tar_target(db_precipitation_long,format_long(db=db_precipitation)),
    tar_target(db_temperature_long,format_long(db=db_temperature)),

    # get homogeneous climatic area
    tar_target(data_homogeneous_climatic_areas_file, paste0(data_source,"/climate/homogeneous_climatic_areas/isbagrid.Rdata"), format = "file"),
    tar_target(distance_homogeneous_climatic_areas,compute_distance_between_homogenous_climatic_area(file=data_homogeneous_climatic_areas_file)),
    tar_target(db_homogeneous_climatic_areas,prepare_data_homogeneous_climatic_areas(file=data_homogeneous_climatic_areas_file)),

    # compute lapse-rate
    tar_target(lapse_rate_temperature,get_lapse_rate_per_homogeneous_climatic_area(db_homogeneous_climatic_areas=db_homogeneous_climatic_areas,
                                                                                   distance=distance_homogeneous_climatic_areas,
                                                                                   db_clim_long=db_temperature_long)),
    tar_target(lapse_rate_precipitation,get_lapse_rate_per_homogeneous_climatic_area(db_homogeneous_climatic_areas=db_homogeneous_climatic_areas,
                                                                                     distance=distance_homogeneous_climatic_areas,
                                                                                     db_clim_long=db_precipitation_long)),
    # get safran cell coordinates
    tar_target(safran_map_zip, paste0(data_source,"/climate/safran/safran.zip"), format = "file"),
    tar_target(safran_map_wgs,read_safran_cell_data(safran_map_zip = safran_map_zip, data_source = data_source)),

  
  # Prepare db IFN + climate ----
    
  tar_target(last_month,8),
  tar_target(duration,c(3,6,12)),
  tar_target(crs_IFN,2154),
  
  # entire db
  tar_target(
    db_climate,
    prepare_ifn_climate(
      db = db.ifn.complete,
      safran_map=safran_map_wgs,
      crs_IFN=crs_IFN,
      db_temperature_long=db_temperature_long,
      db_precipitation_long=db_precipitation_long,
      lapse_rate_temperature=lapse_rate_temperature,
      lapse_rate_precipitation=lapse_rate_precipitation,
      db_homogeneous_climatic_areas=db_homogeneous_climatic_areas
    )
  ),
  
  # write csv
  tar_target(
    check.write.db_climate,
    write_db_climate(
      db_climate = db_climate,
      saving.dir = paste0(data_prepared,"/db_climate.monthly")
    )),
  
  # create a target with only stands of interest (to ease loading in the analysis part)
  tar_target(
    db_climate_pure_stands,
    db_climate[stand %in% stand.pure.stands]
  ),
  
  # compute climate over selected seasonal windows
  tar_target(
    db_climate_seasonnal_windows,
    aggregate_climate_global(
      db = db_climate,
      last_month = last_month,
      # duration = duration,
      list_period_climatic.month = list( # in terms of climatic month
        c(1:3),
        c(4:6),
        c(7:9),
        c(10:12),
        c(1:6),
        c(7:12),
        c(1:12)
        )
      )
    ),
  
    # get other variables

      # co2
      tar_target(co2_distant_year,
                 paste0(data_source,"/co2/mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv"),format="file"),
      tar_target(co2_recent_year,paste0(data_source,"/co2/co2_annmean_mlo.csv"),format="file"),
      tar_target(db_co2,get_co2(file_distant_years=co2_distant_year,file_recent_years=co2_recent_year)),

      # annual radiations
      tar_target(db_radiation_annual,prepare_radiation_annual(db = db.ifn.complete)),
  
      # minimum and maximum temperatures
      tar_target(db_minimum_temperature, get_minimum_temperature(db=db_climate,last_month=last_month)),
      tar_target(db_maximum_temperature, get_maximum_temperature(db=db_climate,last_month=last_month)),

  # write the db with raw data ----

    # get number of stand per species
    tar_target(stand.number_pure.stands, get_number_stands(
      db_ifn = db.ifn.complete,
      stands_selected = stand.pure.stands,
      db_species_name = db_species_name,
      saving.dir = paste0(data_prepared,"/stand.number_pure.stands")
    )),

    # get all species list
    tar_target(list_species_pure.stand_all,stand.number_pure.stands[N >=1, species.code]), # attention : may not be perfectly the number of stands we can sutdy at the end, because some of them are outside the safran perimeter + we need to get rid of some stands with missing variables after.

    # # get species with a threshold of 'threshold_nb.stands' stands
    # tar_target(threshold_nb.stands, 50),
    # tar_target(list_species_pure.stand_selected,names(stand.number_pure.stands[stand.number_pure.stands >= threshold_nb.stands])), # attention : may not be perfectly the number of stands we can sutdy at the end, because some of them are outside the safran perimeter + we need to get rid of some stands with missing variables after.

    # prepare database for all the species
    tar_target(
      list_db_species_raw,
      prepare_db_raw(
        species_selected = list_species_pure.stand_all,
        stands_selected = stand.pure.stands,
        db_ifn = db.ifn.complete,
        db_climate = db_climate_seasonnal_windows,
        db_co2 = db_co2,
        db_radiation_annual = db_radiation_annual,
        db_minimum_temperature = db_minimum_temperature,
        db_maximum_temperature = db_maximum_temperature, 
        output_folder = paste0(data_prepared,"/db_species_raw")
      ),
    pattern = list_species_pure.stand_all,
    iteration = "list"),

    # write raw db
    # tar_target(list_species_data.raw, write_db(db_list = db_species_raw_list, output_folder = paste0(data_prepared,"/db_species_raw"))),
  
  

  # prepare and write the db to be used for inference ----
  
     ## select variables of interest ----
  
     tar_target(climatic_variables,c("Tmean","cwb", "precipitation")),
  
     tar_target(non_climatic_variables, c(
      "Tmin",
      "Tmax",
      "ROCHE.Calc",
      "soil_type",
      "humus_type",
      "sgdd_1_12",
      "pH",
      "CN",
      "ST",
      "P2O5",
      "soil_depth",
      "affroc_perc",
      "cailloux_perc",
      "slope",
      "waterlogging_permanent",
      "waterlogging_temporary",
      "expo_NS",
      "expo_EW",
      "SWHC",
      "radiation_annual"
     )),
  
     tar_target(quadratic_climatic_variables, c("Tmean","cwb", "precipitation")),
  
     tar_target(quadratic_non_climatic_variables, c(
      "Tmin",
      "Tmax",
      "sgdd_1_12",
      "pH",
      "ST",
      "CN",
      "P2O5",
      "SWHC",
      "waterlogging_permanent",
      "waterlogging_temporary"
     )),
     tar_target(qualitative_variables, c(
      "ROCHE.Calc",
      "soil_type",
      "humus_type"
     )),
     tar_target(information_variables, c(
      "aquatic_element",
      "rdiUpper"
     )),
  
  
     ## write db for inference ----
  
      tar_target(
        db_species_for_inference_list,
        prepare_db_for_inference(
          db_species_raw = list_db_species_raw, # we use this argument and not 'list_species_pure.stand_all' to be sure that targets are executed in the right order
          correct.age = F,
          age_interval = NULL,
          height_min = NULL,
          climatic_variables = climatic_variables,
          non_climatic_variables = non_climatic_variables,
          information_variables = information_variables,
          quadratic_climatic_variables = quadratic_climatic_variables,
          quadratic_non_climatic_variables = quadratic_non_climatic_variables,
          qualitative_variables = qualitative_variables,
          output_folder = paste0(data_prepared,"/db_species_inference")
        ),
        pattern = list_db_species_raw,
        iteration = "list"
      ),
  
    # identify missing variables (without modifying previous code at this stage)
    tar_target(
      db_missing.variables,
      identify_missing.variables(
        db_species_raw = list_db_species_raw, # we use this argument and not 'list_species_pure.stand_all' to be sure that targets are executed in the right order
        correct.age = F,
        age_interval = NULL,
        height_min = NULL,
        climatic_variables = climatic_variables,
        non_climatic_variables = non_climatic_variables,
        information_variables = information_variables,
        quadratic_climatic_variables = quadratic_climatic_variables,
        quadratic_non_climatic_variables = quadratic_non_climatic_variables,
        qualitative_variables = qualitative_variables
      ),
      pattern = list_db_species_raw,
      iteration = "list"
    ),
  
  
    # get final number of stand per species ----
    tar_target(
      db_stand.number.final,
      get_stand.number.final(
        list_data_for.inference = db_species_for_inference_list,
        list_species = list_species_pure.stand_all,
        saving.dir = paste0(data_prepared,"/stand.number_pure.stands")
      )
    ),

      # attention: for species 29MA and 59, no database is generated because the number of stand after excluding the stand with missing variable is zero.
            
  NULL
 
)
