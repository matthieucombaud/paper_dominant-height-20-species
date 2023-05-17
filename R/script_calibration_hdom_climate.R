  library(stringr)
  
  # import functions ----
  lapply(grep("R$", list.files("R/functions", recursive = TRUE), value = TRUE), function(x) source(file.path("R/functions", x)))
  
  # arguments to change ----
  sp <- "12V"
  do_no_batch <- T
  do_with_batch <- T
  output_name <- "simplified"
  
  vif.threshold = 2
  break_gamma = FALSE
  nb_batch = 5 # number of batches for the robustness analysis
  
  # choose folders ----
  source_folder <- "data_prepared/db_species_inference"
  output_folder <- paste0("output/calibration_results/",output_name)
  dir.create(output_folder, recursive = T)
  
  # choose variables to include in the calibration ----
    
    # include_variable <- NULL
    include_variable_non_climatic <- c(
      # "Tmin",
      # "Tmax",
      "ROCHE.Calc",
      "soil_type",
      "humus_type",
      "sgdd_1_12",
      "pH",
      "CN",
      # "ST",
      "P2O5",
      "soil_depth",
      "affroc_perc",
      "cailloux_perc",
      "slope",
      # "waterlogging_permanent",
      # "waterlogging_temporary",
      "expo_NS",
      "expo_EW",
      "SWHC",
      "radiation_annual"
    )
    
    include_variable_climatic <- c(
      "Tmean_9_11","Tmean_12_2","Tmean_3_5","Tmean_6_8",
      "Tmean_9_2","Tmean_3_8",
      "Tmean_9_8",
      "cwb_9_11","cwb_12_2","cwb_3_5","cwb_6_8",
      "cwb_9_2","cwb_3_8",
      "cwb_9_8", 
      "precipitation_9_11","precipitation_12_2","precipitation_3_5","precipitation_6_8",
      "precipitation_9_2","precipitation_3_8",
      "precipitation_9_8"
    )
    
    include_variable <- c(include_variable_non_climatic,include_variable_climatic)
    
    if(!is.null(include_variable)){
      include_variable <- sapply(include_variable,function(x){ # remove empty spaces
        return(str_trim(x))
      })
    }
    
    # if(!is.null(exclude_variable)){
    #   exclude_variable <- sapply(exclude_variable,function(x){ # remove empty spaces
    #     return(str_trim(x))
    #   })
    # }
    
    # design qualitative variables
    qualitative_variables <- c(
      "ROCHE.Calc",
      "soil_type",
      "humus_type"
    )
    qualitative_variables <- setdiff(qualitative_variables,exclude_variable)
    
  # standard parametrization - not to change unless specific reasons ----
  vary_macroparameter <- c(
    "alpha",
    "gamma",
    # "beta",
    NULL
    )

  likelihood_folder = "cpp"
  likelihood_file = "likelihood_final_beta"
  
  # print ----
  print(sp)
  print(vary_macroparameter)
  print(output_folder)

  # calibrate ----
  
  if(do_no_batch == T){
    
    output_no_batch <- do_per_species(
      sp=sp,
      batch="no_batch",
      nb_batch=nb_batch,
      include_variable=include_variable,
      source_folder = source_folder,
      output_folder = output_folder,
      vif.threshold = vif.threshold,
      likelihood_file = likelihood_file,
      likelihood_folder = likelihood_folder,
      break_gamma = break_gamma,
      data_ifn_prepared_file = paste0(source_folder,"/db_for_inference_sp",sp,".rds"),
      qualitative_variables=qualitative_variables,
      vary_macroparameter = vary_macroparameter
    )
    
  }
  
  if(do_with_batch == T){
  
  output_with_batch <- do_per_species(
    sp=sp,
    batch="with_batches",
    nb_batch=nb_batch,
    include_variable=include_variable,
    source_folder = source_folder,
    output_folder = output_folder,
    vif.threshold = vif.threshold,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    break_gamma = break_gamma,
    data_ifn_prepared_file = paste0(source_folder,"/db_for_inference_sp",sp,".rds"),
    qualitative_variables=qualitative_variables,
    vary_macroparameter = vary_macroparameter
    )

  }
  
