calibrate.pure <- function(
  sp,
  batch,
  include_variable,
  output_folder,
  vif.threshold,
  n.core,
  range.limit,
  likelihood_file,
  likelihood_folder,
  list_data.inference,
  qualitative_variables
  ){
  
  # required packages
  {
    require(stringr)
    require(ggplot2)
    require(gstat)
    require(data.table)
    require(readr)
    require(stringr)
    require(TMB)
    require(tmbstan)
    require(psych)
    require(foreach)
    require(doParallel)
  }

  # svg name
  svg_name <- paste0("sp", sp) # attention, must be at the end of the svg name to facilitate its use
  print(svg_name)
  
  # import data
  data_ifn_prepared <- list_data.inference[[sp]]

  # select explanatory variables to include in the selection process
  explanatory_variables <- intersect(include_variable, data_ifn_prepared$explanatory_variables) # without square and with generic names for qualitative variables # intersect because some variables may be deleted from data_ifn_prepared$explanatory_variables because of not enough variability
  explanatory_variables_developped <- unname(unlist(lapply(explanatory_variables, function(variable.selected){ # with square and with developped names for qualitative variables
    
    return(
      names(data_ifn_prepared$db_explanatory_normalized_reduced)[
        str_detect(names(data_ifn_prepared$db_explanatory_normalized_reduced), variable.selected)
        ])
    
  })))
  
  print("final explanatory variables:")
  print(explanatory_variables_developped)
  
  # get the data
  db_explanatory <- data_ifn_prepared$db_explanatory_normalized_reduced[,c("stand", "climatic_year", explanatory_variables_developped), with = FALSE]
  data_age_height <- data_ifn_prepared$data_age_height
  initial.height <- data_ifn_prepared$initial.height
  stand_number <- data_ifn_prepared$stand_number
  
  # ensure that the envi db is order by stand and then by climatic year and that db_explanatory and data_age_height have the same stand order
  keyv <- c("stand", "climatic_year")
  setkeyv(db_explanatory, keyv)
  setkeyv(data_age_height, "stand")
  if(!identical(unique(db_explanatory$stand),data_age_height$stand)){ stop("pb stand order")} # check stand order
  
  # list parameters to test
  list_parameters_to_test <- list(
    alpha = unname(explanatory_variables),
    gamma = unname(explanatory_variables[str_detect(explanatory_variables, "Tmean") | str_detect(explanatory_variables, "sgdd")])
  )  
  
  # select variables
  variable_selection <- select_variables(
    list_parameters_to_test = list_parameters_to_test,
    db_explanatory = db_explanatory,
    data_age_height = data_age_height,
    range.limit = range.limit,
    vif.threshold = vif.threshold,
    n.core = n.core,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    initial.height = initial.height,
    qualitative_variables = qualitative_variables
  )
  
  print("variable selection ok")
  
  # analyse best model
  best_model <- analyse_best_model(
    parameters_included = variable_selection$parameters_selected,
    db_explanatory = db_explanatory,
    data_age_height = data_age_height,
    range.limit = range.limit,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    initial.height = initial.height,
    qualitative_variables = qualitative_variables
  )

  print("ok calibration")
  
  # prepare output
  output <- list(
    sp=sp,
    include_variable=include_variable,
    stands=data_age_height$stand,
    vif.threshold=vif.threshold,
    initial.height=initial.height,
    likelihood_file=likelihood_file,
    variable_selection = variable_selection,
    best.model = best_model
  )
  
  # write output and return
  write_rds(output, paste0(output_folder,"/",svg_name,".rds"))
  return(output)
  
}

