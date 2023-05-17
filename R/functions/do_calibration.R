do_calibration <- function(
  explanatory_variables,
  db_explanatory,
  data_age_height,
  vif.threshold,
  likelihood_file,
  likelihood_folder,
  break_gamma,
  initial.height,
  qualitative_variables,
  vary_macroparameter
  ){
 
    
  # model selection
  list_data_calib <- list(
    data_explanatory = db_explanatory,
    data_age_height = data_age_height
    )
  
  if(!identical(unique(db_explanatory$stand),data_age_height$stand)){ stop("pb stand order")}
  
  
  # list parameters to test
  
  list_parameters_to_test <- list(
    alpha = NULL,
    gamma = NULL,
    beta = NULL
  )  
  
  # choose which macroparameter to vary
  if("alpha" %in% vary_macroparameter){
    list_parameters_to_test$alpha <- unname(explanatory_variables)
  }
  if("gamma" %in% vary_macroparameter){
    list_parameters_to_test$gamma <- unname(explanatory_variables[str_detect(explanatory_variables, "Tmean") | str_detect(explanatory_variables, "sgdd")])
  }
  if("beta" %in% vary_macroparameter){
    list_parameters_to_test$beta <- unname(explanatory_variables)
  }
   
  # select variables
  variable_selection <- select_variables(
    list_parameters_to_test = list_parameters_to_test,
    list_data = list_data_calib,
    vif.threshold = vif.threshold,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    break_gamma=break_gamma,
    initial.height=initial.height,
    qualitative_variables=qualitative_variables,
    average_climate_for_correlation = average_climate_for_correlation,
    exclude_variable = exclude_variable
  )
  
  print("variable selection ok")
  
  # analyse best model
  best_model <- analyse_best_model(
    parameters_included = variable_selection$parameters_selected,
    list_data = list_data_calib,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    initial.height = initial.height
  )
  
  print("ok calibration model for standardized variables")
  
  
  # return
  output <- list(
    variable_selection = variable_selection,
    best_model = best_model
  )
  
  return(output)
  
}
