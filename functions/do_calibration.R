do_calibration <- function(
  explanatory_variables,
  db_explanatory,
  data_age_height,
  range.limit,
  vif.threshold,
  likelihood_file,
  likelihood_folder,
  initial.height,
  qualitative_variables
  ){
 
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
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    initial.height=initial.height,
    qualitative_variables=qualitative_variables
  )
  
  print("variable selection ok")
  
  # analyse best model
  best_model <- analyse_best_model(
    parameters_included = variable_selection$parameters_selected,
    db_explanatory = db_explanatory,
    data_age_height = data_age_height,
    likelihood_file = likelihood_file,
    likelihood_folder = likelihood_folder,
    initial.height = initial.height,
    qualitative_variables = qualitative_variables
  )
  
  print("ok calibration model for standardized variables")
  
  # return
  output <- list(
    variable_selection = variable_selection,
    best_model = best_model
  )
  
  return(output)
  
}
