analyse_best_model <- function(
  parameters_included,
  db_explanatory,
  data_age_height,
  range.limit,
  likelihood_file,
  likelihood_folder,
  initial.height,
  qualitative_variables
  ){
  
  # compile files
  path_likelihood_file <- paste0(likelihood_folder,"/",likelihood_file,".cpp")
  compile(path_likelihood_file) # compile files
  dyn.load(dynlib(gsub("\\.cpp", "", path_likelihood_file))) # dynamically load the dll previously compiled (do it here so as to avoid redoing it for each run of the model)
  
  # get the name of all the variable as in the db, ie with the exact names of categorical variables (and not the generic name) ans also square variables
  variable_names <- setdiff(names(db_explanatory),c("stand","climatic_year"))
  
  # prepare parameter
  parameters_included_expanded <- expand_categorical_variables(
    list_parameters_selected = parameters_included,
    qualitative_variables = qualitative_variables,
    variable_names = variable_names
  )
  
  # infer parameter
  best_model <- fit_data_final(
    parameters_included = parameters_included_expanded,
    db_explanatory = db_explanatory,
    data_age_height = data_age_height,
    range.limit = range.limit,
    likelihood_file = likelihood_file,
    initial.height = initial.height
    )

  dyn.unload(dynlib(gsub("\\.cpp", "", path_likelihood_file))) # to avoid error in case of a new loading (eg in "select_variables")
  
  # return
  return(best_model)
  
}
