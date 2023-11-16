add_parameter_to_list <- function(variable_new,list_parameters_selected_expanded,macroparameter,qualitative_variables,variable_names){
  # function to add new parameters to a parameter list
  
  # expand new variable, if categorical
  if(variable_new %in% qualitative_variables){ # if quantitative variable, we add all the modalities except for one
    
    variable_new_modalities <- variable_names[str_detect(variable_names,variable_new)] # to capture all modalities, in case of categorical variable
    
    # remove a reference modality (preferentially the one called "other", because harder to interpret in output db)
    if(sum(str_detect(variable_new_modalities,"other"))==1){
      
      variable_new_modalities <- variable_new_modalities[-which(str_detect(variable_new_modalities,"other"))]
      
    }else{ # remove the last modality (not a random modality, since better to remove the same at each step / each try)
      variable_new_modalities <- variable_new_modalities[-length(variable_new_modalities)]
    }
    
    list_parameters_selected_expanded[[macroparameter]] <- setdiff(c(variable_new_modalities,list_parameters_selected_expanded[[macroparameter]]),variable_new)
    
  }else{
    
    list_parameters_selected_expanded[[macroparameter]] <- c(variable_new,list_parameters_selected_expanded[[macroparameter]])
    
  }
  
  return(list_parameters_selected_expanded)
  
}
