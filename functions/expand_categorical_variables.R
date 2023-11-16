expand_categorical_variables <- function(list_parameters_selected, qualitative_variables, variable_names){
  
  list_parameters_selected_temp <- list_parameters_selected
  
  for (macroparameter in c("alpha","gamma")){
    
    for (variable_selected in list_parameters_selected_temp[[macroparameter]]){
      
      if(variable_selected %in% qualitative_variables){ # if quantitative variable, we add all the modalities except for one
        
        variable_selected_modalities <- variable_names[str_detect(variable_names,variable_selected)] # to capture all modalities, in case of categorical variable
        
        # remove a reference modality (preferentially the one called "other", because harder to interpret in output db)
        if(sum(str_detect(variable_selected_modalities,"other"))==1){
          
          variable_selected_modalities <- variable_selected_modalities[-which(str_detect(variable_selected_modalities,"other"))]
          
        }else{ # remove the last modality (not a random modality, since better to remove the same at each step / each try)
          variable_selected_modalities <- variable_selected_modalities[-length(variable_selected_modalities)]
        }
        
        list_parameters_selected_temp[[macroparameter]] <- setdiff(c(list_parameters_selected_temp[[macroparameter]],variable_selected_modalities),variable_selected)
        
      }
      
    }
    
  }
  
  return(list_parameters_selected_temp)

}
