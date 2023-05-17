update_list_parameters_selected <- function(
  output_models,
  qualitative_variables
  ){
  
  # the variable names in output msut be comparable to those in "variable_to_test"
  
  list_parameters_selected <- list( # case where no variables are selected
    alpha = NULL,
    gamma = NULL,
    beta = NULL
  )
  
  if(dim(output_models[selection_metric==min(selection_metric,na.rm = T)])[1]>1){ # then we randomly choose a model
    
    print(" several model have the same value for the selection metric")
    
    index_selected.model <- sample( x = 1:dim(output_models[selection_metric==min(selection_metric,na.rm = T)])[1], size = 1)
    
    selected.model <- output_models[selection_metric==min(selection_metric,na.rm = T)][index_selected.model]
    
    
  }else{
    
    selected.model <- output_models[selection_metric==min(selection_metric,na.rm = T)]
    
  }
  

  # if some variable in alpha
  if(selected.model$param_alpha != ""){ # important condition to make the update process work
    
    parameters_selected_alpha <- unlist(str_split(string = selected.model$param_alpha,pattern=", ")) # get the full list
    # attention: keep square variables here, else they will not appear in the model!
    
    for(qualitative_variables_selected in qualitative_variables){ # apply a correction for qualitative variable (remove the names of specific modalities and keep the generic variable name)
      
      if(sum(str_detect(parameters_selected_alpha, qualitative_variables_selected)) > 0){ # case where the qualitative variable is selected
        
        parameters_selected_alpha <- parameters_selected_alpha[!str_detect(parameters_selected_alpha ,qualitative_variables_selected)]  # we remove the specific modalities
        parameters_selected_alpha <- c(parameters_selected_alpha,qualitative_variables_selected) # we keep the generic variable name
        
      }

    }
    
    list_parameters_selected$alpha <- parameters_selected_alpha

  }
  
  # if some variables in gamma
  if(selected.model$param_gamma != ""){ # important condition to make the update process work
    
    parameters_selected_gamma <- unlist(str_split(string = selected.model$param_gamma,pattern=", ")) # get the full list
    # attention: keep square variables here, else they will not appear in the model!
    
    for(qualitative_variables_selected in qualitative_variables){ # apply a correction for qualitative variable (remove the names of specific modalities and keep the generic variable name)
      
      if(sum(str_detect(parameters_selected_gamma ,qualitative_variables_selected)) > 0){ # case where the qualitative variable is selected
        
        parameters_selected_gamma <- parameters_selected_gamma[!str_detect(parameters_selected_gamma ,qualitative_variables_selected)]  # we remove the specific modalities
        parameters_selected_gamma <- c(parameters_selected_gamma,qualitative_variables_selected) # we keep the generic variable name
        
      }
      
    }
    list_parameters_selected$gamma <- parameters_selected_gamma
    
  }
  
  # if some variables in beta
  if(selected.model$param_beta != ""){ # important condition to make the update process work
    
    parameters_selected_beta <- unlist(str_split(string = selected.model$param_beta,pattern=", ")) # get the full list
    # attention: keep square variables here, else they will not appear in the model!
    
    for(qualitative_variables_selected in qualitative_variables){ # apply a correction for qualitative variable (remove the names of specific modalities and keep the generic variable name)
      
      if(sum(str_detect(parameters_selected_beta ,qualitative_variables_selected)) > 0){ # case where the qualitative variable is selected
        
        parameters_selected_beta <- parameters_selected_beta[!str_detect(parameters_selected_beta ,qualitative_variables_selected)]  # we remove the specific modalities
        parameters_selected_beta <- c(parameters_selected_beta,qualitative_variables_selected) # we keep the generic variable name
        
      }
      
    }
    list_parameters_selected$beta <- parameters_selected_beta
    
  }
  
  return(list_parameters_selected)
  
}
