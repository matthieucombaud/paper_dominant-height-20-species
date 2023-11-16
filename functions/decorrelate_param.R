decorrelate_param <- function(
  parameters,
  list_parameters_selected_expanded,
  data_to_decorrelate,
  vif.threshold
  ){
  
  # break_gamma=T means that the same variable can intervene both in alpha and gamma

  all.variables <- unique(unlist(parameters)) # attention, because of that, the function allows to include the same variable in both alpha and gamma: to avoid that, use the "break_gamma=F"
  
  new_variable_alpha_with_square <- setdiff(parameters$alpha,list_parameters_selected_expanded$alpha)
  new_variable_gamma_with_square <- setdiff(parameters$gamma,list_parameters_selected_expanded$gamma)
  new.variable_with_square <- setdiff(unlist(parameters),unlist(list_parameters_selected_expanded))
  
  # remove the square variables from the "new_variables" variables, because what we call a "new" variable is for the original variable, not its transformations.
  new_variable_alpha <- new_variable_alpha_with_square[!str_detect(new_variable_alpha_with_square,"square")]
  new_variable_gamma <- new_variable_gamma_with_square[!str_detect(new_variable_gamma_with_square,"square")]
  new.variable <- new.variable_with_square[!str_detect(new.variable_with_square,"square")]
  
  if(length(new.variable)==0){ # case where the new variable is already present in the other macrovariable
  
      # depending on whether the new variable is in alpha or gamma, we add it here and suppress it from the other macrovariable (alpha or gamma)
      if(length(new_variable_alpha) > 0){ # we want to test the variable in alpha, so we remove it from gamma
        # ">0" and not "==1" because if catagorical variable or variable with square, several modalities

        parameters_decorrelated<-list(
          alpha=c(list_parameters_selected_expanded$alpha,new_variable_alpha_with_square), # attention: new_variable_alpha_with_square can be with our withour square
          gamma=setdiff(list_parameters_selected_expanded$gamma,c(new_variable_alpha,paste0(new_variable_alpha,"_square"))) # we need to also remove the square variable, in case it has already been selected. Attention: removing "new_variable_alpha_with_square" is not enough, since it does not necessarily include the square variable
        )

      }
      
      if(length(new_variable_gamma) > 0){ # we want to test the variable in gamma, so we remove it from alpha
        # ">0" and not "==1" because if catagorical variable or variable with square, several modalities
        
        parameters_decorrelated <- list(
          alpha=setdiff(list_parameters_selected_expanded$alpha,c(new_variable_gamma,paste0(new_variable_gamma,"_square"))), # we need to also remove the square variable, in case it has already been selected
          gamma=c(list_parameters_selected_expanded$gamma,new_variable_gamma_with_square)
        )

      }
    
  }else{ # case where it is truly a new variable
    
    if(sum(str_detect(new.variable,"categorical"))==length(new.variable)){ # case when we added a categorical variable : no correlation computation then!
      
      parameters_decorrelated <- list(
        alpha=parameters$alpha,
        gamma=parameters$gamma
      )
      
      
    }else{
      
      all.variables.filtered <- iterate_remove_colinear_variables(
        data_to_decorrelate = data_to_decorrelate,
        all.variables = all.variables,
        new.variable = new.variable,
        vif.threshold = vif.threshold
      )
      
      # we then produce the list of decorrelated variable, allowing all the squared variables provided the original variable in non-correlated
      all.variables.filtered_with_square <- c(all.variables.filtered,paste0(all.variables.filtered,"_square"))
      
      parameters_decorrelated <- list(
        alpha = intersect(parameters$alpha, all.variables.filtered_with_square),
        gamma = intersect(parameters$gamma, all.variables.filtered_with_square)
      )
      
    }
    
  }

  return(parameters_decorrelated)
  
}
