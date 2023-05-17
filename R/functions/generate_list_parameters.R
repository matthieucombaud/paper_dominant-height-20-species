generate_list_parameters <- function(list_parameters_selected_expanded,list_parameters_to_test,qualitative_variables,variable_names){
  
  # add variable in alpha macrovariable
  list_alpha <- lapply(list_parameters_to_test$alpha,function(variable_new_selected){  

    add_parameter_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded = list_parameters_selected_expanded,
      macroparameter = "alpha",
      qualitative_variables = qualitative_variables,
      variable_names = variable_names
      )

  })
  
  # add variable and its square in alpha macrovariable
  list_alpha_square <- lapply(list_parameters_to_test$alpha[paste0(list_parameters_to_test$alpha,"_square") %in% variable_names],function(variable_new_selected){ #  case when we are interested in the the square variable: then we also consider the addition of the square variable directly
    
    add_parameter_square_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded=list_parameters_selected_expanded,
      macroparameter="alpha"
    )
    
  }) 
  
  # add variable in gamma macrovariable
  list_gamma <- lapply(list_parameters_to_test$gamma,function(variable_new_selected){  
    
    add_parameter_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded = list_parameters_selected_expanded,
      macroparameter = "gamma",
      qualitative_variables = qualitative_variables,
      variable_names = variable_names
    )
    
  })
  
  # add variable and its square in gamma macrovariable
  list_gamma_square <- lapply(list_parameters_to_test$gamma[paste0(list_parameters_to_test$gamma,"_square") %in% variable_names],function(variable_new_selected){ #  case when we are interested in the the square variable: then we also consider the addition of the square variable directly
    
    add_parameter_square_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded=list_parameters_selected_expanded,
      macroparameter="gamma"
    )
    
  }) 
  
  # add variable in beta macrovariable
  list_beta <- lapply(list_parameters_to_test$beta,function(variable_new_selected){  
    
    add_parameter_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded = list_parameters_selected_expanded,
      macroparameter = "beta",
      qualitative_variables = qualitative_variables,
      variable_names = variable_names
    )
    
  })
  
  # add variable and its square in beta macrovariable
  list_beta_square <- lapply(list_parameters_to_test$beta[paste0(list_parameters_to_test$beta,"_square") %in% variable_names],function(variable_new_selected){ #  case when we are interested in the the square variable: then we also consider the addition of the square variable directly
    
    add_parameter_square_to_list(
      variable_new = variable_new_selected,
      list_parameters_selected_expanded=list_parameters_selected_expanded,
      macroparameter="beta"
    )
    
  }) 
  
  return(c(list_alpha,list_alpha_square,list_gamma,list_gamma_square,list_beta,list_beta_square)) # This way of concatenate list remove possible null lists
  
}
