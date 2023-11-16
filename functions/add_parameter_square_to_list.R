add_parameter_square_to_list <- function(variable_new,list_parameters_selected_expanded,macroparameter){
  # function to add new parameters to a parameter list
  
  list_parameters_selected_expanded[[macroparameter]] <- c(variable_new,paste0(variable_new,"_square"),list_parameters_selected_expanded[[macroparameter]])
   
    return(list_parameters_selected_expanded)
    
  
}
