decorrelate_list_param <- function(list_parameters_combination,list_parameters_selected_expanded,data_to_decorrelate,vif.threshold,break_gamma){
  
  # list_parameters is the list to test
  # list_parameters_selected is the list of parameters that have been selected at the previous step
  # data is the variable database
  # vif.threshold is the VIF threshold above which we reject variables
  
  list_parameters_decorrelated <- lapply(list_parameters_combination, function(list_parameters_combination_selected){

    parameters_decorrelated <- decorrelate_param(
      parameters = list_parameters_combination_selected,
      data_to_decorrelate = data_to_decorrelate,
      list_parameters_selected_expanded = list_parameters_selected_expanded,
      vif.threshold = vif.threshold,
      break_gamma = break_gamma
      )
    
    return(parameters_decorrelated)
    
  })

  
  return(list_parameters_decorrelated)

  
}
