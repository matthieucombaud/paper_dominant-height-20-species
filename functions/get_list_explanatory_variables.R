get_list_explanatory_variables <- function(db_ifn,climatic_variables,non_climatic_variables,quadratic_climatic_variables,quadratic_non_climatic_variables){
  
  variables <- find_complete_names_variables(all_names=names(db_ifn), # find complete names of variables for which we want to include de square in the computation
                                           climatic_variables=climatic_variables,
                                           other_variables=non_climatic_variables
                                           )
  
  variables_to_square <- find_complete_names_variables(all_names=names(db_ifn), # find complete names of variables for which we want to include de square in the computation
                                                     climatic_variables=quadratic_climatic_variables,
                                                     other_variables=quadratic_non_climatic_variables
                                                     )
  
  if(length(variables_to_square)!=0){
    variables<-c(variables,paste0(variables_to_square,"_square"))
  }
  
  return(variables)
  
}
