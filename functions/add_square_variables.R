add_square_variables<-function(db,quadratic_climatic_variables,quadratic_non_climatic_variables){
  
  variables_to_square <- find_complete_names_variables(all_names=names(db), # find complete names of variables for which we want to include de square in the computation
                                                     climatic_variables=quadratic_climatic_variables,
                                                     other_variables=quadratic_non_climatic_variables
                                                     )
  
  if(length(variables_to_square)>0){
    
    new_names <- paste0(variables_to_square,"_square")
    db[ , (new_names) := lapply( .SD, function(x) c(x^2)), .SDcols = variables_to_square ]
    
  }
  
  return(db)
  
}
