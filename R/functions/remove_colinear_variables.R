remove_colinear_variables <- function(data_to_decorrelate,all.variables_to_decorrelate,new.variable,vif.threshold){
  
  c.vif <- compute_vif(
    data_to_decorrelate = data_to_decorrelate,
    all.variables_to_decorrelate = all.variables_to_decorrelate
    )
  
  # identify which variables are above VIF threshold (excluding the new variable!)
  excess.vif <- names(c.vif[c.vif>vif.threshold])
  
  if(length(excess.vif)==1){
    variables.to.exclude <- excess.vif # in the case only the new variable poses problem, we exclude it (it gives a model already tested, we deal with that latter)
  }else{
    variables.to.exclude <- setdiff(excess.vif,new.variable) # else we exclude only the other variables (to give a chance to include the new variable in the model)
  }
  
  variables.to.keep <- setdiff(all.variables_to_decorrelate,variables.to.exclude)
  
  return(variables.to.keep)
  
}
