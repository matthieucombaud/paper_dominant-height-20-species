iterate_remove_colinear_variables <- function(data_to_decorrelate,all.variables,new.variable,vif.threshold){
  
  # we need to iterate the procedure "remove variables based on VIF" because removing one variable may create high correlation (in terms of VIF) between the remaining variables
  
  all.variables_non_square <- unique(str_remove(all.variables,"_square")) # we do not want to test a square variable against its roots variable, because we want to test model with quadratic effects
  all.variables_categorical <- all.variables[str_detect(all.variables,"categorical")] # we do not want to test categorical variable because strong correlation between their modalities
  all.variables_to_decorrelate <- setdiff(all.variables_non_square,all.variables_categorical)
  
  iterate <- T
  
  filtered.variables <- all.variables_to_decorrelate # necessary to initialize in case length(all.variables_to_decorrelate) == 0 OR length(all.variables_to_decorrelate) ==1
  
  while(iterate==T & length(all.variables_to_decorrelate) > 1){ # we need to iterate because removing one variable may create high correlation (in terms of VIF between the remaining variables)
    
    filtered.variables <- remove_colinear_variables(
      data_to_decorrelate = data_to_decorrelate,
      all.variables_to_decorrelate = all.variables_to_decorrelate,
      new.variable = new.variable,
      vif.threshold = vif.threshold
      )
    
    if(length(filtered.variables)==length(all.variables_to_decorrelate)){ # we stop the loop only if no more evolution in the filtered variables (length equality means vector equality here), else it is always possible to have correlated variables!
      
      iterate <- F
      
    }else{
      
      all.variables_to_decorrelate <- filtered.variables
      
    }
  }
  
  filtered.variables <- c(filtered.variables,all.variables_categorical) # put again the categorical variables
  
  return(filtered.variables)
  
}
