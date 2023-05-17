find_complete_names_variables<-function(all_names,climatic_variables,other_variables){
  
  climatic_variables<-all_names[unlist(sapply(climatic_variables, function(x) { grep(x,all_names)}))] # add the month
  
  other_variables<-other_variables[unlist(sapply(other_variables, function(x) { x %in% all_names}))] # required also for other_variables, in case of some of them are removed in previous step of the process (because non-varying variable) # attention : do not use "grep" because we want only variables with eactly this name
  
  return(c(climatic_variables,other_variables))
  
}
