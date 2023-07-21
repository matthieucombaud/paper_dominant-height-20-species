get_tab.categorical.variables <- function(
  list_inference
){
  
  name.list_inference <- sapply(list_inference, function(list_inference.selected){
    
    if(is.list(list_inference.selected)){ # "if" to avoid problem in case of empty list element (occurs if no data point)
        
        return(list_inference.selected$species)
      
    }
    
    return("no.keep")
    
  })
  
  names(list_inference) <- name.list_inference
  
  list.tab_categorical_var_2 <- lapply(list_inference, function(list_inference.selected){
    
    if(is.list(list_inference.selected)){ # "if" to avoid problem in case of empty list element (occurs if no data point)
      
      print(list_inference.selected$species)
      
      list.tab_categorical_var <- list_inference.selected$tab_categorical_var
      
      if(is.null(list.tab_categorical_var)){return(NULL)}
      
      tab.names <- sapply(list.tab_categorical_var, function(tab.selected){
        names(dimnames(tab.selected))
      })
      
      names(list.tab_categorical_var) <- tab.names
      
      return(list.tab_categorical_var)
      
    }
    
    return("no.keep")
    
  })

  
  return(list.tab_categorical_var_2)
  
}
