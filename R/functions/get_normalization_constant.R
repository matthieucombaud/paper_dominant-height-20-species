get_normalization_constant <- function(
  list_inference
){
  
  name.list_inference <- sapply(list_inference, function(list_inference.selected){
    
    if(is.list(list_inference.selected)){ # "if" to avoid problem in case of empty list element (occurs if no data point)
        
        return(list_inference.selected$species)
      
    }
    
    return("no.keep")
    
  })
  
  list_db.normalization <- lapply(list_inference, function(list_inference.selected){
    
    if(is.list(list_inference.selected)){ # "if" to avoid problem in case of empty list element (occurs if no data point)
      
      return(list_inference.selected$db_normalization_constant)
      
    }
    
    return("no.keep")
    
  })
  names(list_db.normalization) <- name.list_inference
  
  return(list_db.normalization)
  
}