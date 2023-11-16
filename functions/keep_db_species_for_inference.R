keep_db_species_for_inference <- function(
  list_db_species_for_inference_all,
  list_species.keep
){
  
  list_inference.keep <- lapply(1: length(list_db_species_for_inference_all), function(index){
      
      if(is.list(list_db_species_for_inference_all[[index]])){ # "if" to avoid problem in case of empty list element (occurs if no data point)
        
        if(list_db_species_for_inference_all[[index]]$species %in% list_species.keep){
          
          return(list_db_species_for_inference_all[[index]])
  
        }
        
      }
      
      return(NULL)
  })
  
  list_inference.keep <- list_inference.keep[sapply(list_inference.keep, function(list_inference.selected){return(!is.null(list_inference.selected))})]

  names.list_inference.keep <- sapply(list_inference.keep, function(list_inference.selected){return(list_inference.selected$species)})
  
  names(list_inference.keep) <- names.list_inference.keep
  
  return(list_inference.keep)
  
}




