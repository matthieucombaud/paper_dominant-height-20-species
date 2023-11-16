summarize_model_ME_delta.DH_pair <- function(
  list_model, 
  list_species
){
  
  db.summary.model_1 <- rbindlist(lapply(list_species, function(species.focal.selected){
    
    print(species.focal.selected)
    
    db.summary.model_2 <- rbindlist(lapply(list_species, function(species.companion.selected){
      
      print(paste0("***", species.companion.selected))
      
      model.selected <- list_model[[species.focal.selected]][[species.companion.selected]]
      
      if(is.null(model.selected)){return(NULL)}
      if(is.na(model.selected$coefficients[2])){return(NULL)}
      
      db.summary.model_3 <- summary(model.selected)
      
      return(
        data.table(
          species.focal = species.focal.selected,
          species.companion = species.companion.selected,
          coeff = db.summary.model_3$coefficients[2,1],
          pvalue = db.summary.model_3$coefficients[2,4], 
          se = db.summary.model_3$coefficients[2,2]
        )
      )
      
    }))
    
    return(db.summary.model_2)
  }))
  
  return(db.summary.model_1)
  
}
