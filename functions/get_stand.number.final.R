get_stand.number.final <- function(
  list_data_for.inference, 
  list_species,
  saving.dir
){
  
  length_list_data_for.inference <- length(list_data_for.inference)
  
  db_stand.number.final <- rbindlist(lapply(1:length_list_data_for.inference, function(index){
    
    data_for.inference.selected <- list_data_for.inference[[index]]
    
    species.code <- list_species[index]
    
    if(length(data_for.inference.selected) == 1){ # mean no stand remain
      
      return(
        data.table(
          species.code = species.code, 
          N = 0,
          species.code.check = character()
        )
      )
      
      
    }
    
    return(
      data.table(
        species.code = species.code, 
        N = as.numeric(data_for.inference.selected$stand_number["length_IFN_data_final"]),
        species.code.check = data_for.inference.selected$species
      )
    )
    
  }))
  
  write.csv(db_stand.number.final, paste0(saving.dir,"/db_stand.number.final.csv"),row.names = F)
  
  return(db_stand.number.final)
  
}