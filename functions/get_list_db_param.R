get_list_db_param <- function(
  list.species,
  list.model
){
  
  list.db.param <- lapply(list.species, function(sp.sel){
    list.model[[sp.sel]]$best.model$param
  })
  names(list.db.param) <- list.species
  
  return(list.db.param)
   
}