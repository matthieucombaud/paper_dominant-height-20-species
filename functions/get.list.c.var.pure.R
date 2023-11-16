get.list.c.var.pure <- function(
  list.species,
  list.model
){
  
  list.db.param <- lapply(list.species, function(sp.sel){
    return(unique(list.model[[sp.sel]]$best.model$param[param_class %in% c("alpha", "gamma"), param]))
  })
  names(list.db.param) <- list.species
  
  return(list.db.param)
   
}
