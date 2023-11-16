get.param.pure <- function(
  list.model
){
  
  list.sp <- sapply(list.model, function(model.sel){
    return(model.sel$sp)
  })
  
  list.db.par <- lapply(list.model, function(model.sel){
    return(model.sel$best.model$param.nlminb[, .(param_class, param, estimate)])
  })
  
  names(list.db.par) <- list.sp
  
  return(list.db.par)
  
}
