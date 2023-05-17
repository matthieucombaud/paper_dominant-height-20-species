compute_vif <- function(data_to_decorrelate,all.variables_to_decorrelate){
  
  data_to_decorrelate <- data_to_decorrelate[,..all.variables_to_decorrelate]
  
  list_VIF <- lapply(all.variables_to_decorrelate,function(x) {
    
    formula.vif <- paste0(x,"~0+",paste(setdiff(all.variables_to_decorrelate,x),collapse="+"))
    model.vif<-lm(formula=formula.vif, data=data_to_decorrelate)
    R2<-summary(model.vif)$r.squared
    
    return(1/(1-R2))
    
  })
  
  VIF <- unlist(list_VIF)
  names(VIF) <- all.variables_to_decorrelate

  return(VIF)
  
}
