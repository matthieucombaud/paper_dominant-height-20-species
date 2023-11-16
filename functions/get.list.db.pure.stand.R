get.list.db.pure.stand <- function(
  list_data.inference
){
  
  db.pure.stand <- rbindlist(lapply(names(list_data.inference), function(sp.sel){
    
    return(data.table(sp = sp.sel, list_data.inference[[sp.sel]]$data_age_height))
    
  }))
  
  return(db.pure.stand)
}