# Description
# Get stand used for calibration, from a list of objects used for calibration, for each species
# 
# Argument
# list_calib.model.pure: list of objecti used for calibration

# Value
# db of stand used for calibration of SDH model in pure stand and corresponding species

get_stand.calibration.pure <- function(
  list_data.inference
){
  
  list.sp <- sapply(list_data.inference, function(data.sel){
    
    return(data.sel$sp)
    
  })
  
  
  list.db.pure.stand <- lapply(list_data.inference, function(data.sel){
    
    return(data.table(species = data.sel$sp, data.sel$data_age_height))

  })
  names(list.sp) <- list.sp
  
  return(list.db.pure.stand)
  
}
