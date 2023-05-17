write_db_climate <- function(
  db_climate,
  saving.dir
  ){
  
  dir.create(saving.dir, recursive = TRUE)
  
  fwrite(db_climate, paste0(saving.dir,"/db_climate.monthly.csv"))
  
  return("saved")
  
}