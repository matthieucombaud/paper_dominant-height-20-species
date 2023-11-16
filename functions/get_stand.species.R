get_stand.species <- function(
  species_code,
  dir.model
){
  
  # import model
  model_no_batch <- readRDS(paste0(dir.model,"/sp",species_code,"_no_batch.rds"))[[1]]
  
  # return list of stands
  return(model_no_batch$stands)
  
}
