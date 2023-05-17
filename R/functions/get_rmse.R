get_rmse <- function(
  species_list,
  dir.model
){

  
  db_rmse <- rbindlist(lapply(species_list, function(species.selected){
    
    # import model
    model <- readRDS(paste0(dir.model,"/sp",species.selected,"_no_batch.rds"))[[1]]
    
    # compute rmse
    
    rmse <- sqrt(1/length(model$height_simulated)*sum((model$height_simulated - model$height_observed)^2))
    
    return(data.table(
      species_code = species.selected, 
      rmse = rmse
      ))
    
    
  }))

  
  
}
