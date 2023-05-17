describe_species <- function(
  species_list,
  db_species.name,
  dir.data.calibration
){
  
  
  summary_age_height <- rbindlist(lapply(species_list, function(species.selected){
    
    list_data.explanatory <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species.selected,".rds"))
    data_explanatory_complete_normalized <- list_data.explanatory$db_explanatory_normalized_complete
    # data_information <- list_data.explanatory$db_information_variables
    data_age_height <- list_data.explanatory$data_age_height
    db_stand_number <- list_data.explanatory$stand_number
    db_normalization_constant <- list_data.explanatory$db_normalization_constant
    
    
    summary_age_height_species.selected <- data.table(
      species = species.selected,
      stand.number = db_stand_number["length_IFN_data_final"],
      min.age = min(data_age_height$age),
      median.age = median(data_age_height$age),
      mean.age = mean(data_age_height$age),
      max.age = max(data_age_height$age),
      sd.age = sd(data_age_height$age),
      min.height = min(data_age_height$hfinal),
      median.height = median(data_age_height$hfinal),
      mean.height = mean(data_age_height$hfinal),
      max.height = max(data_age_height$hfinal),
      sd.height = sd(data_age_height$hfinal)
      )

    return(summary_age_height_species.selected)
    
  }))
  
  
  summary_age_height_species.selected <- merge(summary_age_height, db_species.name, by.x = "species", by.y = "code")

  return(summary_age_height_species.selected)
  
  
}