prepare_data_explanatory_2 <- function(
  list_species,
  list_parameter.SDH.model,
  list_calibration.SDH.model,
  db.stand,
  db.envi
){
  
  list_data_explanatory_formated <- lapply(list_species, function(species.selected){
    
    data_explanatory_formated <- prepare_data_explanatory(
      species.selected = species.selected,
      db.parameter = list_parameter.SDH.model[[species.selected]],
      db_normalization_constant  = list_calibration.SDH.model[[species.selected]]$db_normalization_constant,
      tab.categorical.variable = list_calibration.SDH.model[[species.selected]]$tab_categorical_var,
      db.stand = db.stand[sp1 == species.selected | sp2 == species.selected],
      db.envi = db.envi
    )
    
    return(data_explanatory_formated)
    
  })
  names(list_data_explanatory_formated) <- list_species
  
  return(list_data_explanatory_formated)
  
}

