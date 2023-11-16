compute_DH_dyn_pur <- function(
  list_species,
  list_parameter.SDH.model,
  list_calibration.SDH.model,
  db_mixed.stand.restricted,
  db.envi.restricted
){
  
  db_dynamics_pur_1 <- rbindlist(lapply(list_species, function(species.selected){
    
    print(species.selected)
    
    db_dynamics_pur_2 <- compute_DH_dyn_pur_species(
      species.selected = species.selected,
      db.parameter = list_parameter.SDH.model[[species.selected]],
      db_normalization_constant  = list_calibration.SDH.model[[species.selected]]$db_normalization_constant,
      tab.categorical.variable = list_calibration.SDH.model[[species.selected]]$tab_categorical_var,
      db.stand = db_mixed.stand.restricted[sp1 == species.selected | sp2 == species.selected],
      db.envi = db.envi.restricted,
      year_final = NULL,
      age_final = NULL,
      initial.height = 1.3
    )
    
    return(db_dynamics_pur_2)
    
  }))
  
  return(db_dynamics_pur_1)
  
}
