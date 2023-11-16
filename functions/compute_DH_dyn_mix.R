compute_DH_dyn_mix <- function(
  list_species,
  list_parameter.SDH.model,
  list_calibration.SDH.model,
  db_mixed.stand,
  db.envi,
  list_model_ME_delta.DH_pair,
  year_final,
  age_final,
  initial.height
){
  
  # simulate dynamics in pure stand
  
  db_simulated_DH_pur <- rbindlist(lapply(c(list_species), function(species.selected){
    
    db_dynamics_pur <- compute_DH_dyn_pur_species(
      species.selected = species.selected,
      db.parameter = list_parameter.SDH.model[[species.selected]],
      db_normalization_constant  = list_calibration.SDH.model[[species.selected]]$db_normalization_constant,
      tab.categorical.variable = list_calibration.SDH.model[[species.selected]]$tab_categorical_var,
      db.stand = db_mixed.stand[sp1 == species.selected | sp2 == species.selected],
      db.envi = db.envi,
      year_final = year_final,
      age_final = age_final,
      initial.height = initial.height
    )
    
    return(db_dynamics_pur)
  }))
  
  # apply mixture effect
  
  db_ME_long_3 <- rbindlist(lapply(list_species, function(species.selected_1){
    
    print(species.selected_1)
    
    db_ME_long_2 <- rbindlist(lapply(list_species, function(species.selected_2){
      
      print(paste0("*** ", species.selected_2))
      
      # database of simulated DH in pure stand, for stand common to both selected species
      db_simulated_DH_pur_both.species <- merge(
        db_simulated_DH_pur[species == species.selected_1], 
        db_simulated_DH_pur[species == species.selected_2], 
        by = c("stand", "year", "age"),
        suffixes = c("_1", "_2")
      )
      
      if(nrow(db_simulated_DH_pur_both.species) == 0 | species.selected_1 == species.selected_2){
        return(NULL)
      }
      
      # ME for species_1
      db_ME_1_simulated <- copy(db_simulated_DH_pur_both.species)
      db_ME_1_simulated[, delta_DH_pur := (DH_p_2 - DH_p_1) / DH_p_1]
      
      model.selected_1 <- list_model_ME_delta.DH_pair[[species.selected_1]][[species.selected_2]] # in "list_model_ME_delta.DH_pair", the first level indicates the focal species and the second level indicates the companion species
      
      ME_1_prediction <- predict(
        object = model.selected_1,
        newdata = db_ME_1_simulated,
        interval = "confidence",
        level = 0.95
      )
      
      db_ME_1_simulated <- cbind(db_ME_1_simulated, ME_1_prediction)
      db_ME_1_simulated[, ":="(
        DH_m_1_fit = DH_p_1 * (1 + fit),
        DH_m_1_lwr = DH_p_1 * (1 + lwr),
        DH_m_1_upr = DH_p_1 * (1 + upr)
      )]
      
      # ME for species_2
      db_ME_2_simulated <- copy(db_simulated_DH_pur_both.species)
      db_ME_2_simulated[, delta_DH_pur := (DH_p_1 - DH_p_2) / DH_p_2]
      
      model.selected_2 <- list_model_ME_delta.DH_pair[[species.selected_2]][[species.selected_1]]
      
      ME_2_prediction <- predict(
        object = model.selected_2,
        newdata = db_ME_2_simulated,
        interval = "confidence",
        level = 0.95
      )
      
      db_ME_2_simulated <- cbind(db_ME_2_simulated, ME_2_prediction)
      db_ME_2_simulated[, ":="(
        DH_m_2_fit = DH_p_2 * (1 + fit),
        DH_m_2_lwr = DH_p_2 * (1 + lwr),
        DH_m_2_upr = DH_p_2 * (1 + upr)
      )]
      
      
      # merge results
      db_ME_simulated <- merge(
        db_ME_1_simulated[, -c("delta_DH_pur", "species_1", "species_2", "fit","lwr","upr")],
        db_ME_2_simulated[, -c("delta_DH_pur", "species_1", "species_2", "fit","lwr","upr")],
        by = c("stand","year","age","DH_p_1","DH_p_2")
      )
      
      
      # prepare for plotting
      db_ME_long <- melt(
        data = db_ME_simulated,
        id.vars = c("stand", "year", "age"),
        value.name = "DH"
      )
      
      db_ME_long[,
                 ":="(
                   type = str_split(variable, "_", simplify = TRUE)[, 2], # pure (p) or mixed (m)
                   species.index = str_split(variable, "_", simplify = TRUE)[, 3], # species 1 (1) or 2 (2)
                   uncertainty = str_split(variable, "_", simplify = TRUE)[, 4] # fit, upper range of the confience interval (upr), lower range of the confidence interval (lwr)
                 )
      ]
      
      db_ME_long[type == "p", uncertainty := "fit"] # because we do not plot include uncertainty of the pure stand dynamics
      
      # clarify variables
      
      db_ME_long <- merge(
        db_ME_long,
        data.table(species.index = c("1", "2"), species = c(species.selected_1, species.selected_2)),
        by = "species.index"
      )
      
      db_ME_long <- merge(
        db_ME_long, 
        db_species.name[, .(code, species.name = name)], 
        by.x = "species",
        by.y = "code"
      )
      
      db_ME_long[type == "p", type := "pure"]
      db_ME_long[type == "m", type := "mixed"]
      
      return(db_ME_long)
      
    }))
    
    return(db_ME_long_2)
    
  }))
  
  db_ME_long_3 <- unique(db_ME_long_3[, .(species, stand, year, age, DH, type, uncertainty, species.name)]) # "unique" to avoid double counting in species.index1 and species.index2 (which is also present in DH_m_1_xx and DH_m_2_xx) (computation is done twice for a given species-pair with the script above, due to permutation between species_selected_1 and species_selected_2)
  
  return(db_ME_long_3)
  
  
}
