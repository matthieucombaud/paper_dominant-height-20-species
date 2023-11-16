compute_DH_dyn_pur_species <- function(
  species.selected,
  db.parameter,
  db_normalization_constant,
  tab.categorical.variable,
  db.stand,
  db.envi,
  year_final = NULL, # if NULL, the year_observation in db_stand is used 
  age_final = NULL,# if NULL, the age in db_stand is used 
  initial.height
){
  
  
  # db of observed DH per stand ----
  db_DH_m <- rbind( 
    db.stand[sp1 == species.selected, .(stand, species.focal = sp1, DH_m.focal = h_sp1, age.focal = age_sp1, species.companion = sp2, DH_m.companion = h_sp2, age.companion = age_sp2, year_observation = year_observation)],
    db.stand[sp2 == species.selected, .(stand, species.focal = sp2, DH_m.focal = h_sp2, age.focal = age_sp2, species.companion = sp1, DH_m.companion = h_sp1, age.companion = age_sp1, year_observation = year_observation)]
  )
  
  # get variable names ----
  explanatory_variables <- db.parameter[!param_class %in% c("intercept", "sigma", "delta") & param != "intercept", param]
  explanatory_variables.no.square <- explanatory_variables[!str_detect(explanatory_variables, "square") & !str_detect(explanatory_variables, "categorical")]
  explanatory_variables.categorical_developped <- explanatory_variables[str_detect(explanatory_variables, "categorical")] # developed name of the categorical variable
  
  # get names of categorical variables (generic names, not developed names) ----
  
  names_categorical.variables <- sapply(tab.categorical.variable, function(tab.selected){
    names(dimnames(tab.selected))
  })
  
  names(tab.categorical.variable) <- names_categorical.variables
  

  if(length(explanatory_variables.categorical_developped) > 0){
    explanatory_variables.categorical <- unlist(lapply(names_categorical.variables, function(potential.categorical.variables.selected){ # reduced name of the categorical variable
      
      if(sum(str_detect(explanatory_variables.categorical_developped, potential.categorical.variables.selected)) > 0){
        
        return(potential.categorical.variables.selected)
        
      }else{
        
        return(NULL)
        
      }
      
    }))
  }else{
    explanatory_variables.categorical <- character()
  }
  
  # prepare the variable database ----
  
  # get non-standardized data
  # this code is adapted from the function "prepare_db_for_inference"
  
  db.envi.selected <- db.envi[stand %in% unique(db_DH_m$stand)]
  db.envi.selected.non.categorical <- db.envi.selected[, c("stand","climatic_year",explanatory_variables.no.square), with = FALSE]
  db.envi.selected.categorical <- db.envi.selected[, c("stand","climatic_year", explanatory_variables.categorical), with = FALSE]
  
  # no need to check for NA, because where excluded in the "filter.stand" function
  
  # standardize data
  for(explanatory_variables.no.square.selected in explanatory_variables.no.square){
    
    normalization.mean <- db_normalization_constant[variable == explanatory_variables.no.square.selected, mean]
    normalization.sd <- db_normalization_constant[variable == explanatory_variables.no.square.selected, sd]
    
    db.envi.selected.non.categorical[,
                                     (explanatory_variables.no.square.selected) := lapply(.SD, function(x){ (x-normalization.mean) / normalization.sd}),
                                     .SDcols = explanatory_variables.no.square.selected
    ]
    
  }
  
  # add square variables
  for(square.variable.selected in explanatory_variables[str_detect(explanatory_variables, "square")]){
    
    corresponding.variable <- str_remove(square.variable.selected, "_square")
    
    db.envi.selected.non.categorical[,
                                     (square.variable.selected) := lapply(.SD, function(x){x^2}),
                                     .SDcols = corresponding.variable
    ]
    
  }
  
  # reclassify categorical variables
  if(length(explanatory_variables.categorical) > 0){
    
    names_categorical_variable <- unlist(lapply(list_calibration.SDH.model[[species.selected]]$tab_categorical_var, function(x){names(dimnames(x))}))
    names(list_calibration.SDH.model[[species.selected]]$tab_categorical_var) <- names_categorical_variable
    
    
    for(variable_selected in explanatory_variables.categorical){ # not lapply, to modify db_ifn iteratively
      
      # attention, there are three levels to describe modalities of catagorical variables
      # we have modalities in the IFN database (in this target pipeline, correspond to "db.ifn.complete")
      # for the calibration, modalities either directly correspond to the modality in the IFN database or a "other" modality grouping modalities with too low observations : these are the modalities in "list_calibration.SDH.model[[species.selected]]$tab_categorical_var[['name of the variable']]
      # During the calibration process, one of the modalities (possibly "other") is taken as the reference modality and therefore does not appear in the parameter table
      # Regarding the mixed stand database, stand with modalities that are not observed in the IFN database are excluded in the function "filter.stand"
      # Here, we need to assign the value "other" to reflect the modalities entering the calibration process (keeping the reference modality, that must be distinguish from the "other" modality)
      
      # list modalities to be kept (ie those used in the calibration database)
      modalities <- names(list_calibration.SDH.model[[species.selected]]$tab_categorical_var[[variable_selected]])
      
      temp1 <- as.vector(db.envi.selected.categorical[, variable_selected, with = FALSE])[[1]] # vector of observed values
      temp1[!(temp1 %in% modalities)] <- "other" # attribute "other" to values not in the calibration datatable (it ensure the same classification of modalities as in the calibration datatable)
      
      db.envi.selected.categorical[, (variable_selected) := lapply(.SD, function(x){return(temp1)}), .SDcols = variable_selected] # we replace with "other", where relevant
      
      # reclassify
      temp2 <- reclassify_categorical_variables_no_deletion( # function "no_deletion" because at this stage we do not want to remove variables for which there is no variation.
        db = copy(db.envi.selected.categorical),
        variable = variable_selected,
        min_pct_per_group = 0, # min share of the sample in each group, set to 0 because we want the exact value (may create a slight error for the "other" category)
        min_abs_per_group = 0 # min stand number per group, set to 0 because we want the exact value (may create a slight error for the "other" category)
      )
      
      db.envi.selected.categorical <- copy(temp2$db)
      
    }
    
  }
  
  db.envi.selected.categorical <- db.envi.selected.categorical[, c("stand", "climatic_year", explanatory_variables.categorical_developped), with = FALSE]
  
  # final db.envi
  db.envi.final <- merge(db.envi.selected.categorical, db.envi.selected.non.categorical, by = c("stand", "climatic_year"))
  
  # prepare age data ----
  
  
  data_age_height <- db_DH_m[, .( # attention : differ from the method used to compute ME because need to set the same origin year to compare both species
    stand,
    year_observation = ifelse(is.null(year_final), year_observation, year_final),
    age = ifelse(is.null(age_final), age.focal, age_final)
  ), by = stand]
  
  data_age_height[, year_first := year_observation - age + 1]
  
  
  # compute height in pure stand ----
  db_DH.dynamics_p <- compute_height(
    db_parameter = db.parameter,
    data_explanatory = db.envi.final,
    initial.height = initial.height,
    data_age_height = data_age_height
  )
  
  # rename
  setnames(db_DH.dynamics_p, "height", "DH_p")
  
  # add species
  db_DH.dynamics_p[,species := species.selected]
  
  # return
  
  return(db_DH.dynamics_p[, -"height_asymptotic"])
  
}
