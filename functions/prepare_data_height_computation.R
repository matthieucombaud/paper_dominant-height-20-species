prepare_data_height_computation <- function(
  db.parameter,
  db.normalization,
  cat.var_names.gen, # generic names of categorical variables to consider
  db.envi,
  c.stand
){
  
  # get variable names ----
    expl.var <- unique(db.parameter[param_class %in% c("alpha", "gamma"), param])
    expl.var_non.cat_non.square <- expl.var[!str_detect(expl.var, "square") & !str_detect(expl.var, "categorical")]
    expl.var_non.cat_square <- expl.var[str_detect(expl.var, "square")  & !str_detect(expl.var, "categorical")]
    expl.var_cat_dvp <- expl.var[str_detect(expl.var, "categorical")] # developed name of the categorical variable
  
  # prepare non categorical variables ----
  
    db.envi.non.cat <- db.envi[stand %in% c.stand, c("stand", "climatic_year", expl.var_non.cat_non.square), with = FALSE]
  
    ## standardize non categorical & non square variables ----
      for(var.selected in expl.var_non.cat_non.square){
        
        normalization.mean <- db.normalization[variable == var.selected, mean]
        normalization.sd <- db.normalization[variable == var.selected, sd]
        
        db.envi.non.cat[, (var.selected) := lapply(.SD, function(x){ (x-normalization.mean) / normalization.sd}),.SDcols = var.selected]
        
      }
    
    ## add square variables ----
      for(var.selected in expl.var_non.cat_square){
        
        var.non.square.selected <- str_remove(var.selected, "_square")
  
        db.envi.non.cat[, (var.selected) := lapply(.SD, function(x){x^2}),.SDcols = var.non.square.selected]
        
      }
  
  # Prepare categorical variables ----
    
    ## Identify generic names of the categorical variables used in the model

    expl.var_cat_gen <- unique(unlist(lapply(cat.var_names.gen, function(var.selected){
      
      if(sum(str_detect(expl.var_cat_dvp, var.selected)) > 0){
        return(var.selected)
      }else{
        return(NULL)
      }
      
    })))

    db.envi.cat <- db.envi[stand %in% c.stand, c("stand", "climatic_year", expl.var_cat_gen), with = FALSE]
  
    ## Recategorize for each variable
    
    if(length(expl.var_cat_gen) > 0){
      
      for(var.selected in expl.var_cat_gen){ # not lapply, to modify db_ifn iteratively
        
        # get modalities used in the model, for the considered variable
        modalities <- expl.var_cat_dvp[str_detect(expl.var_cat_dvp, var.selected)]
        
        # put db in the wide format for the considered variable
        temp.db.envi.cat <- reclassify_categorical_variables_no_deletion( # function "no_deletion" because at this stage we do not want to remove variables for which there is no variation.
          db = copy(db.envi.cat),
          variable = var.selected,
          min_pct_per_group = 0, # min share of the sample in each group, set to 0 because we want the exact value (may create a slight error for the "other" category)
          min_abs_per_group = 0 # min stand number per group, set to 0 because we want the exact value (may create a slight error for the "other" category)
        )
        db.envi.cat <- temp.db.envi.cat$db
        
        # add column for missing modalities
        missing.modalities <- setdiff(modalities, names(db.envi.cat)) # can be non-empty depending on the stands considered
        if(length(missing.modalities) > 0){
          
          for(missing.modalities.selected in missing.modalities){
            
            db.envi.cat <- cbind(db.envi.cat, new.var = 0)
            setnames(db.envi.cat, "new.var", missing.modalities.selected)
            
          }
          
        }
        
        db.envi.cat <- db.envi.cat[, setdiff(names(db.envi.cat), var.selected), with = FALSE] # remove the column with the generic variable
        
      }
      
    }
    
    # merge non-categorical and categorical variables ----
    db.envi.final <- merge(db.envi.non.cat, db.envi.cat, by = c("stand", "climatic_year"))
    
    return(db.envi.final)
  
}
