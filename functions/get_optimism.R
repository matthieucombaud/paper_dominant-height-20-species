get_optimism <- function(
  list_data.inference,
  list_model
  
){
  
  db.quality <- rbindlist(lapply(list.species, function(sp.sel){
    
    db.envi.norm.sel <- list_data.inference[[sp.sel]]$db_explanatory_normalized_reduced # environmental data
    db.stand.sel <- list_data.inference[[sp.sel]]$data_age_height # detail for all the stands of the species
    c.stand.sel <- unique(db.stand.sel$stand) # all the stand of the species
    
    list_model_sel <- list_model[[sp.sel]] # list of the 5 models for the selected species
    
    db.quality.sp <- rbindlist(lapply(1:5, function(batch.sel){
      
      db.parameter.sel <- list_model_sel[[batch.sel]]$best.model$param
      
      db.Hp_pred.sel <- compute_height_base(
        param_A0 = db.parameter.sel[param_class == "A0", estimate],
        param_C0 = db.parameter.sel[param_class == "C0", estimate],
        param_beta0 = db.parameter.sel[param_class == "beta0", estimate],
        param_alpha = db.parameter.sel[param_class == "alpha", estimate],
        param_gamma = db.parameter.sel[param_class == "gamma", estimate],
        names.param_alpha = db.parameter.sel[param_class == "alpha", param],
        names.param_gamma = db.parameter.sel[param_class == "gamma", param],
        data_explanatory = db.envi.norm.sel,
        initial.height = initial.height,
        data_age_height = db.stand.sel
      )
      
      # merge Hp_pred and Hp_obs
      db_Hp_comparison.sel <- merge(
        db.stand.sel[, .(stand, Hp_obs = hfinal, year_observation)],
        db.Hp_pred[, .(stand, Hp_pred = Hp, year_observation = year)],
        by = c("stand", "year_observation")
      )
      
      # separate the calibration and the validation stands
      stand.calib.sel <- list_model_sel[[batch.sel]]$stands
      stand.valid.sel <- setdiff(c.stand.sel, stand.calib.sel)
      
      db_Hp_comparison.sel.calib <- db_Hp_comparison.sel[stand %in% stand.calib.sel]
      db_Hp_comparison.sel.valid <- db_Hp_comparison.sel[stand %in% stand.valid.sel]
      
      db_quality.calib <- db_Hp_comparison.sel.calib[, .(
        rmse = sqrt(mean((Hp_obs - Hp_pred)^2)),
        rmspe =  sqrt(mean((100 *(Hp_obs - Hp_pred) / Hp_obs)^2)),
        bias = mean(Hp_pred - Hp_obs),
        bias.absolute = mean(abs(Hp_pred - Hp_obs))
      )]
      
      db_quality.valid <- db_Hp_comparison.sel.valid[, .(
        rmse = sqrt(mean((Hp_obs - Hp_pred)^2)),
        rmspe =  sqrt(mean((100 *(Hp_obs - Hp_pred) / Hp_obs)^2)),
        bias = mean(Hp_pred - Hp_obs),
        bias.absolute = mean(abs(Hp_pred - Hp_obs))
      )]
      
      # merge calibration and validation results
      db.quality <- cbind(
        db_quality.calib[, .(rmse.calib = rmse, bias.calib = bias)], 
        db_quality.valid[, .(rmse.valid = rmse, bias.valid = bias)]
      )
      db.quality[, optimism := max(0, rmse.valid - rmse.calib)]
      db.quality[, batch := batch.sel]
      db.quality[, sp := sp.sel]
      
      return(db.quality)
      
    }))
    
    return(db.quality.sp)
    
  }))
  
  db_optimism <- db.quality[, .(optimism = mean(optimism)), by = sp]
  
}