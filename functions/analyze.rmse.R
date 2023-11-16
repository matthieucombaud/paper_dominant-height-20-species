analyze.rmse <- function(
  species_code,
  dir.model,
  dir.data.calibration
){
  
  print(species_code)
  
  # import
  
    # model output
    model_main <- readRDS(paste0(dir.model,"/sp",species_code,"_no_batch.rds"))
    model_annex <- readRDS(paste0(dir.model,"/sp",species_code,"_with_batches.rds"))
    
    # exclude batch without convergence
    # why: for some species, there is no convergence for some batch (no convergence, singular convergence, false convergence).
    # In such cases, we do not take the batch into account in the robustness analysis.
    
    keep.batch <- sapply(1:length(model_annex), function(batch){
      
      if(is.null(model_annex[[batch]]$output_optimisation)){
        return(FALSE)
      }else{
        return(TRUE)
      }
    })
    
    model_annex <- model_annex[keep.batch]
    
    # data
    data_inference <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species_code,".rds"))
    
  # compute calibration and validation rmse per batch
    
  db_rmse_model.submodel <- rbindlist(lapply(names(model_annex), function(name_model_annex.selected){
    
    model_annex.selected <- model_annex[[name_model_annex.selected]]
    db_parameter <- model_annex.selected$db_parameter_normalized
    
    # compute simulated height for all stands
    
    db_dynamics <- compute_height(
      db_parameter = db_parameter,
      data_explanatory = data_inference$db_explanatory_normalized_reduced,
      data_age_height = data_inference$data_age_height,
      initial.height = 1.3
    )
    
    db_height.final<- merge(
      data_inference$data_age_height[,.(stand, age, height.observed = hfinal)],
      db_dynamics[,.(stand, age, height.simulated = height)],
      by = c("stand", "age")
      )
    
    
    # compute rmse 
    
    stands.calibration <- model_annex.selected$stands
    stands.validation <- setdiff(model_main[[1]]$stands, stands.calibration)
    
    rmse.calibration <- sqrt(db_height.final[stand %in% stands.calibration, sum((height.observed - height.simulated)^2)] / nrow(db_height.final[stand %in% stands.calibration]))
    rmse.validation <- sqrt(db_height.final[stand %in% stands.validation, sum((height.observed - height.simulated)^2)] / nrow(db_height.final[stand %in% stands.validation]))
    
    # return
    
    return(
      data.table(
        species_code = species_code,
        batch = name_model_annex.selected,
        rmse.calibration = rmse.calibration,
        rmse.validation = rmse.validation
      )
    )
    
  }))
  
  # compute calibration rmse for the whole model ----
    
    # parameters
    db_parameter_model.main <- model_main[[1]]$db_parameter_normalized
    
    # compute simulated height for all stands
    
    db_dynamics_model.main <- compute_height(
      db_parameter = db_parameter_model.main,
      data_explanatory = data_inference$db_explanatory_normalized_reduced,
      data_age_height = data_inference$data_age_height,
      initial.height = 1.3
    )
    
    db_height.final_model.main <- merge(
      data_inference$data_age_height[,.(stand, age, height.observed = hfinal)],
      db_dynamics_model.main[,.(stand, age, height.simulated = height)],
      by = c("stand", "age")
    )
    
    # compute rmse 
    rmse_model.main <- sqrt(db_height.final_model.main[, sum((height.observed - height.simulated)^2)] / nrow(db_height.final_model.main))
    
  # return ----
    
  return(
    list(
      species_code = species_code,
      db_rmse_model.submodel = db_rmse_model.submodel,
      rmse_model.main = rmse_model.main
    )
  )  
    
}
