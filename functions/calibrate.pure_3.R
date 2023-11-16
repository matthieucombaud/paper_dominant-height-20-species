calibrate.pure_3 <- function(
  # attention, the "_3" version is just a small adjustment to make computation for batches easier.
  # Do note delete the original version (calibrate.pure_2) (require to use this function + to do standard calibration)
  sp,
  batch,
  include_variable,
  output_folder,
  vif.threshold,
  n.core,
  range.limit,
  likelihood_file,
  likelihood_folder,
  list.data.batch,
  qualitative_variables,
  rel.tol,
  iter.max,
  eval.max,
  method
  ){

  list.data.batch.sp <- list.data.batch[[sp]]
  
  list_model_batch <- lapply(1 : length(list.data.batch.sp), function(batch.sel){
    
    # create dir to store output
    output_folder_batch <- paste0(output_folder, "/batch_analysis/sp", sp, "/batch_", batch.sel)
    dir.create(output_folder_batch, recursive = TRUE)
    
    list_data.inference_sel <- list(element_1 = list.data.batch.sp[[batch.sel]])
    names(list_data.inference_sel) <- sp
    
    model_batch_sel <- calibrate.pure_2(
      sp = sp,
      include_variable = include_variable,
      output_folder = output_folder_batch,
      vif.threshold = vif.threshold,
      n.core = n.core,
      range.limit = range.limit,
      likelihood_file = likelihood_file,
      likelihood_folder = likelihood_folder,
      list_data.inference = list_data.inference_sel,
      qualitative_variables = qualitative_variables,
      rel.tol = 10^-8,
      iter.max = 10000,
      eval.max = 10000,
      method = "nlminb.uncons"
    )
    
    return(model_batch_sel)
    
  })
  names(list_model_batch) <- 1 : length(list.data.batch.sp)
  
  return(list_model_batch)
  
}

