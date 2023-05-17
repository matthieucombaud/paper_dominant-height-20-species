do_per_species <- function(
  sp,
  batch,
  nb_batch,
  include_variable=NULL,
  source_folder,
  output_folder,
  vif.threshold,
  likelihood_file,
  likelihood_folder,
  break_gamma,
  data_ifn_prepared_file,
  qualitative_variables,
  vary_macroparameter
  ){
  
  # required packages
  {
    require(stringr)
    require(ggplot2)
    require(gstat)
    require(data.table)
    require(readr)
    require(stringr)
    require(TMB)
    require(psych)
    require(foreach)
    require(doParallel)
  }

  # svg name
  svg_name <- paste0("sp",sp,"_",batch) # attention, must be at the end of the svg name to facilitate its use
  print(svg_name)
  
  # import data
  data_ifn_prepared <- read_rds(data_ifn_prepared_file)
  
  # select explanatory variables to include in the selection process
  explanatory_variables <- intersect(include_variable, data_ifn_prepared$explanatory_variables) # without square and with generic names for qualitative variables # intersect because some variables may be deleted from data_ifn_prepared$explanatory_variables because of not enough variability
  explanatory_variables_developped <- unname(unlist(lapply(explanatory_variables, function(variable.selected){ # with square and with developped names for qualitative variables
    
    return(
      names(data_ifn_prepared$db_explanatory_normalized_reduced)[
        str_detect(names(data_ifn_prepared$db_explanatory_normalized_reduced), variable.selected)
        ])
    
  })))
  
  
  print("final explanatory variables:")
  print(explanatory_variables_developped)
  
  # get the data
  db_explanatory <- data_ifn_prepared$db_explanatory_normalized_reduced[,c("stand", "climatic_year", explanatory_variables_developped), with = FALSE]
  data_age_height <- data_ifn_prepared$data_age_height
  initial.height <- data_ifn_prepared$initial.height
  stand_number <- data_ifn_prepared$stand_number
  
  # split in K samples
  
  if(batch=="no_batch"){
    K <- 1
  }else if(batch=="with_batches"){
    K <- nb_batch
  }
  
  db_allocation<-data.table(stand=unique(data_age_height$stand),group=integer())
  stand_pool<-db_allocation$stand
  
  nb_stands<-length(stand_pool)
  size_group<-nb_stands%/%K
  rest<-nb_stands%%K
  
  group_CV<-1:K
  
  for(k in group_CV){
    
    if(rest>0){ # allocate the additional stands
      size<-size_group+1
      rest<-rest-1
    }else{
      size<-size_group
    }
    
    set.seed(123)
    stand_selected<-sample(stand_pool,size=size,replace=F)
    stand_pool<-setdiff(stand_pool,stand_selected) # update the list of non-affected stands
    db_allocation[stand%in%stand_selected,group:=k]
    
  }
  
  print("grouping ok")
  
  # run calibration and validation for each sample

  no_cores <- K
  cl <- makeCluster(no_cores,outfile=paste0(output_folder,"/debug_",svg_name,".txt"))  
  registerDoParallel(cl) 
  
  required_functions <- c(
    "add_parameter_square_to_list",
    "add_parameter_to_list",
    "analyse_best_model",
    "analyse_best_model_optim",
    "compute_vif",
    "decorrelate_list_param",
    "decorrelate_param",
    "do_calibration",
    "expand_categorical_variables",
    "fit_data",
    "fit_data_optim",
    "format_data_explanatory",
    "generate_list_parameters",
    "iterate_remove_colinear_variables",
    "prepare_param_range",
    "remove_colinear_variables",
    "select_variables",
    "select_variables_optim",
    "try_nlminb_tol",
    "try_optim_tol",
    "update_list_parameters_selected",
    "update_list_parameters_to_test"
    )
  
  required_packages<-c("TMB","data.table","stringr","ggplot2","psych")
  
  output <- foreach(group_CV_k=group_CV,.packages=required_packages,.export=required_functions) %dopar% {
    
    print(group_CV_k)
    
    # identify stands for validation or for calibration
    if(length(group_CV)>1){ # calibration is done on group different from group_CV_k
      
      stand_selected_k <- db_allocation[!group%in%group_CV_k,stand] # we exclude group k
      db_explanatory_calibration <- db_explanatory[stand%in%stand_selected_k]
      data_age_height_calibration <- data_age_height[stand%in%stand_selected_k]
      
      # additional check to see if because of the grouping a categorical variables is not varying anymore in the calibration sample.
      # If there is such a variable, we remove it from the process.
      
      modalities_quantitative_variables <- names(db_explanatory)[str_detect(names(db_explanatory),"categorical")]
      count_unique <- sapply(modalities_quantitative_variables,function(x){
        
        dim(unique(db_explanatory[,..x]))[1]
        
      })
      
      non_varying_var <- count_unique[count_unique==1]
      
      if(length(non_varying_var)>0){
        
        explanatory_variables_to_exclude <- explanatory_variables[str_detect(non_varying_var,explanatory_variables)]
        
        explanatory_variables_batch <- setdiff(explanatory_variables,explanatory_variables_to_exclude)
        
        print(paste0("Exclusion of variable(s) ",explanatory_variables_to_exclude," for batch n°",group_CV_k," because not enough variation."))
        
        
      }else{
        explanatory_variables_batch <- explanatory_variables
      }

    }else{ # calibration is done on all stands
      
      stand_selected_k <- db_allocation[,stand] # to provide it as an output
      db_explanatory_calibration <- db_explanatory
      data_age_height_calibration <- data_age_height
      explanatory_variables_batch <- explanatory_variables # to be consistent with the case "with batches"

    }

    cat(dput(group_CV_k), file = paste0(output_folder,"/before_calib_",svg_name,"_batch_", group_CV_k, ".txt"))
    
    # do calibration ----
    calibration_k <- do_calibration(
      explanatory_variables = explanatory_variables_batch,
      db_explanatory = db_explanatory_calibration,
      data_age_height = data_age_height_calibration,
      vif.threshold = vif.threshold,
      likelihood_file = likelihood_file,
      likelihood_folder = likelihood_folder,
      break_gamma = break_gamma,
      initial.height = initial.height,
      qualitative_variables = qualitative_variables,
      vary_macroparameter = vary_macroparameter
    )
    
    cat(dput(group_CV_k), file = paste0(output_folder,"/after_calib_",svg_name,"_batch_", group_CV_k, ".txt"))
    
    print("ok calibration")
    
    # prepare output
    output_k <- list(
      sp=sp,
      vary_macroparameter = vary_macroparameter,
      variable_selection=calibration_k$variable_selection$table_selection_metric,
      variable_selection_stop_criteria=calibration_k$variable_selection$stop_criteria,
      db_parameter_normalized=calibration_k$best_model$parameter_report_final,
      selection_metric = calibration_k$best_model$selection_metric,
      output_optimisation = calibration_k$best_model$output,
      height_simulated = calibration_k$best_model$height_simulated,
      height_observed = calibration_k$best_model$height_observed,
      stand_number=stand_number,
      include_variable=include_variable,
      stands=stand_selected_k,
      vif.threshold=vif.threshold,
      likelihood_file=likelihood_file,
      break_gamma=break_gamma,
      initial.height=initial.height
    )
    
    print(paste0("end for batch ",group_CV_k))
    
    return(output_k)
    
  }
  stopCluster(cl)
  
  names(output)<-paste0("batch_",group_CV)
  
  write_rds(output, paste0(output_folder,"/",svg_name,".rds"))
  
  return(output)
  
}

