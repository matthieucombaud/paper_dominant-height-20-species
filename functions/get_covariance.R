# idea of the function: we run the optimization algorithm using the selected parameter set, but asking for the Hessian matrix (the Hessian matrix is not returned in the rest of the code)

get_covariance <- function(
  species_list, 
  dir.model,
  initial.height,
  dir.data.calibration
){
  
  
  list_mat_cov <- lapply(species_list, function(species.selected){
    
    print(species.selected)
    
    model <- readRDS(paste0(dir.model,"/sp",species.selected,"_no_batch.rds"))$batch_1
    
    
    # included variables
    
    include_variable_non_climatic <- c(
      
      "ROCHE.Calc",
      "soil_type",
      "humus_type",
      "sgdd_1_12",
      "pH",
      "CN",
      "P2O5",
      "soil_depth",
      "affroc_perc",
      "cailloux_perc",
      "slope",
      "expo_NS",
      "expo_EW",
      "SWHC",
      "radiation_annual"
    )
    
    include_variable_climatic <- c(
      "Tmean_9_11","Tmean_12_2","Tmean_3_5","Tmean_6_8",
      "Tmean_9_2","Tmean_3_8",
      "Tmean_9_8",
      "cwb_9_11","cwb_12_2","cwb_3_5","cwb_6_8",
      "cwb_9_2","cwb_3_8",
      "cwb_9_8", 
      "precipitation_9_11","precipitation_12_2","precipitation_3_5","precipitation_6_8",
      "precipitation_9_2","precipitation_3_8",
      "precipitation_9_8"
    )
    
    include_variable <- c(include_variable_non_climatic,include_variable_climatic)
    
    # likelihood
    likelihood_folder = "cpp"
    likelihood_file = "likelihood_final_beta"
    path_likelihood_file <- paste0(likelihood_folder,"/",likelihood_file,".cpp")
    compile(path_likelihood_file) # compile files
    dyn.load(dynlib(gsub("\\.cpp", "", path_likelihood_file))) # dynamically load the dll previously compiled (do it here so as to avoid redoing it for each run of the model)
    
    
    # param
    parameters_included <- list(
      alpha = model$db_parameter_normalized[param_class == "alpha", param], 
      beta = NULL, 
      gamma = model$db_parameter_normalized[param_class == "gamma", param]
    )
    
    # provide range and initial values for each parameter
    param_range <- prepare_param_range(   # attention: in the output, same order as in list_info_model_parameters required, to be sure to do the right thing when refreing to the "start values" in nlminb
      A0 = c(0.5,0,50),
      C0 = c(0.1,0,50),
      alpha = c(0.1,-100,100),
      gamma = c(0.1,-100,100),
      beta = c(0.1,-100,100),
      sigma = c(0.5,0.1,20),
      delta = c(0,-10,10),
      parameters = parameters_included
    ) # does not contain macroparam # attention: initial values most allow to get a no-zero increment, else trapped in a fixed point
    
    
    # data
    
    # import data
    data_ifn_prepared_file = paste0(dir.data.calibration,"/db_for_inference_sp",species.selected,".rds")
    data_ifn_prepared <- read_rds(data_ifn_prepared_file)
    
    # choose explanatory variables to include
    explanatory_variables <- intersect(include_variable, data_ifn_prepared$explanatory_variables) # without square and with generic names for qualitative variables # intersect because some variables may be deleted from data_ifn_prepared$explanatory_variables because of not enough variability
    explanatory_variables_developped <- unname(unlist(lapply(explanatory_variables, function(variable.selected){ # with square and with developped names for qualitative variables
      
      return(
        names(data_ifn_prepared$db_explanatory_normalized_reduced)[
          str_detect(names(data_ifn_prepared$db_explanatory_normalized_reduced), variable.selected)
        ])
      
    })))
    
    # get the data
    db_explanatory <- data_ifn_prepared$db_explanatory_normalized_reduced[,c("stand", "climatic_year", explanatory_variables_developped), with = FALSE]
    data_age_height <- data_ifn_prepared$data_age_height
    
    list_data <- list(
      data_explanatory = db_explanatory,
      data_age_height = data_age_height
    )
    
    # format data as matrix to easily export to cpp
    data_explanatory_formated <- format_data_explanatory(
      data = list_data$data_explanatory,
      param_alpha=  parameters_included[["alpha"]],
      param_gamma = parameters_included[["gamma"]],
      param_beta = parameters_included[["beta"]]
    )
    
    # build the model
    list_info_model <- list(
      data=list(
        "height_observed"=list_data$data_age_height$hfinal,
        "initial_height"=initial.height,
        "nb_stands"=as.integer(nrow(list_data$data_age_height)),
        "nb_rows"=as.integer(nrow(data_explanatory_formated$alpha)),
        "c_age"=as.integer(list_data$data_age_height$age),
        "m_alpha"=as.matrix(data_explanatory_formated$alpha),
        "m_gamma"=as.matrix(data_explanatory_formated$gamma),
        "m_beta"=as.matrix(data_explanatory_formated$beta),
        "unitary_vector"=as.integer(rep(1,times=nrow(data_explanatory_formated$alpha))),
        "nb_intercept" = 2
      ),
      parameters=list( # attention, I don't kno why, but the value here is important (ie is it not sufficient to specify the starting value below, I have to put it here!)
        "param_interest"=param_range[param_class%in%c("intercept","alpha","gamma","beta")]$start_par,
        "param_error"=param_range[param_class%in%c("sigma","delta")]$start_par
      ),
      hessian=T,
      DLL=likelihood_file
    )
    
    f <- MakeADFun(
      silent = T,
      data = list_info_model$data,
      parameters = list_info_model$parameters,
      hessian = list_info_model$hessian,
      DLL = list_info_model$DLL
    )
    
    # infer parameters
    
    # do first optimization
    relative_tolerance <- 10^-7
    fit <- try_nlminb_tol(
      param_range = param_range,
      f=f,
      relative_tolerance=relative_tolerance
    )
    
    # manage error cases (do it first to exit the function in case of error)
    if(inherits(fit ,what="try-error")){
      
      relative_tolerance <- 10^-4
      fit <- try_nlminb_tol(
        param_range = param_range,
        f=f,
        relative_tolerance=relative_tolerance
      )
      
      if( inherits(fit,what="try-error")){ # if no convergence with this convergence threshold, we return "no convergence"
        
        print(paste0("******************************* no convergence ***"))
        
        return(data.table(
          selection_metric=NA,
          param_alpha=toString(parameters_included[["alpha"]]),
          param_gamma=toString(parameters_included[["gamma"]]),
          param_beta = toString(parameters_included[["beta"]]),
          rel.tol = relative_tolerance,
          conv.info = "no convergence"
        ))
        
      }
      
    }
    
    # manage no true convergence cases (do it second to exit the function in case of false convergence)
    if(fit$convergence != 0){ # attention, relative convergence is also convergence
      
      relative_tolerance <- 10^-4
      fit <- try_nlminb_tol(
        param_range=param_range,
        f=f,
        relative_tolerance=relative_tolerance
      )
      
      if( fit$convergence != 0 ){ # if no true convergence with this convergence threshold, we consider its an inference failure (include the case of singular convergence)
        
        print(paste0("******************************* ",fit$message,"***"))
        
        return(data.table(
          selection_metric = NA,
          param_alpha = toString(parameters_included[["alpha"]]),
          param_gamma = toString(parameters_included[["gamma"]]),
          param_beta = toString(parameters_included[["beta"]]),
          rel.tol = relative_tolerance,
          conv.info = fit$message
        ))
        
      }
      
    }
    
    # get the variance-covariance matrix
    hessian_rounded <- round(f$he(f$env$last.par.best),digits=3)
    mat_cov <- solve(hessian_rounded)
    
    # to be sure that this is the good matri, some cheks thereafter
    
      # get param report
      parameter_report <- sdreport(
        obj=f,
        par.fixed=f$env$last.par.best,
        hessian.fixed = hessian_rounded, # round to avoid slight matrix asymetry (would prevent to have a "positive definite matrix")
        getJointPrecision = F,
        bias.correct = F,
        bias.correct.control = list(sd = FALSE, split = NULL, nsplit = NULL),
        ignore.parm.uncertainty = FALSE,
        getReportCovariance = TRUE,
        skip.delta.method = FALSE
      )
      
      parameter_estimate <- as.list(parameter_report,what="Estimate")
      parameter_sde <- as.list(parameter_report,what="Std. Error")
      parameter_pvalue <- as.list(parameter_report,what="Pr",p.value=T)

      parameter_report_final <- cbind(param_range,estimate=unlist(parameter_estimate),sde=unlist(parameter_sde),pvalue=unlist(parameter_pvalue))
      
      # check that parameter estimates and sde are the same as the one determined at the end of the variable selection process
      
      db_parameter_compare <- merge(
        parameter_report_final[, .(param_class, param, estimate, sde)],
        model$db_parameter_normalized[, .(param_class, param, estimate, sde)], 
        by = c("param_class", "param"), 
        suffixes = c("_new", "_old")
      )
      
      db_parameter_compare[, ":="(gap_estimate = (estimate_new - estimate_old) / estimate_old, gap_sde = (sde_new - sde_old) / sde_old)]
      
      values <- unique(c(db_parameter_compare$gap_estimate, db_parameter_compare$gap_sde))
      
      if(max(values, na.rm = TRUE) > 0.001){
        print(species.selected)
        stop("not the same output")
      }
      
      # check that sde estimate from the algo correspond to those directly obtain by the inversion of the Hessian matrix
      
      sde.direct <- numeric()
      
      for(index in 1:nrow(parameter_report_final)){
        
        sde.direct <- c(sde.direct, sqrt(mat_cov[index, index]))
        
      }
      
      sde.diff <- (sde.direct - parameter_report_final$sde) / parameter_report_final$sde
      
      if(max(sde.diff, na.rm = TRUE) > 0.001){
        print(species.selected)
        stop("not the same output")
      }
      
    
    # return
    
    return(mat_cov)
    
  })
  
  
  names(list_mat_cov) <- species_list
  
  return(list_mat_cov)
        
}
