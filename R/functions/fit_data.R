fit_data <- function(parameters_included,list_data,likelihood_file,more=F,initial.height){
  
  print(paste(c("alpha: ", parameters_included$alpha),collapse=" "))
  print(paste(c("gamma: ",parameters_included$gamma),collapse=" "))
  print(paste(c("beta: ",parameters_included$beta),collapse=" "))
  
  # format data as matrix to easily export to cpp
  data_explanatory_formated <- format_data_explanatory(
    data = list_data$data_explanatory,
    param_alpha=  parameters_included[["alpha"]],
    param_gamma = parameters_included[["gamma"]],
    param_beta = parameters_included[["beta"]]
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
  
  
  # create optimization function
  
  list_info_model <- list(
    data=list(
      "height_observed"=list_data$data_age_height$hfinal,
      "initial_height"=initial.height,
      "nb_stands"=as.integer(dim(list_data$data_age_height)[1]),
      "nb_rows"=as.integer(length(data_explanatory_formated$stand)),
      "c_age"=as.integer(list_data$data_age_height$age),
      "m_alpha"=as.matrix(data_explanatory_formated$alpha),
      "m_gamma"=as.matrix(data_explanatory_formated$gamma),
      "m_beta"=as.matrix(data_explanatory_formated$beta),
      "unitary_vector"=as.integer(rep(1,times=length(data_explanatory_formated$stand))),
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
  
  
  # do optimization
  
  relative_tolerance <- 10^-7
  fit <- try_nlminb_tol(
    param_range = param_range,
    f=f,
    relative_tolerance=relative_tolerance
    )
  
  # manage error cases (do it first to exist the function in case of error)
  if(inherits(fit,what="try-error")){

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
  
  # manage no true convergence cases (do it second to exist the function in case of false convergence)
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
    
  # get selection_metric (BIC)

      selection_metric <- 2*fit$objective+dim(param_range)[1]*log(dim(list_data$data_age_height)[1]) # bic # fit$objective is "-sum(log_likelihood)
      
      print(paste0("******************************* selection_metric = ",trunc(selection_metric)," ***"))
  
  # return
    
  if(more==T){ #return more info (useful in the case we study only the best model)
    
    # get final parameters
    
      # attention: to compute sd, need to hahve a hessian definite positive. But because of rounding, the hessian is not symetric. To overcome that, I round the Hessian to a lower number of decimals, empirically 3.
      hessian_rounded <- round(f$he(f$env$last.par.best),digits=3)
      
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
    

    return(list(
      nll = f$env$report()$nll,
      selection_metric=selection_metric,
      parameters_included=parameters_included,
      parameter_report_final=parameter_report_final,
      height_simulated = f$env$report()$height_simulated,
      height_observed = f$env$report()$height_observed,
      output=fit,
      f=f
      ))
    
  }else{
    
    return(data.table(
      selection_metric=selection_metric,
      param_alpha=toString(parameters_included[["alpha"]]),
      param_gamma=toString(parameters_included[["gamma"]]),
      param_beta=toString(parameters_included[["beta"]]),
      rel.tol=as.character(relative_tolerance),
      conv.info = fit$message
    ))
    
  }
  
}
