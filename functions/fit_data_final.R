fit_data_final <- function(
  parameters_included,
  db_explanatory,
  data_age_height,
  range.limit,
  likelihood_file,
  initial.height
  ){
  
  print(paste(c("alpha: ", parameters_included$alpha),collapse=" "))
  print(paste(c("gamma: ",parameters_included$gamma),collapse=" "))

  # format data as matrix to easily export to cpp
  db_explanatory_formated <- list(
    alpha = as.matrix(db_explanatory)[, param_alpha = parameters_included$alpha, drop = FALSE], # if param_alpha==NULL gives an empty matrix, but not an issue
    gamma = as.matrix(db_explanatory)[, param_gamma = parameters_included$gamma, drop = FALSE] # if param_alpha==NULL gives an empty matrix, but not an issue
  )
  
  # provide range and initial values for each parameter
  param_range <- prepare_param_range(   # attention: in the output, same order as in list_info_model_parameters required, to be sure to do the right thing when refreing to the "start values" in nlminb
    range.limit = range.limit,
    parameters = parameters_included
  ) # does not contain macroparam # attention: initial values most allow to get a no-zero increment, else trapped in a fixed point
  
  
  # create optimization function
  list_info_model <- list(
    data=list(
      "initial_height"=initial.height,
      "height_observed"=data_age_height$hfinal,
      "c_age"=as.integer(data_age_height$age),
      "nb_stands"=as.integer(nrow(data_age_height)),
      "nb_rows"=nrow(db_explanatory_formated$alpha),
      "m_alpha"=as.matrix(db_explanatory_formated$alpha),
      "m_gamma"=as.matrix(db_explanatory_formated$gamma)
    ),
    parameters=list(
      # attention, I don't kno why, but the value here is important (ie is it not sufficient to specify the starting value below, I have to put it here!)
      # attention to the order to match param_range
      "param_A0" = param_range[param_class == "A0" , start_par],
      "param_C0" = param_range[param_class == "C0", start_par],
      "param_beta0" = param_range[param_class == "beta0", start_par],
      "param_alpha" = param_range[param_class == "alpha", start_par],
      "param_gamma" = param_range[param_class == "gamma", start_par],
      "param_sigma" = param_range[param_class == "sigma", start_par],
      "param_delta" = param_range[param_class == "delta", start_par]
    ),
    hessian=F,
    DLL=likelihood_file
  )
  
  f <- MakeADFun(
    silent = T,
    data = list_info_model$data,
    parameters = list_info_model$parameters,
    hessian = list_info_model$hessian,
    DLL = list_info_model$DLL
  )
  
  # fit with nlminb
    fit.nlminb <- try(nlminb(
      start = param_range$start_par,
      objective=f$fn,
      f$gr,
      f$he,
      lower=param_range$lower_par,
      upper=param_range$upper_par
    ))
    
  # get selection_metric (BIC)
      selection_metric_nlminb <- 2*f$env$report()$nll + nrow(param_range)*log(nrow(data_age_height)) # bic # fit$objective is "-sum(log_likelihood)
      print(paste0("selection metric nlminb: ", selection_metric_nlminb))
      
  # param value and sde and p-value (but attention, not linear model...)
 
    # check parameter order between param_class (A0, A0, bet0, alpha, gamma, sigma, delta). Within param classes, there is no reason to mix up.
    if(!identical(str_remove(names(f$par), "param_"), param_range$param_class)){stop("pb parameter order")}
    
    # get report
    parameter_report <- sdreport(
      obj=f,
      par.fixed=f$env$last.par.best,
      # hessian.fixed = hessian_rounded, # round to avoid slight matrix asymetry (would prevent to have a "positive definite matrix")
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
  
    # format report
    parameter_report_final <- cbind(param_range,estimate=unlist(parameter_estimate),sde=unlist(parameter_sde),pvalue=unlist(parameter_pvalue))
    
    # return
    return(list(
      param.nlminb = parameter_report_final,
      selection_metric_nlminb = selection_metric_nlminb,
      fit.nlminb = fit.nlminb,
      f = f # TMB object
      ))
    
}
