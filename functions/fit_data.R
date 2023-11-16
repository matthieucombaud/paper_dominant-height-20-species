fit_data <- function(
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
    parameters=list( # attention, I don't kno why, but the value here is important (ie is it not sufficient to specify the starting value below, I have to put it here!)
      "param_A0" = param_range[param_class == "A0", start_par],
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
  
  # do optimization
  # rel.tol <- 10^-10
  
  rel.tol <- 10^-10
  fit <- try(nlminb(
    start = param_range$start_par,
    objective=f$fn,
    f$gr,
    f$he,
    lower=param_range$lower_par,
    upper=param_range$upper_par,
    control=list(rel.tol=rel.tol)
  ))
  
    # manage error cases (do it first to exist the function in case of error)
    if(inherits(fit,what="try-error")){

      # rel.tol <- 10^-4
      # fit <- try(nlminb(
      #   start = param_range$start_par,
      #   objective=f$fn,
      #   f$gr,
      #   f$he,
      #   lower=param_range$lower_par,
      #   upper=param_range$upper_par,
      #   control=list(rel.tol=rel.tol)
      # ))
      # 
      # if(inherits(fit,what="try-error")){ # if no convergence with this convergence threshold, we return "no convergence"

        print(paste0("******************************* no convergence ***"))
        
        return(data.table(
          selection_metric=NA,
          param_alpha=toString(parameters_included[["alpha"]]),
          param_gamma=toString(parameters_included[["gamma"]]),
          rel.tol = NA,
          conv.info = "no convergence"
        ))
        
      # }
      
    }
    
    # manage no true convergence cases (do it second to exist the function in case of false convergence)
    if(fit$convergence != 0){ # attention, relative convergence is also convergence
  
      # rel.tol <- 10^-4
      # fit <- try(nlminb(
      #   start = param_range$start_par,
      #   objective=f$fn,
      #   f$gr,
      #   f$he,
      #   lower=param_range$lower_par,
      #   upper=param_range$upper_par,
      #   control=list(rel.tol=rel.tol)
      # ))
      # 
      # if(fit$convergence != 0){ # if no true convergence with this convergence threshold, we consider its an inference failure (include the case of singular convergence)

        print(paste0("******************************* ",fit$message,"***"))
        
        return(data.table(
          selection_metric = NA,
          param_alpha = toString(parameters_included[["alpha"]]),
          param_gamma = toString(parameters_included[["gamma"]]),
          rel.tol = NA,
          conv.info = fit$message
        ))
        
      # }
      
    }
    
  # get selection_metric (BIC)
      selection_metric <- 2*f$env$report()$nll+nrow(param_range)*log(nrow(data_age_height)) # bic # fit$objective is "-sum(log_likelihood)
      print(paste0("******************************* selection_metric = ",trunc(selection_metric)," ***"))
  
  # return
    return(data.table(
      selection_metric= selection_metric,
      param_alpha=toString(parameters_included[["alpha"]]),
      param_gamma=toString(parameters_included[["gamma"]]),
      rel.tol=rel.tol,
      conv.info = fit$message
    ))

  
}
