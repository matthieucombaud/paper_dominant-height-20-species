  select_variables_optim <- function(list_parameters_to_test,list_data,vif.threshold,likelihood_file,likelihood_folder,break_gamma,initial.height,qualitative_variables){

  # compile files
  path_likelihood_file <- paste0(likelihood_folder,"/",likelihood_file,".cpp")
  compile(path_likelihood_file) # compile files
  dyn.load(dynlib(gsub("\\.cpp", "", path_likelihood_file))) # dynamically load the dll previously compiled (do it here so as to avoid redoing it for each run of the model)

  # get the name of all the variable as in the db, ie with the exact names of categorical variables (and not the generic name) ans also square variables
  variable_names <- setdiff(names(list_data$data_explanatory),c("stand","climatic_year"))
  
  # initialize a datatable to keep trace of aic
  table_selection_metric <- data.table(
    step=integer(),
    selection_metric=numeric(),
    param_alpha=character(),
    param_gamma=character(),
    param_beta=character(),
    rel.tol=character(),
    conv.info = character()
    )
  
  # initialize with the null model
  
  step_index <- 0 # step index
  
  list_parameters_selected <- list( # initialy, no variable is selected
    alpha = NULL,
    gamma = NULL,
    beta = NULL
  )
  
  output_model_null <- fit_data_optim(
    parameters_included = list_parameters_selected,
    likelihood_file = likelihood_file,
    list_data = list_data,
    initial.height = initial.height
    )
  
  list_models_already_tested <- c("alpha__gamma__beta_") # initialize list of models already considered
  selection_metric_best <- output_model_null$selection_metric # initialize aic at it value for the null model
  
  table_selection_metric <- rbind(table_selection_metric,data.table(step=step_index,output_model_null))
  
  # "while" loop to select variables

  continue_selection <- T # initialize stopping criteria
  
  while((length(list_parameters_to_test$alpha)>0 | length(list_parameters_to_test$gamma)>0 | length(list_parameters_to_test$beta)>0) & continue_selection==T){
    
    step_index <- step_index + 1
    print(paste0("begin of step ",step_index))
    
    # expand the names of categorical variables
    list_parameters_selected_expanded <- expand_categorical_variables(
      list_parameters_selected = list_parameters_selected,
      qualitative_variables = qualitative_variables,
      variable_names = variable_names
    )
    
    # list the variable combinations to test at this stage
    list_parameters_combination <- generate_list_parameters(
      list_parameters_selected_expanded = list_parameters_selected_expanded,
      list_parameters_to_test = list_parameters_to_test,
      qualitative_variables = qualitative_variables,
      variable_names = variable_names
    )
    
    # correct parameters list in case of too high correlation (in terms of VIF) 
      # attention : we need to include categorical variables in this check for the part "avoid the same variables" in alpha and gamma". However, we exclude them from the computation of correlations.
    list_parameters_combination_decorrelated <- decorrelate_list_param(
      list_parameters_combination = list_parameters_combination,
      list_parameters_selected_expanded = list_parameters_selected_expanded, # useful to identify the "new variable" for each model
      data_to_decorrelate = list_data$data_explanatory,
      vif.threshold = vif.threshold,
      break_gamma = break_gamma
      )
    
    # some lines to delete the models that have already been considered in the selection process
    list_models <- c(unlist(lapply(list_parameters_combination_decorrelated,function(x){ # prepare a code name per model
      paste0("alpha_",paste(x$alpha,collapse="_"),"_gamma_",paste(x$gamma,collapse="_"),"_beta_",paste(x$beta,collapse="_"))
    })))
    
    index_models_new <- !is.element(list_models,list_models_already_tested) # identify which model is new using this code name
    list_parameters_combination_decorrelated <- list_parameters_combination_decorrelated[index_models_new] # build the final list of parameters to test by identifying the corresponding model using the list format to declare explanatory variables
    
    if(length(list_parameters_combination_decorrelated)==0){ # case where there is no variable to test after the correlation filter
      
      continue_selection <- F
      
    }else{
      
      # update the list of models already tested
      list_models_new <- list_models[index_models_new]
      list_models_already_tested <- c(list_models_already_tested,list_models_new)
      
      print(
        paste0(length(index_models_new)-sum(index_models_new)," model(s) already tested in previous steps and reconsidered because of vif correction: no recomputation")
      )
      
      
      # do model inference and get associated BIC
      output_models <- rbindlist(lapply(list_parameters_combination_decorrelated,function(list_parameters_combination_decorrelated_selected){
        
        fit_data_optim(
          parameters_included = list_parameters_combination_decorrelated_selected,
          list_data=list_data,
          likelihood_file=likelihood_file,
          initial.height=initial.height
        )

      }
      ))
  
      
      table_selection_metric <- rbind(table_selection_metric,data.table(step=step_index,output_models)) # complete bic table
      
      if(selection_metric_best > min(output_models$selection_metric,na.rm = T)){ # if bic is improved by adding a variable, we update the model with this variable # na.rm =T to avoid the model that did not converge
        
        selection_metric_best <- min(output_models$selection_metric,na.rm = T)
        
        list_parameters_selected <- update_list_parameters_selected(output_models=output_models) # do it not on the expanded list but the list with the generic names, to update parameter_to_test
        
        list_parameters_to_test <- update_list_parameters_to_test(
          list_parameters_to_test = list_parameters_to_test, #  former list (attention: do not use list_parameters_to_test_corrected here, since we want the whole variable list!)
          list_parameters_selected = list_parameters_selected # updated list
        )
        # attention: when a variable has been selected without its square variable, the square variable will not be tested in the following steps, since the variable is not considered as "to_test"
  
        continue_selection<-T
  
        
      }else{
        
        continue_selection<-F
        
      }
    
    }
    
    print(paste0("end of step ",step_index))
    
    
  }
  
  # add a variable to identify "best in step" models
  table_selection_metric[,best_in_step:=ifelse(selection_metric==min(selection_metric,na.rm=T),T,F),by=step]
  
  # plot graph variable selection
  table_selection_metric <- cbind(x=c(1:dim(table_selection_metric)[1]),table_selection_metric)
  graph_selection <- ggplot(data=table_selection_metric)+geom_point(aes(x=x,y=selection_metric,col=as.factor(step)))+
    ggtitle("BIC")
  
  dyn.unload(dynlib(gsub("\\.cpp", "", path_likelihood_file))) # to avoid error in case of a new loading (eg in "best_model")
  
  return(list(
    parameters_selected=list_parameters_selected,
    table_selection_metric=table_selection_metric,
    table_selection_metric_best=table_selection_metric[best_in_step==T],
    stop_criteria=ifelse(length(c(list_parameters_to_test$alpha,list_parameters_to_test$gamma,list_parameters_to_test$beta))>0,"selection_metric stops decreasing","all variables added"),
    graph_selection=graph_selection
    ))
  
}
