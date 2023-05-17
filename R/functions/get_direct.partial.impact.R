get_direct.partial.impact <- function(
  species_code,
  dir.model,
  dir.data.calibration,
  comparison.age, 
  quantile.focus,
  model.type,
  batch, 
  climate.only,
  restrict.to.significant
  ){
  
  # output
    # impact: 
  # attention
    # This functions does not work if there are no variable at all in the model
  
  print(species_code)
  
  # import model
  model <- readRDS(paste0(dir.model,"/sp",species_code,"_",model.type,".rds"))[[batch]]
  
  # get stands
  stands_species.selected <- model$stands
  
  # get parameters
  db_parameter <- model$db_parameter_normalized
  db_parameter$param_class <- factor(db_parameter$param_class,levels=c("intercept","alpha","gamma","beta","sigma","delta"))
  
  # import data
  list_data.explanatory <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species_code,".rds"))
  data_explanatory_complete_normalized <- list_data.explanatory$db_explanatory_normalized_complete
  data_information <- list_data.explanatory$db_information_variables
  data_age_height <- list_data.explanatory$data_age_height
  db_stand_number <- list_data.explanatory$stand_number
  db_normalization_constant <- list_data.explanatory$db_normalization_constant
  
  qualitative_variables <- c("ROCHE.Calc","soil_type","humus_type")
  

  # identify climate variables (with and without square)
  
  climate.variable <- unlist(sapply(c("Tmean", "cwb", "precipitation", "sgdd"), function(variable.climate.generic.selected){
    
    climate.variable.selected <- db_parameter[stringr::str_detect(param,variable.climate.generic.selected),param]
    return(climate.variable.selected)
    
  }))
  climate.variable <- unname(climate.variable)
  
  climate.variable_no.square <- unique(str_remove(climate.variable,"_square")) # required for next steps. Attention: also keep "climate.variable" with square
  
  # build a "mean" database (we could put everything at 0 since here I used normalized variables, but I write it like that in case we use non-normalized variables)
  
    # qualitative columns
    keep_col_qualitative <- unlist(sapply(qualitative_variables, function(qualitative_variables.selected){
      
      names(data_explanatory_complete_normalized)[str_detect(names(data_explanatory_complete_normalized),qualitative_variables.selected)]
      
    }))
    
    # quantitative columns
    keep_col_quantitative <- setdiff(names(data_explanatory_complete_normalized), c("stand", "climatic_year", keep_col_qualitative))
    
    # take mean (or median)
    
    data_explanatory_mean <- copy (data_explanatory_complete_normalized[,lapply(.SD,function(x){mean(x)}),.SDcols=keep_col_quantitative])
    
    if (length(keep_col_qualitative) > 0){
      
      data_explanatory_median <- copy(data_explanatory_complete_normalized[,lapply(.SD,function(x){median(x)}),.SDcols=keep_col_qualitative])
      
      data_explanatory_mean <- cbind(data_explanatory_mean,data_explanatory_median)
      
    }else{
      data_explanatory_mean <- data_explanatory_mean
    }
  
    # deal with square variables
    for(variable_selected in names(data_explanatory_mean)){
      
      if(str_detect(variable_selected,"square")){
        
        new.value <- as.numeric(data_explanatory_mean[, str_remove(variable_selected,"_square"), with = FALSE]^2)
        data_explanatory_mean[, (variable_selected) := lapply(.SD, function(x){new.value}), .SDcols = variable_selected]
        
      }
      
    }
    
    # a stand index and a is required for further computation
    data_explanatory_mean$stand <- 0 
    
    # add temporality (necessary to simulate dynamics)
    db_temporality <- data.table(stand = 0, climatic_year = c(0 : comparison.age))
    
    data_explanatory_mean <- merge(data_explanatory_mean, db_temporality, by = "stand")
    
    
  
  # compute impact for each variable  ----
  
    # list variables to test (without treating square variables separately from the linear corresponding variable)
    variable.to.test <- unique(db_parameter[, param]) 
    variable.to.test <- unique(str_remove(variable.to.test, "_square"))
    variable.to.test <- variable.to.test[!variable.to.test %in% c("A0", "C0", "intercept","sigma", "delta")]  
    
    # restrict to significant variable
    if(restrict.to.significant == TRUE){
      
      db_parameter_significant <- db_parameter[pvalue <= 0.05]
      
      variable.to.test <- unique(db_parameter_significant[, param]) 
      variable.to.test <- unique(str_remove(variable.to.test, "_square"))
      variable.to.test <- variable.to.test[!variable.to.test %in% c("A0", "C0", "intercept","sigma", "delta")]
    }
    
    # focus on climate variable
    if(climate.only == TRUE){
      variable.to.test <- variable.to.test[
        str_detect(variable.to.test, "Tmean") |
        str_detect(variable.to.test, "precipitation") |
        str_detect(variable.to.test, "cwb") |
        str_detect(variable.to.test, "sgdd")
        ]
    }
    
  db_variable_impact <- rbindlist(lapply(variable.to.test, function(variable_selected){
    
    print(variable_selected)    
    
    significant <- (db_parameter[str_detect(param, variable_selected), min(pvalue)] <= 0.05) # to see if significant or not
    
    variable_selected_square <- paste0(variable_selected,"_square")
    
    # define the simulation range and point number
    quantile.selected <- quantile(as.vector(data_explanatory_complete_normalized[, variable_selected, with = FALSE ])[[1]], probs = quantile.focus)
    
    Q_inf <- quantile.selected[1]
    Q_sup <- quantile.selected[2]
    
    nb_points <- 10
    
    value.sequence <- seq(Q_inf, Q_sup, length.out = nb_points)
    
    # do simulations for each value of the variable
    
    db_SDH_variable.selected <- rbindlist(lapply(value.sequence, function(value.selected){
      
      # prepare data
      data_explanatory_selected <- copy(data_explanatory_mean)
      data_explanatory_selected[, (variable_selected) := lapply(.SD, function(x){value.selected}), .SDcols = variable_selected]
      if(variable_selected_square %in% names(data_explanatory_mean)){
        data_explanatory_selected[, (variable_selected_square) := lapply(.SD, function(x){value.selected ^ 2}), .SDcols = variable_selected_square]
      }
      
      # simulate
      initial.height <- 1.3
      
      db_dynamics_selected <- compute_height(
        db_parameter = db_parameter,
        data_explanatory = data_explanatory_selected,
        data_age_height = data.table(
          stand = 0,
          year_observation = max(data_explanatory_selected$climatic_year),
          age = comparison.age,
          year_first = 1 # first year, not year 0
        ),
        initial.height = initial.height
      )
      SDH_selected <- db_dynamics_selected[age == comparison.age, height]
      
      
      # get absolute value
      value_abs.selected <- value.selected * db_normalization_constant[variable == variable_selected,sd] + db_normalization_constant[variable == variable_selected,mean]
      
      
      return(
        data.table(
          species = species_code,
          variable = variable_selected,
          value = value.selected,
          value_abs = value_abs.selected,
          SDH = SDH_selected,
          significant = significant
        )
      )
      
    }))
    
    return(db_SDH_variable.selected)
    
  }))
  
  
  # graph_variable.impact <- ggplot(
  #   data = db_variable_impact,
  #   aes(
  #     x = value,
  #     y = SDH, 
  #     color = as.factor(variable)
  #   )
  # ) + 
  #   geom_line()
  # 
  # 
  # # add parameter significance ----
  # db_parameter_temp <- copy(db_parameter)
  # db_parameter_temp[,variable := str_remove(param,"_square")]
  # db_parameter_temp <- db_parameter_temp[,.(pvalue_min = min(pvalue)), by = variable]
  # db_parameter_temp[,pvalue_class:=cut(pvalue_min,breaks=c(0,0.001,0.01,0.05,1))]
  # 
  # db_significance <- data.table(pvalue_class=levels(db_parameter_temp$pvalue_class),star=c("***","**","*","ns"))
  # db_parameter_temp <- merge(db_parameter_temp,db_significance,by="pvalue_class",all.x=T,sort=F) # all=T to deal with is.na(pvalue)
  # 
  # 
  # if(dim(db_variable_impact)[1] > 0){
  #   
  #   db_variable_impact <- merge(db_variable_impact, db_parameter_temp, by= "variable", all.x = T)
  #   db_variable_impact[, species_code := species_code]
  # 
  # }else{
  #   db_variable_impact <- NULL
  # }

  
  # return
  return(db_variable_impact)
  
}
