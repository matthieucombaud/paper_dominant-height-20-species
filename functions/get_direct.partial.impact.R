get_direct.partial.impact <- function(
  list.species,
  list_data.inference,
  list_model,
  comparison.age, 
  quantile.focus,
  qualitative_variables,
  initial.height
  ){
  

  list.db_variable_impact <- lapply(list.species, function(sp.sel){
    
      print(sp.sel)
    
    # get parameters
      model <- list_model[[sp.sel]]
      db_parameter <- model$best.model$param
      db_parameter$param_class <- factor(db_parameter$param_class,levels=c("A0","C0","beta0", "alpha", "gamma", "sigma","delta"))
      
    # get explanatory variables
      c.variable <- db_parameter[param_class %in% c("alpha", "gamma"), param]
      c.variable.quali <- unlist(sapply(qualitative_variables, function(qualitative_variables.sel){
        return(c.variable[str_detect(c.variable, qualitative_variables.sel)])
      }))
      c.variable.quanti <- setdiff(c.variable, c.variable.quali)
      c.variable.climate <- c.variable.quanti[str_detect(c.variable.quanti, "Tmean") | str_detect(c.variable.quanti, "precipitation") | str_detect(c.variable.quanti, "cwb") | str_detect(c.variable.quanti, "sgdd")]
      
        
    # get calibration data
      data.calibration <- list_data.inference[[sp.sel]]
      data_explanatory_complete_normalized <- data.calibration$db_explanatory_normalized_complete[, c("stand", "climatic_year", c.variable), with = FALSE]
      data_age_height <- data.calibration$data_age_height
      db_normalization_constant <- data.calibration$db_normalization_constant
      
    # get stands
      stands_species.selected <- model$stands
    
    # build a "mean" database (attention: must include the non climatic variables, including the categorical variables)
      data_explanatory_mean <- data_explanatory_complete_normalized[, lapply(.SD, function(x){mean(x)}), .SDcols = c.variable.quanti]
      
      # not that the mean of the square climate variable is not used per se, since we redefine the value of the square climate var as the square of the quantile under scrutiny (even if the square of the quantile is not the quantile of the square!)
      
      # add categorical variables (ie put 1 for the mode, 0 for the other modalities)
      if(length(c.variable.quali) > 0){
        
        # initialize at zero
        data_explanatory_median <- data_explanatory_complete_normalized[, lapply(.SD, function(x){0}), .SDcols = c.variable.quali]
        
        # find the mode per category, and put its value at 1
        for(quali.var.sel in qualitative_variables){ # choose a generic variable
          
          if(sum(str_detect(c.variable.quali, quali.var.sel)) > 0){ # check if this generic variable is selected in the model
            
            c.variable.quali.sel <- c.variable.quali[str_detect(c.variable.quali, quali.var.sel)]
            col.sum <- colSums(data_explanatory_complete_normalized[, c.variable.quali.sel, with = FALSE])
            mode <- names(col.sum[which(col.sum == max(col.sum))])
            
            data_explanatory_median[, (mode) := lapply(.SD, function(x){1}), .SDcols = mode]
          
          }
          
        }
        
        data_explanatory_mean <- cbind(data_explanatory_mean, data_explanatory_median)
        
      }

      # add temporality (necessary to simulate dynamics)
      data_explanatory_mean <- cbind(data_explanatory_mean, data.table(stand = as.integer(0), climatic_year = c(0 : comparison.age))) # "as.integer" to check stand order in the "compute_height_base" function
    
    # compute impact for each variable  ----
    
    # list variables to test (without treating square variables separately from the linear corresponding variable)
        # reminder: a single variable can appear in the alph term OR in the gamma term, but not in both
      variable.to.test <- c.variable.climate[!str_detect(c.variable.climate, "square")]
    
      db_variable_impact <- rbindlist(lapply(variable.to.test, function(variable_selected){
        
        print(variable_selected)    
        
        # test if significant
        significant <- (db_parameter[str_detect(param, variable_selected), min(pvalue)] <= 0.05) # to see if significant or not
        
        # square variable
        variable_selected_square <- paste0(variable_selected,"_square")
        
        # define the simulation range and point number
        quantile.selected <- quantile(as.vector(data_explanatory_complete_normalized[, variable_selected, with = FALSE ])[[1]], probs = quantile.focus)
        Q_inf <- quantile.selected[1]
        Q_sup <- quantile.selected[2]
        
        nb_points <- 10
        
        value.sequence <- seq(Q_inf, Q_sup, length.out = nb_points)
        
        # do simulations for each value of the variable
        
        db.Hp.sel <- rbindlist(lapply(value.sequence, function(value.selected){
          
          # modify only the value of the selected variables (and the associated square variable, if relevant)
          data_explanatory_selected <- copy(data_explanatory_mean)
          data_explanatory_selected[, (variable_selected) := lapply(.SD, function(x){value.selected}), .SDcols = variable_selected]
          if(variable_selected_square %in% names(data_explanatory_mean)){
            data_explanatory_selected[, (variable_selected_square) := lapply(.SD, function(x){value.selected ^ 2}), .SDcols = variable_selected_square]
          }
          
          # simulate
          db_dynamics_selected <- compute_height_base(
            param_A0 = db_parameter[param_class == "A0", estimate],
            param_C0 = db_parameter[param_class == "C0", estimate],
            param_beta0 = db_parameter[param_class == "beta0", estimate],
            param_alpha = db_parameter[param_class == "alpha", estimate],
            param_gamma = db_parameter[param_class == "gamma", estimate],
            names.param_alpha = db_parameter[param_class == "alpha", param],
            names.param_gamma = db_parameter[param_class == "gamma", param],
            data_explanatory = data_explanatory_selected,
            initial.height = initial.height,
            data_age_height = data.table(
              stand = 0,
              year_observation = max(data_explanatory_selected$climatic_year),
              age = comparison.age,
              year_first = 1 # first year, not year 0
            )
          )
          db_dynamics_selected[, age := year - min(year), by = "stand"]
          Hp_final <- db_dynamics_selected[age == comparison.age, Hp]
          
          # get absolute value
          value_abs.selected <- value.selected * db_normalization_constant[variable == variable_selected,sd] + db_normalization_constant[variable == variable_selected,mean]
          
          # return
          return(
            data.table(
              species = sp.sel,
              variable = variable_selected,
              value = value.selected,
              value_abs = value_abs.selected,
              SDH = Hp_final,
              significant = significant
            )
          )
          
        }))
        
        # return at value level
        return(db.Hp.sel)
      
      }))
    
      # return at the variable level
      return(db_variable_impact)
  })
  names(list.db_variable_impact) <- list.species
  
  # return
  return(list.db_variable_impact)
  
}
