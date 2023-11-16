get_indirect.partial.impact <- function(
  species_code,
  dir.model,
  dir.data.calibration,
  model.type,
  batch,
  db_climate.monthly,
  db.radiation,
  comparison.age
){
  
  # some constant parameters
  last_month <- 8
  initial.height <- 1.3
  qualitative_variables <- c("ROCHE.Calc", "soil_type", "humus_type")
  
  
  # print info
  
  print(paste0("************* ",species_code, " *****************"))
  
  # import model
  model <- readRDS(paste0(dir.model,"/sp",species_code,"_",model.type,".rds"))[[batch]]
  
  # get stands
  stands_species.selected <- model$stands
  
  # get parameters
  db_parameter <- model$db_parameter_normalized
  db_parameter$param_class <- factor(db_parameter$param_class,levels=c("intercept","alpha","gamma","beta","sigma","delta"))
  
  # identify significant parameters
  significant_parameters <- unique(db_parameter[pvalue <= 0.05, param]) 
  significant_parameters <- unique(str_remove(significant_parameters, "_square"))
  significant_parameters <- significant_parameters[!significant_parameters %in% c("A0", "C0", "intercept","sigma", "delta")]
  
  
  # identify climate variables (with and without square)
  
  climate.variable <- unlist(sapply(c("Tmean", "cwb", "precipitation", "sgdd"), function(variable.climate.generic.selected){
    
    climate.variable.selected <- db_parameter[stringr::str_detect(param,variable.climate.generic.selected),param]
    return(climate.variable.selected)
    
  }))
  climate.variable <- unname(climate.variable)
  climate.variable_no.square <- unique(str_remove(climate.variable,"_square")) # required for next steps. Attention: also keep "climate.variable" with square
  
  # import data
  list_data.explanatory <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species_code,".rds"))
  data_explanatory <- list_data.explanatory$db_explanatory_normalized_complete
  data_information <- list_data.explanatory$db_information_variables
  data_age_height <- list_data.explanatory$data_age_height
  db_stand_number <- list_data.explanatory$stand_number
  db_normalization_constant <- list_data.explanatory$db_normalization_constant
  
  # focus on non climatic variables
  non.climatic.variables <- c(
    c(unique(db_parameter[param_class%in%c("alpha","gamma") | (param_class == "beta" & param != "intercept"),param]))
  )
  non.climatic.variables <- non.climatic.variables[
    ! str_detect(non.climatic.variables, "Tmean") &
      ! str_detect(non.climatic.variables, "precipitation") &
      ! str_detect(non.climatic.variables, "cwb") &
      ! str_detect(non.climatic.variables, "sgdd")
  ]
  
  data_explanatory_non.climatic.variables <- data_explanatory[, non.climatic.variables, with = FALSE]
  
  
  ## prepare average non climate variables ----
  
  # qualitative columns
  keep_col_qualitative <- unlist(sapply(qualitative_variables, function(qualitative_variables.selected){
    
    names(data_explanatory_non.climatic.variables)[str_detect(names(data_explanatory_non.climatic.variables),qualitative_variables.selected)]
    
  }))
  
  # quantitative columns
  keep_col_quantitative <- setdiff(names(data_explanatory_non.climatic.variables), c("stand", "climatic_year", keep_col_qualitative))
  
  # take mean (or median)
  
  data_explanatory_non.climatic.variables_mean <- copy (data_explanatory_non.climatic.variables[,lapply(.SD,function(x){mean(x)}),.SDcols=keep_col_quantitative])
  
  if (length(keep_col_qualitative) > 0){
    
    data_explanatory_non.climatic.variables_median <- copy(data_explanatory_non.climatic.variables[,lapply(.SD,function(x){median(x)}),.SDcols=keep_col_qualitative])
    data_explanatory_non.climatic.variables_mean <- cbind(data_explanatory_non.climatic.variables_mean, data_explanatory_non.climatic.variables_median)
    
  }
  
  
  
  ## prepare climate ----
  
  quantile.interest <- seq(0.05,0.95,0.1)
  
  db_climate_observed <- db_climate.monthly[
    stand %in% stands_species.selected
  ][,.(stand, year, month, precipitation, Tmean)]
  
  
  db_climate_observed <- db_climate_observed[ !(year <= 1871 & month <= last_month) & !(year >= 2020 & month > last_month)] # remove month not considered in the calibration sample. Useful to get consistent mean
  
  # climat observé moyen par quantile + à la moyenne
  
  db_climate_observed_quantile <- rbindlist(lapply(quantile.interest, function(quantile.selected){
    
    db_climate_observed_quantile.selected <- db_climate_observed[,
                                                                 lapply(.SD, function(x){
                                                                   quantile(x, quantile.selected)
                                                                 }),
                                                                 by = "month",
                                                                 .SDcols = c("Tmean", "precipitation")
    ]
    db_climate_observed_quantile.selected[, quantile := quantile.selected]
    
    
  }))
  db_climate_observed_quantile <- melt(db_climate_observed_quantile, id.vars = c("quantile","month"))
  
  db_climate_observed_mean <- db_climate_observed[,
                                                  lapply(.SD, function(x){
                                                    mean(x)
                                                  }),
                                                  by = "month",
                                                  .SDcols = c("Tmean", "precipitation")
  ]
  db_climate_observed_mean <- melt(db_climate_observed_mean, id.vars = c("month"))

  # compute SDH ----
  db_SDH <- rbindlist(lapply(c("Tmean", "precipitation"), function(variable.selected){
    
    db_SDH_variable <- rbindlist(lapply(list(
      c(9:11),
      c(12,1,2),
      c(3:5),
      c(6:8),
      c(9:12,1:2),
      c(3:8),
      c(9:12,1:8)
    ), function(season.selected){
      
      variable.selected.full <- paste0(variable.selected,"_",season.selected[1],"_",season.selected[length(season.selected)])
      
      print(variable.selected.full)
      
      # if(!variable.selected.full %in% climate.variable_no.square){ # case where the variable was not selected in the model
      #   return(NULL)
      # }
      
      db_SDH_variable.season <- rbindlist(lapply(quantile.interest, function(quantile.selected){
        
        print(paste0("quantile ",quantile.selected))
        
        db_climate_virtual <- copy(db_climate_observed_mean)
        db_climate_virtual[, quantile := quantile.selected]
        
        for(month.selected in season.selected){
          
          value.replacement <- db_climate_observed_quantile[
            quantile == quantile.selected & month == month.selected & variable == variable.selected,
            value
          ]
          
          db_climate_virtual[month == month.selected & variable == variable.selected, value := value.replacement ]
        }
        
        
        
        # complete formating
        db_climate_virtual[variable == "Tmean", variable := "temperature"]
        setnames(db_climate_virtual, "quantile", "stand")
        db_climate_virtual[, year := 0]
        
        db.radiation.formated <- prepare_ifn_radiation(
          db = db.radiation[, stand := IDP],
          cloud = T
        )
        db.radiation.averaged <- unique(db.radiation.formated[, .(stand = quantile.selected, radiation = mean(radiation)), by = month])
        
        db_climate_virtual <- prepare_ifn_cwb_sgdd(
          db_temperature_precipitation = db_climate_virtual,
          db_radiation = db.radiation.averaged
        )
        
        # add temporality
        db_year <- as.data.table(expand(data.table(year = seq(0, comparison.age, 1), month = c(1:12)), year, month))
        db_climate_virtual <- merge(db_climate_virtual[, -"year"], db_year, by = "month")
        
        # aggregate climate variables
        
        db_climate_virtual <- aggregate_climate_global(
          db = db_climate_virtual,
          last_month = last_month,
          list_period_climatic.month = list(
            c(1:3),
            c(4:6),
            c(7:9),
            c(10:12),
            c(1:6),
            c(7:12),
            c(1:12)
          ) # in terms of climatic month
        )
        
        # save this full database
        db_climate_virtual.save <- copy(unique(db_climate_virtual[,-"climatic_year"]))
        
        # keep only useful variables
        db_climate_virtual <- db_climate_virtual[, c("stand", "climatic_year", climate.variable_no.square), with = FALSE]
        
        # normalize climate data
        
        db_climate_virtual_normalized <- copy(db_climate_virtual)
        
        for(climate.variable_no.square.selected in climate.variable_no.square){
          
          normalization.mean <- db_normalization_constant[variable == climate.variable_no.square.selected, mean]
          normalization.sd <- db_normalization_constant[variable == climate.variable_no.square.selected, sd]
          
          db_climate_virtual_normalized[,
                                        (climate.variable_no.square.selected) := lapply(.SD, function(x){ (x-normalization.mean) / normalization.sd}),
                                        .SDcols = climate.variable_no.square.selected
          ]
          
        }
        
        # add square variables
        for(square.variable.selected in climate.variable[str_detect(climate.variable, "square")]){
          
          corresponding.variable <- str_remove(square.variable.selected, "_square")
          
          db_climate_virtual_normalized[,
                                        (square.variable.selected) := lapply(.SD, function(x){x^2}),
                                        .SDcols = corresponding.variable
          ]
          
        }
        
        
        # merge with non-climatic data
        
        db_explanatory_virtual <- merge(
          db_climate_virtual_normalized, # db with modified climate var
          data_explanatory_non.climatic.variables_mean[, stand := quantile.selected], # db with non-climate var
          by = c("stand")
        )
        
        # compute SDH
        db_dynamics <- compute_height(
          db_parameter = db_parameter,
          data_explanatory = db_explanatory_virtual,
          data_age_height = data.table(
            stand = quantile.selected,
            year_observation = comparison.age,
            age = comparison.age,
            year_first = 1 # first year, not year 0
          ),
          initial.height = initial.height
        )
        
        SDH <- db_dynamics[age == comparison.age, height]
        
        # get absolute value

        value_abs.selected <- as.numeric(
          unique(
            db_climate_virtual.save[, variable.selected.full, with = FALSE]
          )
        )
        
        return(
          data.table(
            species = species_code,
            variable = variable.selected.full,
            quantile.selected = quantile.selected,
            value_abs = value_abs.selected,
            SDH = SDH,
            significant = is.element(variable.selected.full, significant_parameters)
          )
        )
        
      }))
      
      return(db_SDH_variable.season)
      
    }))
    
    return(db_SDH_variable)
    
  }))
  
  return(db_SDH)
  
}
