simulate_height_dynamics <- function(
  species_code,
  year.simulation.initial,
  comparison.age,
  list_climate,
  dir.model,
  dir.data.calibration
  ){ # name ends by "analysis" to avoid confusion with other function
  
  print(species_code)
  
  # import model
    model <- readRDS(paste0(dir.model,"/sp",species_code,"_no_batch.rds"))[[1]]
  
  # get stands
    stands_species.selected <- model$stands
    
  # get parameters
    db_parameter_model <- model$db_parameter_normalized
    db_parameter_model$param_class <- factor(db_parameter_model$param_class,levels=c("intercept","alpha","gamma","beta","sigma","delta"))
    
  # import data
    list_data.explanatory <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species_code,".rds"))
    data_explanatory_complete_normalized <- list_data.explanatory$db_explanatory_normalized_complete
    data_information <- list_data.explanatory$db_information_variables
    data_age_height <- list_data.explanatory$data_age_height
    db_stand_number <- list_data.explanatory$stand_number
    db_normalization_constant <- list_data.explanatory$db_normalization_constant
  
  # restrict climate to selected species
    db_climate <- list_climate[[species_code]]
    
  # restrict data to useful variables
    
    keep_col_model <- c("stand","climatic_year",c(unique(db_parameter_model[param_class%in%c("alpha","gamma") | (param_class == "beta" & param != "intercept"),param])))
    data_explanatory_complete_normalized_model <- data_explanatory_complete_normalized[,..keep_col_model]
    
  # identify climate variables (with and without square)
    
    climate.variable <- unlist(sapply(c("Tmean", "cwb", "precipitation", "sgdd"), function(variable.climate.generic.selected){
      
      climate.variable.selected <- db_parameter_model[stringr::str_detect(param,variable.climate.generic.selected),param]
      return(climate.variable.selected)
      
    }))
    climate.variable <- unname(climate.variable)
    
    climate.variable_no.square <- unique(str_remove(climate.variable,"_square")) # required for next steps. Attention: also keep "climate.variable" with square
    
  # Parameters of the simulation
    initial.height <- 1.3  
    year.simulation.final <- year.simulation.initial + comparison.age
  
  # build the list of simulation databases, including only useful climate variables (with relevant square variables)
  
    list_climate.scenario <- names(db_climate)
    list_season.varying <- list(
      year = 1:12
    )
  
  db_dynamics_climate.season.variable <- rbindlist(lapply(list_climate.scenario, function(climate.scenario.selected){
    
    print(climate.scenario.selected)
    
    db_dynamics_climate.season <- rbindlist(lapply(names(list_season.varying), function(season.varying.selected){
      
      print(season.varying.selected)
      
      db.climate.selected <- db_climate[[climate.scenario.selected]][[season.varying.selected]]
      
      
      # restrict to useful variables
      db.climate.selected_retricted <-
        db.climate.selected[,
                           c("stand", "climatic_year", climate.variable_no.square),
                           with = F
                           ]
      
      # normalize
      db.climate.selected_retricted_normalized <- copy(db.climate.selected_retricted)
      
      for(climate.variable_no.square.selected in climate.variable_no.square){
        
        normalization.mean <- db_normalization_constant[variable == climate.variable_no.square.selected, mean]
        normalization.sd <- db_normalization_constant[variable == climate.variable_no.square.selected, sd]
        
        
        db.climate.selected_retricted_normalized[,
                                                 (climate.variable_no.square.selected) := lapply(.SD, function(x){ (x-normalization.mean) / normalization.sd}),
                                                 .SDcols = climate.variable_no.square.selected
        ]
        
      }
      
      # add square variables
      db.climate.selected_retricted_normalized_square <- copy(db.climate.selected_retricted_normalized)
      
      for(square.variable.selected in climate.variable[str_detect(climate.variable, "square")]){
        
        corresponding.variable <- str_remove(square.variable.selected, "_square")
        
        db.climate.selected_retricted_normalized_square[,
                                                        (square.variable.selected) := lapply(.SD, function(x){x^2}),
                                                        .SDcols = corresponding.variable
                                                        ]
        
      }
      
      # add non climate variables
      
      db_explanatory.virtual.selected <- merge(
        db.climate.selected_retricted_normalized_square, # db with modified climate var
        data_explanatory_complete_normalized_model[, setdiff(names(data_explanatory_complete_normalized_model),climate.variable), with = FALSE], # db with non-climate var
        by = c("stand", "climatic_year")
      )
    
      # do the simulation
      
      db_dynamics_climate_selected <- compute_height(
        db_parameter = db_parameter_model,
        data_explanatory = db_explanatory.virtual.selected,
        data_age_height = data.table(
          stand = data_age_height$stand,
          year_observation = year.simulation.final,
          age = comparison.age,
          year_first = year.simulation.initial + 1 # first year, not year 0
        ),
        initial.height = initial.height
      )
      
      # add scenario
      db_dynamics_climate_selected[, ":=" (climate.scenario = climate.scenario.selected, season.varying = season.varying.selected)]
      
      
      # return
      return(db_dynamics_climate_selected)
      
    }))
    
    return(db_dynamics_climate.season)
    
  }))
  
  # add species name
  db_dynamics_climate.season.variable[,species_code := species_code]
    
  # return
  return(db_dynamics_climate.season.variable)
  
}
