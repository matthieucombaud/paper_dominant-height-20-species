get_summary <- function(
  species_list,
  db_species.name,
  parameter_list_named, 
  db_rmse
){
  
  # get summary table ----
  
    # variable table
    db_variable <- rbindlist(lapply(species_list, function(species.selected){
      
      significant_param_species.selected <- parameter_list_named[[species.selected]][
        !param_class %in% c("intercept", "sigma", "delta") & !param %in% "intercept" & star != "ns", 
        unique(str_remove(param, "_square"))]
      
      # simplify climate var
      
      if(sum(str_detect(significant_param_species.selected, "sgdd")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "sgdd")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "sgdd")
      }
      
      if(sum(str_detect(significant_param_species.selected, "Tmean")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "Tmean")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "temperature")
      }
      
      if(sum(str_detect(significant_param_species.selected, "precipitation")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "precipitation")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "precipitation")
      }
      
      if(sum(str_detect(significant_param_species.selected, "cwb")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "cwb")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "climatic water balance")
      }
      
      # rename
      if(sum(str_detect(significant_param_species.selected, "affroc_perc")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "affroc_perc")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "rock emergence")
      }
      
      if(sum(str_detect(significant_param_species.selected, "cailloux_perc")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "cailloux_perc")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "rock presence")
      }
      
      if(sum(str_detect(significant_param_species.selected, "soil_depth")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "soil_depth")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "soil depth")
      }
      
      if(sum(str_detect(significant_param_species.selected, "radiation_annual")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "radiation_annual")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "annual radiations")
      }
      
      
      # simplify water logging
      if(sum(str_detect(significant_param_species.selected, "waterlogging")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "waterlogging")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "waterlogging")
      }
      
      
      # correct categorical variables
      
      if(sum(str_detect(significant_param_species.selected, "ROCHE.Calc_0_categorical")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "ROCHE.Calc_0_categorical")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "calcareous rock")
      }
      
      if(sum(str_detect(significant_param_species.selected, "humus_type")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "humus_type")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "humus type")
      }
      
      if(sum(str_detect(significant_param_species.selected, "soil_type")) > 0){
        
        significant_param_species.selected <- significant_param_species.selected[ !str_detect(significant_param_species.selected, "soil_type")]
        
        significant_param_species.selected <- c(significant_param_species.selected, "soil type")
      }
      
      
      
      db_variable_species.selected <- data.table(
        species_code = species.selected,
        variable = paste(significant_param_species.selected, collapse = ", ")
      )
      
      return(db_variable_species.selected)
      
      
    }))
    
    # add rmse
    
    db_rmse_variable <- merge(db_rmse, db_variable, by = "species_code")
    db_rmse_variable <- merge(db_species.name, db_rmse_variable, by.x = "code", by.y = "species_code")
  
  # get list of species with significant impact or not ----
  
  # identify species with no significant variables
  db_parameter_all <- rbindlist(parameter_list_named)
  
  species_significant_variable <- db_parameter_all[
    !param_class %in% c("intercept", "sigma", "delta") & !param %in% "intercept" & star != "ns",
    unique(species_code)
  ]
  
  
  species_variable.climate <- db_parameter_all[
    !param_class %in% c("intercept", "sigma", "delta") & !param %in% "intercept" &
      (
        str_detect(param, "sgdd") |
          str_detect(param, "Tmean") |
          str_detect(param, "precipitation") |
          str_detect(param, "cwb")
      ),
    unique(species_code)
  ]
  
  species_significant.variable.climate <- db_parameter_all[
    !param_class %in% c("intercept", "sigma", "delta") & !param %in% "intercept" & star != "ns" &
      (
        str_detect(param, "sgdd") |
          str_detect(param, "Tmean") |
          str_detect(param, "precipitation") |
          str_detect(param, "cwb")
      ),
    unique(species_code)
  ]
  
  species_no.significant.variable <- setdiff (species_list, species_significant_variable)
  species_no.variable.climate <- setdiff (species_list, species_variable.climate)
  species_no.significant.variable.climate <- setdiff (species_variable.climate, species_significant.variable.climate)

  
  # return
  return(
    list(
      db_summary = db_rmse_variable,
      db_parameter_all = db_parameter_all,
      species_significant = list(
        species_significant.variable.climate = species_significant.variable.climate,
        species_no.significant.variable = species_no.significant.variable, 
        species_no.variable.climate = species_no.variable.climate, 
        species_no.significant.variable.climate = species_no.significant.variable.climate
      )
    )
  )
  
}