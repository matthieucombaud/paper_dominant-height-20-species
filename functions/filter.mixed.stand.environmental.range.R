# Description
# From db of mixed stands containing stand ID, sp1 ID and sp2 ID , return a sub-db of stands that belong to the range of environmental conditions used for SDH model calibration.

# Arguments
# db.stand: db containing stand ID, sp1 ID and sp2 ID that we want to restrict to calibration environmental conditions
# db.stand.ref: db containing stand ID used for model calibration and corresponding sp
# list.variable : list containing one character vector per sp, containing variables selected in the model
# db.envi: dt of stand environmental conditions
# list.species: list of sp to consider
# threshold : quantile threshold to consider an observation is in the calibration range
# first_yera_climate_db: first year of climate data (if stand origin is earlier, we won't be able to simulate stand dynamics)
# potential.categorical.variables : categorical variables introduced in the model calibration process, that have to be treated separately

# Value
# a subvector of stand 

filter.mixed.stand.environmental.range <- function(
  db.stand.new,
  db.stand.ref,
  list.variable,
  db.envi,
  list.species, 
  threshold = 0.025,
  first.year.climate.db = 1872, # first year of the climate data base
  potential.categorical.variables = c(
    "ROCHE.Calc",
    "soil_type",
    "humus_type"
  )  
){

  # remove stand with too old trees ----
  db.stand.new <- db.stand.new[
    year_observation - age_sp1 + 1 >= first.year.climate.db & year_observation - age_sp2 +  1 >= first.year.climate.db
    ]
  
  # keep stands in the calibration range, sp per sp ----

  list_stand.keep <- lapply(list.species, function(sp.selected){
    
    print(sp.selected)
    
    # stands to examine
    db.stand.ref.selected <- db.stand.ref[sp == sp.selected]
    
    db.stand.new.selected.1 <- db.stand.new[sp1 == sp.selected, .(stand, year_first = year_observation - age_sp1 + 1)] # case when the focal sp is sp 1 in the db
    db.stand.new.selected.2 <- db.stand.new[sp2 == sp.selected, .(stand, year_first = year_observation - age_sp2 + 1)] # case when the focal sp is sp 2 in the db
    db.stand.new.selected <- rbind(db.stand.new.selected.1, db.stand.new.selected.2)
    
    
    # divide the envi db into categorical and non-categorical variables, and simplify the db
    variable.selected <- list.variable[[sp.selected]]
    variable.selected <- variable.selected[!str_detect(variable.selected, "square")]
    variable.selected.non.categorical <- variable.selected[!str_detect(variable.selected, "categorical")]
    variable.selected.categorical <- variable.selected[str_detect(variable.selected, "categorical")]

    db.envi.selected.non.categorical <- db.envi[, c("stand", "climatic_year", variable.selected.non.categorical), with = FALSE]
    db.envi.selected.categorical <- db.envi[, c("stand", "climatic_year", potential.categorical.variables), with = FALSE]
    
    
    # remove stands with categorical variables out of the calibration range ----

    db.envi.selected.categorical.ref <- unique(merge(db.envi.selected.categorical[, -"climatic_year"], db.stand.ref.selected, by = "stand")) # we keep only the stand corresponding to the selected sp
    db.envi.selected.categorical.new <- unique(merge(db.envi.selected.categorical[, -"climatic_year"], db.stand.new.selected, by = "stand")) # we keep only the stand corresponding to the selected sp
    
    stand.to.remove <- unique(unlist(lapply(potential.categorical.variables, function(potential.categorical.variables.selected){
      
      # restrict db to this variable and rename db
      db.envi.selected.categorical.ref.selected <- db.envi.selected.categorical.ref[
        , c("stand", potential.categorical.variables.selected), with = FALSE
        ]
      setnames(db.envi.selected.categorical.ref.selected, potential.categorical.variables.selected, "variable.interest") # to be able to compute quantile
      
      db.envi.selected.categorical.new.selected <- db.envi.selected.categorical.new[
        , c("stand", potential.categorical.variables.selected), with = FALSE
        ]
      setnames(db.envi.selected.categorical.new.selected, potential.categorical.variables.selected, "variable.interest") # to be able to compute quantile
      
      # if the categorical variable is relevant for this sp
        # we exclude all new stands for which the variable takes a value that is not taken over the calibration stands
      if(sum(str_detect(variable.selected.categorical, potential.categorical.variables.selected)) > 0){
        
        accepted.values <- unique(db.envi.selected.categorical.ref.selected[, variable.interest])
        
        remove.stand <- db.envi.selected.categorical.new.selected[!(variable.interest %in% accepted.values), stand] # also removes NA
        
        return(remove.stand)
        
      }
      
      return(integer())
      
    })))
    
    stand.keep.variable.categorical <- setdiff(db.stand.new.selected$stand, stand.to.remove)
    
    # Remove stands with non-categorical variables out of the calibration range ----

      # get envi db
      db.envi.selected.non.categorical.ref <- merge(db.envi.selected.non.categorical, db.stand.ref.selected, by = "stand") # we keep only the stand corresponding to the selected sp
      db.envi.selected.non.categorical.ref <- db.envi.selected.non.categorical.ref[climatic_year >= year_first] # we keep only the year that were actually used for calibration
      
      # loop to treat both climatic and non-climatic variables (ok to treat both kind of variables simultaneously since we first take the quantile at the stand level)
      db.range.calibration <- rbindlist(lapply(variable.selected.non.categorical, function(variable.selected.bis){
        
        db.envi.selected.non.categorical.ref.selected <- db.envi.selected.non.categorical.ref[, c("stand", variable.selected.bis), with = FALSE]
        setnames(db.envi.selected.non.categorical.ref.selected, variable.selected.bis, "variable.interest") # to be able to compute quantile
        
        # take the min / max over time for each stand (useful only for climate variable, but works for all non-categorical variables)
        
        # db.quantile.stand <- db.envi.selected.non.categorical.ref.selected[, .(
        #   variable = variable.selected.bis,
        #   var_min = quantile(variable.interest, threshold), 
        #   var_max = quantile(variable.interest, 1-threshold)
        # ), by = stand]
        
        db.extrema.stand <- db.envi.selected.non.categorical.ref.selected[, .(
          variable = variable.selected.bis,
          var_min = min(variable.interest), 
          var_max = max(variable.interest)
          ), by = stand]
        
        # take the quantile over all stands (thus, quantile of quantile)
        db.quantiles.final <- unique(db.extrema.stand[,.(
          variable,
          Qinf = quantile(var_min, threshold),
          Qsup = quantile(var_max, 1 - threshold)
          )
          ])
        
        return(db.quantiles.final)
  
      }))
      
    # get the variable distribution for the new stands

      # get db envi
      db.envi.selected.non.categorical.new <- merge(db.envi.selected.non.categorical, db.stand.new.selected, by = "stand") # we keep only the stand corresponding to the selected sp
      db.envi.selected.non.categorical.new <- db.envi.selected.non.categorical.new[climatic_year >= year_first] # we keep only the year that were actually used for calibration
      
      # remove stand with missing value for predictive variable
      stand.pb <- unique(unlist(lapply(variable.selected.non.categorical, function(variable.selected.bis){
        
        db.envi.selected.non.categorical.new.selected <- db.envi.selected.non.categorical.new[, c("stand", variable.selected.bis), with = FALSE]
        setnames(db.envi.selected.non.categorical.new.selected, variable.selected.bis, "variable.interest") # to be able to compute quantile
        
        stand.pb <- unique(db.envi.selected.non.categorical.new.selected[is.na(variable.interest), stand])
        
        return(stand.pb)
        
      })))
      
      db.envi.selected.non.categorical.new <- db.envi.selected.non.categorical.new[!(stand %in% stand.pb)]
      
      
      # get quantiles (for both climatic and non-climatic variables, cf supra for reference stands)
      
        db.range.new.stand <- rbindlist(lapply(variable.selected.non.categorical, function(variable.selected.bis){
          
          db.envi.selected.non.categorical.new.selected <- db.envi.selected.non.categorical.new[, c("stand", variable.selected.bis), with = FALSE]
          setnames(db.envi.selected.non.categorical.new.selected, variable.selected.bis, "variable.interest") # to be able to compute quantile
          
          db.extrema.stand <- db.envi.selected.non.categorical.new.selected[, .(
            variable = variable.selected.bis,
            var_min = min(variable.interest), 
            var_max = max(variable.interest)
          ), by = stand]
          
          return(db.extrema.stand)
          
        }))

    # identify the stand to keep for the criteria over non-categorical variables
      
      db.quantile.filtering <- merge(db.range.new.stand, db.range.calibration, by = "variable")
      db.quantile.filtering <- db.quantile.filtering[, sum_criteria := sum(var_min >= Qinf & var_max <= Qsup), by = "stand"] # count how many variables are in the calibration range
      
      # check all criteria are fullfilled simultaneously
      # db.quantile.filtering[, N := .N, by = "stand"]
      # db.quantile.filtering <- db.quantile.filtering[N == length(variable.selected.non.categorical)]
    
      stand.keep.variable.non.categorical <- unique(db.quantile.filtering[sum_criteria == length(variable.selected.non.categorical), stand])
      
      # examples of excluded stands
      # stand.exclude.variable.non.categorical <-setdiff(unique(db.quantile.filtering$stand), stand.keep.variable.non.categorical)
      # db.quantile.filtering[stand == stand.exclude.variable.non.categorical[1]]
      
    # Get the stand to keep for both criteria (categorical and non-categorical variables) ----
      
      stand.keep <- intersect(stand.keep.variable.categorical, stand.keep.variable.non.categorical)
      
    # return
    return(stand.keep)
  
  })
  names(list_stand.keep) <- list.species
  
  # keep stands in the calibration range for both sp ----
  
  c.keep.stand <- sapply(db.stand.new$stand, function(stand.selected){
    
    sp1 <- db.stand.new[stand == stand.selected, sp1]
    sp2 <- db.stand.new[stand == stand.selected, sp2]
    
    keep.sp1 <- is.element(stand.selected, list_stand.keep[[sp1]])
    keep.sp2 <- is.element(stand.selected, list_stand.keep[[sp2]])

    if(keep.sp1 & keep.sp2){
      return(stand.selected)
    }
    
    return(NULL)
    
  })
  
  db.stand.new.filtered <- db.stand.new[stand %in% c.keep.stand]
  
  return(db.stand.new.filtered)
  
}
