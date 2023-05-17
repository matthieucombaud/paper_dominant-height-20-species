compute_climate.virtual <- function(
  year.average.climate.noCC,
  db_climate.monthly,
  last_month, 
  species_code,
  list_stand.named,
  db.radiation,
  list_season.varying,
  list_variables.varying
  ){ # name ends by "analysis" to avoid confusion with other function
  
  # restrict to the selected species
  db_climate.monthly_species.selected <- db_climate.monthly[stand %in% list_stand.named[[species_code]] ]
  

  # compute virtual climate for fixed climate variable

  list_db_climate.modified.season.variable <- lapply(list_variables.varying, function(variables.varying.selected){
    
    print(variables.varying.selected)
    
    list_db_climate.modified.season <- lapply(list_season.varying, function(season.varying.selected){
      
      print(season.varying.selected)

      # compute climate for average variables
      db_climate.monthly_virtual.selected <- get_climate.virtual(
        db_climate.monthly = db_climate.monthly_species.selected,
        db.radiation = db.radiation,
        variables.varying = variables.varying.selected,
        season.varying = season.varying.selected,
        averaging.period = year.average.climate.noCC
      )
      
      # compute for the necessary climatic windows
      
      list_period_climatic.month = list( # in terms of climatic month
        c(1:3),
        c(4:6),
        c(7:9),
        c(10:12),
        c(1:6),
        c(7:12),
        c(1:12)
      )
      
        # at this stage, we go to "climatic_year" instead of "year"
      db_climate.monthly_averaged_formated.selected <- aggregate_climate_global(
        db = db_climate.monthly_virtual.selected,
        last_month = last_month,
        list_period_climatic.month = list_period_climatic.month # in terms of climatic month
      )
        # this function opers the switch from "year" to "climatic year"
      
      return(db_climate.monthly_averaged_formated.selected)
      
    })
    names(list_db_climate.modified.season) <- names(list_season.varying)
    
    return(list_db_climate.modified.season)
    
  })
  names(list_db_climate.modified.season.variable) <- names(list_variables.varying)
  
  return(list_db_climate.modified.season.variable)
  
}
