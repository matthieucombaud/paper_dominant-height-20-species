average_climate <- function(
  db_climate.monthly, 
  db.radiation,
  variables.to.average,
  season.to.average,
  averaging.period
){
  
  
  db_climate.monthly_modified <- db_climate.monthly[,.(stand, year, month, precipitation, Tmean)]
  
  # do averaging for the selected variables
  
  if(variables.to.average %in% c("Tmean", "all")){
    
    db_climate.monthly_modified[
      month %in% season.to.average,
      Tmean := sum(Tmean * year %in% averaging.period) / sum(year %in% averaging.period),
      by = c("stand", "month")
      ]
    
  }
  setnames(db_climate.monthly_modified, "Tmean", "temperature") # necessary to use some functions after
  
  if(variables.to.average %in% c("precipitation", "all")){
    
    db_climate.monthly_modified[
      month %in% season.to.average,
      precipitation := sum(precipitation * year %in% averaging.period) / sum(year %in% averaging.period),
      by = c("stand", "month")
    ]
    
  }
  
  
  db_climate.monthly_modified <- melt(db_climate.monthly_modified, id.vars = c("stand", "year", "month"), variable.name = "variable", value.name = "value")
  
  
  # compute cwb and sgdd after averaging

  db.radiation.formated <- prepare_ifn_radiation(
    db = db.radiation[, stand := IDP],
    cloud = T
    )

  db_climate.monthly_modified <- prepare_ifn_cwb_sgdd(
    db_temperature_precipitation = db_climate.monthly_modified,
    db_radiation = db.radiation.formated
  )
    
  # return
  return(db_climate.monthly_modified)

}
