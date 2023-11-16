get_climate.virtual <- function(
  db_climate.monthly,
  db.radiation,
  variables.varying,
  month.varying,
  averaging.period
){
  
  db_climate.monthly_modified <- db_climate.monthly[,.(stand, year, month, precipitation, Tmean)]
  
  # average non-selected variable and periods
  
  db_climate.monthly_modified <- melt(
    db_climate.monthly_modified,
    id.vars = c("stand", "year", "month"),
    measure.vars = c("precipitation", "Tmean"),
    variable.name = "variable"
    )
  
  db_climate.monthly_modified[
    ! month %in% month.varying | ! variable %in% variables.varying,
    value := sum(value * year %in% averaging.period) / sum(year %in% averaging.period), # manual computation of the :ean, that allows to fill also the row outside he period over which mean is computed
    by = c("stand", "month", "variable")
  ]
  
  db_climate.monthly_modified[variable == "Tmean", variable := "temperature"] # to have input in the right format for function 'prepare_ifn_cwb_sgdd"
  
  # control that the function functions well
  # month.test = 12
  # 
  # db_climate.monthly[stand == db_climate.monthly_modified$stand[1] & month == month.test & year %in% averaging.period, mean(Tmean)]
  # db_climate.monthly_modified[stand == db_climate.monthly_modified$stand[1] & month == month.test & variable == "temperature", unique(value)]
  # 
  # db_climate.monthly[stand == db_climate.monthly_modified$stand[1] & month == month.test & year %in% averaging.period, mean(precipitation)]
  # db_climate.monthly_modified[stand == db_climate.monthly_modified$stand[1] & month == month.test & variable == "precipitation", unique(value)]
  # 
  # db_climate.monthly_modified[stand == db_climate.monthly_modified$stand[1] & month == month.test & year == 2020]

  
  # compute cwb and sgdd after averaging
  
  db.radiation.formated <- prepare_ifn_radiation(
    db = db.radiation[, stand := IDP],
    cloud = T
  )
  
  db_climate.monthly_modified <- prepare_ifn_cwb_sgdd(
    db_temperature_precipitation = db_climate.monthly_modified,
    db_radiation = db.radiation.formated
  )
  
  return(db_climate.monthly_modified)
  
}
