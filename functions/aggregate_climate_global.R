aggregate_climate_global <- function(db, last_month, list_period_climatic.month){
  
  # add a climatic month and a climatic year columns
  db <- add_climatic_month_year(db,last_month)
  
  # identify complete climatic year (to do keep only year climatic years for which we have complete data)
  temp <- unique(db[,.(climatic_month,climatic_year)])
  temp <- temp[,.N,by=climatic_year]
  keep.climatic.years <- temp[N==12,climatic_year]
  
  # db <- db[climatic_year %in% keep.years]
  
  # order the db (to facilitate visualisation)
  # keys=c("stand","climatic_year")
  # setkeyv(db, keys)
  
  # list_period_climatic.month <- get_all_periods(duration = duration) # old version: put 'n' to get period of n months
  
  # Tmean ----
    # attention : the period is in terms of climatic month!
  list_db_Tmean_output <- lapply(list_period_climatic.month, function(period) {
    
    db_aggregated <- db[climatic_month %in% period,.(value=mean(Tmean,na.rm=F)), by = c("stand","climatic_year")] # we take the mean
    setnames(db_aggregated, "value", paste0("Tmean_",modulo_up(n=min(period)+last_month,p=12),"_",modulo_up(n=max(period)+last_month,p=12)))
    return(db_aggregated) 
    
    })  
  db_Tmean_output <- Reduce(function(x, y) merge(x=x, y=y, by=c("stand","climatic_year"), all.x = T, all.y = T), list_db_Tmean_output) # 'all.x = T, all.y = T' to keep also yers for which we do not have all the climatic months. This enables to exclude them in a clean way after.
  
  # precipitation ----
  list_db_precipitation_output <- lapply(list_period_climatic.month, function(period) {
    
    db_aggregated <- db[climatic_month %in% period,.(value=sum(precipitation,na.rm=F)),by=c("stand","climatic_year")] # we take the mean
    setnames(db_aggregated, "value", paste0("precipitation_",modulo_up(n=min(period)+last_month,p=12),"_",modulo_up(n=max(period)+last_month,p=12)))
    return(db_aggregated) 
    
  })   # attention : the period is in terms of climatic month!
  db_precipitation_output <- Reduce(function(x, y) merge(x=x, y=y, by=c("stand","climatic_year"), all.x = T, all.y = T), list_db_precipitation_output)
  
  # cwb ----
  list_db_cwb_output <- lapply(list_period_climatic.month, function(period) {
    
    db_aggregated <- db[climatic_month %in% period,.(value=sum(cwb,na.rm=F)),by=c("stand","climatic_year")] # we take the mean
    setnames(db_aggregated, "value", paste0("cwb_",modulo_up(n=min(period)+last_month,p=12),"_",modulo_up(n=max(period)+last_month,p=12)))
    return(db_aggregated) 
    
  })   # attention : the period is in terms of climatic month!
  db_cwb_output <- Reduce(function(x, y) merge(x=x, y=y, by=c("stand","climatic_year"), all.x = T, all.y = T), list_db_cwb_output)
  
  # sgdd ----
    # attention, sgdd is based on January to December period!
  list_db_sgdd_output <- lapply(list(periods = c(1:12)), function(period) {
    
    db_aggregated <- db[month %in% period,.(value = sum(sgdd,na.rm=F)),by=c("stand","year")] # we take the mean
    setnames(db_aggregated, "value", paste0("sgdd_",min(period),"_",max(period)))
    
    return(db_aggregated) 
    
  })   # attention : the period is in terms of climatic month!
  db_sgdd_output <- Reduce(function(x, y) merge(x=x, y=y, by=c("stand","year"), all.x = T, all.y = T), list_db_sgdd_output)
  setnames(db_sgdd_output, "year", "climatic_year")
  
 # output
  output <- Reduce(function(x, y) merge(x=x, y=y, by=c("stand","climatic_year"), all.x = T, all.y = T),
                   list(
                     db_Tmean_output, 
                     db_precipitation_output, 
                     db_cwb_output, 
                     db_sgdd_output
                   ))
  
  output <- output[climatic_year %in% keep.climatic.years]
  
  return(output)
  
}
