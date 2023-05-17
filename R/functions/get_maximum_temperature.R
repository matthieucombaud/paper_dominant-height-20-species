get_maximum_temperature <- function(db,last_month){
  
  # The yearly maximum temperature is defined as the maximum of the Tmean temperature over the 12 months of the year
  
  # convert year into climatic year
  db <- add_climatic_month_year(db,last_month)
  
  # identify and keepc complete climatic year (to do merges)
  temp<-unique(db[,.(climatic_month,climatic_year)])
  temp<-temp[,.N,by=climatic_year]
  db <- db[climatic_year%in%temp[N==12,climatic_year]]
  
  db_Tmax <- db[,.(Tmax = max(Tmean)),by=c("stand","climatic_year")]
  
  return(db_Tmax)
  
}
