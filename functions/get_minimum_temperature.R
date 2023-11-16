get_minimum_temperature <- function(db,last_month){
  
  # The yearly minimal temperature is defined as the minimum of the Tmean temperature over the 12 months of the year
  
  # convert year into climatic year
  db <- add_climatic_month_year(db,last_month)
  
  # identify and keep complete climatic year (to do merges)
  temp<-unique(db[,.(climatic_month,climatic_year)])
  temp<-temp[,.N,by=climatic_year]
  db <- db[climatic_year%in%temp[N==12,climatic_year]]
  
  db_Tmin <- db[,.(Tmin = min(Tmean)),by=c("stand","climatic_year")]
  
  return(db_Tmin)
  
}
