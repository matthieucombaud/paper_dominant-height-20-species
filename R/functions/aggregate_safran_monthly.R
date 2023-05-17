aggregate_safran_monthly<-function(db,pos){
  
  # fonction to transform daily data into monthly data
  
  db<-as.data.table(db)
  
  clim.var.average<-"t_q" # variable for which we want the monthly average
  clim.var.cumul<-c("pe_q","preliq_q","prenei_q","dli_q") # variable for which we want the monthly cumul
  
  climate.monthly_average<-db[,lapply(.SD, mean), .SDcols = clim.var.average, by = c("Site","Year","Month")]
  climate.monthly_cumul<-db[,lapply(.SD, sum), .SDcols = clim.var.cumul, by = c("Site","Year","Month")]
  
  climate.monthly<-merge(x=climate.monthly_average,y=climate.monthly_cumul,by=c("Site","Year","Month"))
  
  return(climate.monthly)

}
