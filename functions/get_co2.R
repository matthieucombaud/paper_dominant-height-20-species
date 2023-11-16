get_co2<-function(file_distant_years,file_recent_years){
  
  db_distant<-as.data.table(read.csv(file_distant_years,header=T))
  setnames(db_distant,old=c("data_mean_nh"),new=c("co2"))
  
  db_recent<-as.data.table(read.csv(file_recent_years,header=T,skip=55))
  setnames(db_recent,old=c("mean"),new=c("co2"))
  
  db<-merge(db_distant[,.(year,co2)],db_recent[,.(year,co2)],by="year",all=T,suffixes=c("_distant","_recent"))
  
  db[year%in%1959:2014,co2_distant-co2_recent] # there is a slight difference, but we do not take it into account
  
  db[year<1959,co2:=co2_distant]
  db[year%in%1959:2014,co2:=(co2_distant+co2_recent)/2]
  db[year>2014,co2:=co2_recent]
  
  return(db[,.(year,co2)])
  
}
