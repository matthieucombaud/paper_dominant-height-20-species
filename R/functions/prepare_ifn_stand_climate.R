prepare_ifn_stand_climate<-function(db_stand,db_climate,db_co2,db_radiation_annual){
  
  db<-merge(db_stand,db_climate,by="stand")
  db<-merge(db,db_radiation_annual,by="stand")
  db<-merge(db,db_co2,by.x="climatic_year",by.y="year")
  
  return(db)
  
}
