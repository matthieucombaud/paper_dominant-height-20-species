prepare_radiation_annual<-function(db){
  
  db_radiation<-prepare_ifn_radiation(db=db,cloud=T) # monthly_radiations in MJ/m2/month
  db_radiation_annual<-db_radiation[,.(radiation_annual=sum(radiation)),by=stand]
  setkey(db_radiation_annual,"stand")
  
  return(db_radiation_annual)
  
}
