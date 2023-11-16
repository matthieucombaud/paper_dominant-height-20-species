prepare_db_altitude<-function(file){
  
  # return a db with altitude in kilometers
  db.altitude<-as.data.table(read.csv(file,header=T,sep=";"))
  
  setnames(db.altitude,old=c("idp","zp"),new=c("IDP","altitude"))

  return(db.altitude[,.(IDP,altitude)])
  
}
