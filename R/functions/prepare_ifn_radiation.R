prepare_ifn_radiation<-function(db,cloud){
  
  if(cloud==T){
    radiation_variables<-paste0("RAD_C",c("01","02","03","04","05","06","07","08","09","10","11","12")) # radiations with cloudiness
  
  
  keep.cols.radiation<-c("stand",radiation_variables)
  
  db<-db[,..keep.cols.radiation]
  
  db<-melt(data=db,id.vars="stand",variable.name = "month",value.name = "radiation")
  db[,month:=as.numeric(str_sub(month,6,7))] # must  be in the "if", because else index change
  
  }
  
  return(db)
  
}
