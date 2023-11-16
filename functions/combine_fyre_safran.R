combine_fyre_safran <- function(db_fyre,db_safran,reduce){
  
  keep.cols<-names(db_safran) # safran cells in France (and the date)
  
  db_fyre<-db_fyre[,..keep.cols] # remove the safran cells than are not in the French area (in the FYRE db)
  
  db_fyre[,":="(year=as.numeric(str_sub(date,1,4)),month=as.numeric(str_sub(date,6,7)))]
  db_safran[,":="(year=as.numeric(str_sub(date,1,4)),month=as.numeric(str_sub(date,6,7)))]
  
  if(reduce=="safran"){
    db_safran<-db_safran[year>max(db_fyre$year)] # we cut safran
  }
  
  if(reduce=="fyre"){
    db_fyre<-db_fyre[year<min(db_safran$year)] # we cut safran
  }
  
  db<-rbind(db_fyre,db_safran)
  
  db[,":="(year=NULL,month=NULL)] # to keep the manipulation easy, we keep only the "date" variable here
  
  return(db)
  
}
