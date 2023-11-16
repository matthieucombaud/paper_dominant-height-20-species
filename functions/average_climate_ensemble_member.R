average_climate_ensemble_member<-function(list_file){
  
  
  list_db<-lapply(list_file,function(x){
    as.data.table(fread(x,header=T))
  }
  )
  
  # compute the average by taking the sum over all ensemble member and then dividind
  db_temp<-list_db[[1]][,-1]
  
  if(length(list_file)>1){
    for(i in 2:length(list_file)){
      db_temp<-db_temp+list_db[[i]][,-1]
    }
    db_temp<-db_temp/length(list_file)
  }

  db_temp<-cbind(list_db[[1]][,1],db_temp)
  setnames(db_temp,"V1","date")
  
  db_temp$date<-format(db_temp$date,"%Y_%m")
  
  return(db_temp)
  
}
