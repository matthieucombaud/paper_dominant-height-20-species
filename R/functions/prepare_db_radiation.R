prepare_db_radiation<-function(file){
  
  db_radiation<-as.data.table(fread(file))
  
  setnames(db_radiation,"stand","IDP")
  
  db_radiation<-unique(db_radiation) # because two stands are duplicated
 
  return(db_radiation[,-c("x","y","altitude","PENT2","EXPO")]) # to avoid redundancy accross databases
}


