keep_IDP<-function(db,selected.IDP){
  
  return(db[IDP%in%selected.IDP])
  
}