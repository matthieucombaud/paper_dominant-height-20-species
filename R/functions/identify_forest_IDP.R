identify_forest_IDP<-function(db.stand){
  
  return(unique(db.stand[CSA%in%c(1,2,3),IDP]))
  
}