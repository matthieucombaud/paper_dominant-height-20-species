add_climatic_month_year<-function(db,last_month){
  
  db[,climatic_month:=modulo_up(n=month+(12-last_month),p=12)]
  db[,climatic_year:=ifelse(month<=last_month,year,year+1)]
  
  return(db)
  
}
