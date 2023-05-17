merge_db_ifn<-function(db.list){
  
  db.all<-Reduce(function(x, y) merge(x=x, y=y, by=c("IDP"), all=T), db.list)
  
}
