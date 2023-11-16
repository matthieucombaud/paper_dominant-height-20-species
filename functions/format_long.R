format_long<-function(db){
  
  db_long<-melt(data=db,id.vars="date",variable.name = "cell",variable.factor = FALSE)
  return(db_long[,.(date,cell=as.integer(cell),value)])
  
}
