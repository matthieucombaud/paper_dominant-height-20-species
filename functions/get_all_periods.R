get_all_periods<-function(duration){ # get all possible months combinations
  # duration is a vector containing the number of months for which we want the cumulation. eg duraction=c(1,3) to test mensual and trimestrial variables
  
  list.vector <- list()
  
  k<-1
  
  for(i in duration){
    
    for(j in 1:12){
      
      vec<-seq(from=j,by=1,length.out=i)
      
      if(max(vec)<=12){
        list.vector[[k]]<-vec
        k<-k+1
      }
      
    }
    
  }
  
  return(list.vector)
  
}
