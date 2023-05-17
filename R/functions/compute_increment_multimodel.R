compute_increment_multimodel<-function(db_param_list,data_explanatory,height){
  
  increment<-data.frame(envi_condition=numeric(),climatic_year=numeric(),stand=numeric(),height=numeric(),increment=numeric())
  
  for(db_param in db_param_list){
    
    increment<-rbind(increment,compute_increment(db_parameter =db_param ,data_explanatory =data_explanatory ,height=height))
    
  }
  
  increment$name<-names(db_param_list)
  
  return(increment)
  
}
