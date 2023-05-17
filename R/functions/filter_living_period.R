filter_living_period <- function(data_explanatory,data_age_height){
  
  db_output <- rbindlist(lapply(data_age_height$stand,function(x){ # attention, do it only after normalization over the whole sample (ie same temporal depth)
    
    year.initial<-data_age_height[stand==x,year_first]  # year_first correspond to age=1, while heigth=1.3 m corresponds to age=0
    year.final<-data_age_height[stand==x,year_observation]
    
    return(data_explanatory[stand==x & climatic_year%in%year.initial:year.final])
    
  })) # this code ensure that stands are in the same order in db_output as in data_age_height, and that years are well ordered
  
  return(db_output)
  
}
