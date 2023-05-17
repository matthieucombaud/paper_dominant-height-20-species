compute_mean_increment<-function(db_envi,db_parameter,height,qualitative_variables){
  
  # prepare mean environment
  db_envi_mean <- prepare_mean_stand_mean_year(
    db_envi = db_envi,
    qualitative_variables = qualitative_variables
    )
  
  # compute increment
  increment_envi_mean <- compute_increment(
    db_parameter = db_parameter,
    data_explanatory = db_envi_mean,
    height = height
    )
  
  return(increment_envi_mean$increment)

}
