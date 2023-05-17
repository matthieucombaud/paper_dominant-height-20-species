format_data_height_age <- function(data,age.correction){
  
  data <- unique(data[,.(stand,year_observation,Value1.AGE13,Value2.AGE13,Value1.HTOT,Value2.HTOT,Sp1.AGE13.HTOT,Sp2.AGE13.HTOT,DATEECO)])
  
  # correct year_observation for observation month (assuming same date for age/height measure and ecological measure)
  data[,month_obs :=str_split(DATEECO,"-",simplify = T)[,2]]
  data[,table(month_obs)]
  
  month_begin_growth <- "04"
  data[, year_observation_non_corrected := year_observation] 
  data[, year_observation := ifelse(month_obs < month_begin_growth, year_observation-1, year_observation)] 
  
  
  data[,":="(
    age=ifelse(!is.na(Value2.AGE13),(Value1.AGE13+Value2.AGE13)/2,Value1.AGE13), # we have always Value1.AGE13!=NA, by construction
    hfinal=ifelse(!is.na(Value2.AGE13),(Value1.HTOT+Value2.HTOT)/2,Value1.HTOT),
    nb_trees = ifelse(!is.na(Value2.AGE13),2,1) # nb of trees to compute the stand variables
    )
    ] 
  
  # age correction to get age from the origin
  data$age<-data$age+age.correction
  
  # to have an integer for age, without having a bias in the rounding
  set.seed(123)
  data[age%%1==0.5,age:=trunc(age)+sample(x=c(0,1),size=1)] 
  
  data[,year_first:=year_observation-age+1] # year_first correspond to age=1 (ie climatic year of the first period of growth), while height=1.3 m corresponds to age=0

  return(data[,.(stand,year_observation,age,hfinal,year_first,nb_trees,year_observation_non_corrected)])
  
}
