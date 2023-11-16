prepare_ifn_climate<-function(db,safran_map,crs_IFN,db_temperature_long,db_precipitation_long,lapse_rate_temperature,lapse_rate_precipitation,db_homogeneous_climatic_areas){
  
  # this function compute the climate variables before aggregation into seasonal windows, for all ifn stands (all year, all forest (even-aged or not, pure or mixed)) 
  
  # add temperature & precipitation
  db_temperature_precipitation <- prepare_ifn_temperature_precipitation(
    db=db,
    safran_map=safran_map,
    crs_IFN=crs_IFN,
    db_temperature_long=db_temperature_long,
    db_precipitation_long=db_precipitation_long,
    lapse_rate_temperature=lapse_rate_temperature,
    lapse_rate_precipitation=lapse_rate_precipitation,
    db_homogeneous_climatic_areas=db_homogeneous_climatic_areas
    )
  
  # keys_db_temperature_precipitation<-c("stand","month","year","variable")
  # setkeyv(db_temperature_precipitation,keys_db_temperature_precipitation)
  
  print("db_temperature_precipitation ok")
  
  # get radiation db
  db_radiation <- prepare_ifn_radiation(db=db,cloud=T) # monthly_radiations in MJ/m2/month

  # get db with temperature, climatic water balance (cwb=P-ETP) and sgdd
  db_climate <- prepare_ifn_cwb_sgdd(
    db_temperature_precipitation = db_temperature_precipitation,
    db_radiation = db_radiation
    )
  print("cwb and sgdd computed")
  
  # keys_db_climate<-c("stand","year","month")
  # setkeyv(db_climate,keys_db_climate)  
  
  return(db_climate)
  
}
