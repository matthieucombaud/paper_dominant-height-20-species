  prepare_ifn_temperature_precipitation <- function(
    db,
    safran_map,
    crs_IFN,
    db_temperature_long,
    db_precipitation_long,
    lapse_rate_temperature,
    lapse_rate_precipitation,
    db_homogeneous_climatic_areas
    ){
  
    
  # transform coordinates
  safran_map <- st_transform(x=safran_map,crs=crs_IFN)
  
  # filter stand in the perimeter of safran data
  out_of_safran_perimeter<-is.na(get_safran_cell(db=db,safran_map=safran_map,crs=crs_IFN))
  db <- db[!out_of_safran_perimeter]
  print(paste0(sum(out_of_safran_perimeter)," stands excluded because outside of Safran perimeter"))
    
    
  # add climate based on FYRE data
  db_ifn_temperature_precipitation <- compute_stand_temperature_precipitation_FYRE(
    db = db,
    safran_map = safran_map,
    crs_IFN = crs_IFN,
    list_variable = c("temperature", "precipitation"),
    list_db_clim = list(temperature=copy(db_temperature_long),precipitation=copy(db_precipitation_long)),
    list_lapse_rate = list(temperature=lapse_rate_temperature,precipitation=lapse_rate_precipitation),
    db_homogeneous_climatic_areas = db_homogeneous_climatic_areas
    )

  # add year and month columns
  
  db_ifn_temperature_precipitation[, year := as.numeric(str_sub(period,1,4))]
  db_ifn_temperature_precipitation[, month := as.numeric(str_sub(period,6,7))] # eanble to remove "0" at the befinning of some months
  db_ifn_temperature_precipitation[,period:=NULL]
  
  # keys_db_temperature_precipitation<-c("stand","month","year","variable")
  # setkeyv(db_ifn_temperature_precipitation,keys_db_temperature_precipitation)

  return(db_ifn_temperature_precipitation)
  
}
