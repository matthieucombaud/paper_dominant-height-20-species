prepare_db_raw <- function(
  species_selected,
  stands_selected,
  db_ifn,db_climate,
  db_radiation_annual,
  db_co2,
  db_minimum_temperature,
  db_maximum_temperature,
  output_folder
  ){
  
  dir.create(output_folder, recursive = T)
  
  db <- db_ifn[Sp1.TCL==species_selected]
  db <- db[stand%in%stands_selected]

  db <- merge(db,db_climate,by="stand") # introduces the "climatic_year column"
  db <- merge(db,db_radiation_annual,by="stand")
  db <- merge(db,db_minimum_temperature,by=c("stand","climatic_year"))
  db <- merge(db,db_maximum_temperature,by=c("stand","climatic_year"))
  db <- merge(db,db_co2,by.x="climatic_year",by.y="year")
  
  fwrite(db,paste0(output_folder,"/db_raw_",species_selected,".csv"))
  
  return(db)
  
}
