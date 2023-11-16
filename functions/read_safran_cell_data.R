read_safran_cell_data<-function(safran_map_zip, data_source){
  
  # attention, no conversion into Lambert
  
  unzip(safran_map_zip, exdir = paste0(data_source,"/climate/safran/safran_map"), overwrite = T)
  
  safran_map <- sf::st_read(paste0(data_source,"/climate/safran/safran_map/safran.shp"))

  names(safran_map)[names(safran_map)=="number"]<-"cell"
  
  return(safran_map[,c("cell","altitude")])

}
