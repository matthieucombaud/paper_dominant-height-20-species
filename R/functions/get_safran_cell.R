get_safran_cell<-function(db,safran_map,crs_IFN){
  
  safran_map<-st_transform(x=safran_map,crs=crs_IFN) # convert into Lambert
  sf<-st_as_sf(db,coords=c("x","y"),crs=st_crs(safran_map)) # coords of IFN points in the sf format
  index<-as.numeric(st_intersects(sf,safran_map)) # "as.numeric" and not "unlist", to keep NAsdb<-db[,.(stand,x,y,altitude)]
  
  return(safran_map$cell[index])
  
}
