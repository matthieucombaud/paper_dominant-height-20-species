get_lapse_rate_proxy<-function(homo_clim_area_selected,db_lapse_rate){
  
  # get the proxy area(s)
  proxy_area_selected<-db_lapse_rate[homo_clim_area==homo_clim_area_selected,proxy_area] 
  proxy_area_selected<-str_split(proxy_area_selected,pattern="_",simplify=T)
  
  # take the mean of the lapse-rate of the selected area(s)
  return(mean(db_lapse_rate[homo_clim_area%in%proxy_area_selected,lapse_rate],na.rm=T))
  
}