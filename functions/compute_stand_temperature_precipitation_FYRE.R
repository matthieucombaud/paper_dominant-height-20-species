compute_stand_temperature_precipitation_FYRE<-function(db,safran_map,crs_IFN,list_variable,list_db_clim,list_lapse_rate,db_homogeneous_climatic_areas){
  
  # prepare db
  db<-db[,.(stand,x,y,altitude)] # restrict db
  db[,safran_cell:=get_safran_cell(db=db,safran_map=safran_map,crs=crs_IFN)] # associate a point to a safran cell # for some stand we are out the safran scope and we receive NA there

  db[,safran_altitude:=safran_map$altitude[match(db$safran_cell,safran_map$cell)]] # add (mean) altitude of the safran cell
  setnames(db,"altitude","ifn_altitude")
  db[,delta_altitude:=ifn_altitude-safran_altitude]
  
  db <- merge(db,db_homogeneous_climatic_areas[,.(cell,symp)],by.x="safran_cell",by.y="cell") # add homogeneous climatic area ("symp")
  
  # enter the loop over variables
  db_output <- rbindlist(lapply(list_variable,function(variable_current){
    
    print(variable_current)
    
    # select the good db
    db_clim<-list_db_clim[[variable_current]]
    lapse_rate_all_years<-list_lapse_rate[[variable_current]]
    
    # rename the "value" column in db_clim to avoid confusion
    setnames(db_clim,"value","value_cell")
    
    # compute climate through a loop over period
    periods=unique(db_clim$date)
    
    # enter the loop over periods
    db_output_var<-rbindlist(lapply(periods,function(period_selected){ 
      
      print(period_selected)
      
      lapse_rate_period<-lapse_rate_all_years[period==period_selected]
      
      db_copy<-merge(
        db,
        db_clim[date==period_selected,],
        by.x="safran_cell",by.y="cell")
      
      # db_copy[,lapse_rate:=extract(x=rast(lapse_rate_rasters_all_years[[period]]),y=db_copy[,.(x,y)])$lapse_rate] # add lapse-rate to the db
      db_copy[,lapse_rate:=lapse_rate_period$lapse_rate[match(db$symp,lapse_rate_period$homo_clim_area)]] # add lapse-rate to the db
      
      # compute final climate value
      
      db_copy[,value_stand := ifelse( 
        variable_current == "precipitation" & abs(delta_altitude) < 300,
        value_cell, # no lapse-rate correction for precipitations in low-lands (cf paper on FYRE)
        value_cell + lapse_rate*(ifn_altitude - safran_altitude) # in all other cases, lapse-rate correction
                                   )
              ] 
      
      if(variable_current == "precipitation"){ # we do not allow negative precipitation
        
        db_copy[, value_stand := pmax(0, value_cell)] 
        
      }
      
      return(db_copy[,.(stand ,period = date, value = value_stand)])
      
    })) # end the loop over periods
    
    db_output_var[,variable := variable_current]
    
    return(db_output_var)
    
  })) # end the loop over variables
    
  return(db_output)
  
}
