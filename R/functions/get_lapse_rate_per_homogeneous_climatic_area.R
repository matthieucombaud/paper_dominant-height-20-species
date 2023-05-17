get_lapse_rate_per_homogeneous_climatic_area<-function(db_homogeneous_climatic_areas,distance,db_clim_long){

  periods<-unique(db_clim_long$date) # list periods

  db_lapse_rate <- rbindlist(lapply(periods,function(period){
  
    print(period)
    
    db_climate <- db_clim_long[date==period][,c("cell","value")] # restrict db to the relevant period
    db_climate <- merge(db_climate,db_homogeneous_climatic_areas,by="cell") #create correspondance homogeneous climatic area / safran cell number. Remove cell not in France
    
    db_lapse_rate<-rbindlist(lapply(unique(db_climate$symp),function(homo_clim_area){
     
      db_climate_temp<-db_climate[symp==homo_clim_area]
      
      if(dim(db_climate_temp)[1]>2){ # case where we have more than 2 cells in the homogeneous climatic area to do the regression
        
        model<-lm(data=db_climate_temp,value~alt)
        return(data.table(homo_clim_area=homo_clim_area,lapse_rate=summary(model)$coefficients[2,1],p_value=summary(model)$coefficients[2,4],proxy_area=NA))
        
      }else{ # case where we have less than 2 Safran cells in the homogeneous area (not really embarassing to have a raw approximation of the gradient since no a lot of IFN point there, but still : I take the mean of all the lapse-rate of the closest homoclim areas)
       
        min_dist <- min(distance[which(rownames(distance)==homo_clim_area),],na.rm=T)
        index_homo_clim_area<-which(distance[rownames(distance)==homo_clim_area,]<=min_dist+1) # index of the homo clim area (not code of the homo clim area) # 1 km margin
        
        proxy_area<-colnames(distance)[index_homo_clim_area]
        
        return(data.table(homo_clim_area=homo_clim_area,lapse_rate=NA,p_value=NA,proxy_area=paste0(proxy_area,collapse="_")))
      
      }
      

    }))
    
    # case of NA
    iteration<-0
    while(length(db_lapse_rate[is.na(lapse_rate),homo_clim_area])>0 & iteration<=1){ # we limit to 2 iterations (enough)
      
      iteration<-iteration+1
      
      homo_clim_area_pb<-db_lapse_rate[is.na(lapse_rate),homo_clim_area]
      
      for(homo_clim_area_selected in homo_clim_area_pb){
        
        db_lapse_rate[homo_clim_area==homo_clim_area_selected,lapse_rate:=get_lapse_rate_proxy(homo_clim_area_selected=homo_clim_area_selected,db_lapse_rate=db_lapse_rate)]
        
      }
      
    }
    
    
    # add period
    db_lapse_rate$period<-period
    
    return(db_lapse_rate)
    
  })) # end for this period
    
  return(db_lapse_rate)

}
