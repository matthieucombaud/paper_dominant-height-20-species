divide.stands <- function(
  nb.batch,
  list_data.inference,
  list.species
){
  
  c.group <- 1:nb.batch

  list.data.batch.2 <- lapply(list.species, function(sp.sel){
    
    # all stand of the species
    stand.sel <- unique(list_data.inference[[sp.sel]]$data_age_height$stand)
    
    # create a db to allocate each stand to a group
    db_allocation <- data.table(stand=stand.sel,group=integer())
    
    # allocate each stand to a group
    stand_pool<-db_allocation$stand
    nb_stands<-length(stand_pool)
    size_group<-nb_stands%/%nb.batch
    rest<-nb_stands%%nb.batch
    
    for(k in c.group){
      
      if(rest>0){ # allocate the additional stands
        size<-size_group+1
        rest<-rest-1
      }else{
        size<-size_group
      }
      
      set.seed(123)
      stand_selected<-sample(stand_pool,size=size,replace=F)
      stand_pool<-setdiff(stand_pool,stand_selected) # update the list of non-affected stands
      db_allocation[stand%in%stand_selected,group:=k]
      
    } #end "for"
    
    # get list of data, one per batch
    
    list.data.batch <- lapply(c.group, function(group.sel){
      
      list_data.inference_sel <- list_data.inference[[sp.sel]]
      list_data.inference_sel$data_age_height <- list_data.inference_sel$data_age_height[!stand %in% db_allocation[group == group.sel, stand]]
      list_data.inference_sel$db_explanatory_normalized_reduced <- list_data.inference_sel$db_explanatory_normalized_reduced[!stand %in% db_allocation[group == group.sel, stand]]
      list_data.inference_sel$db_explanatory_normalized_complete<- list_data.inference_sel$db_explanatory_normalized_complete[!stand %in% db_allocation[group == group.sel, stand]]
      
      return(list_data.inference_sel)
      
    })
    names(list.data.batch) <- c.group
  
    return(list.data.batch)
    
  }) # end "lapply"
  names(list.data.batch.2) <- list.species

  return(list.data.batch.2)

}
