reclassify_categorical_variables_no_deletion <- function(db,variable,min_pct_per_group,min_abs_per_group){
  
  setnames(db,variable,"group")
  
  db_temp <- unique(db[,.(stand,group)])
  
  min_number_per_group <- round(max(min_pct_per_group * dim(db_temp)[1],min_abs_per_group))
  
  db_number <- db_temp[!is.na(group),.(N=.N),by=group] # to avoid to put the "NA" in "other"
  db_number <- db_number[order(db_number$N)]
  db_number[,Ncum := cumsum(N)]
  db_number[, group_corrected := ifelse(Ncum > min_number_per_group, group, "other")] # no need to take NA into account since they have been removed

  if( "other" %in% db_number$group_corrected ){ # needed to implement a correction to ensure that the "other" group is big enough. Idea: we add a supplementary group to ensure that the "other" group is big enough

    if(db_number[group_corrected == "other",max(N)] < min_number_per_group){ # Attention : the condition "db_number[group_corrected == "other",max(N)] < min_number_per_group" may not be fulfilled if there is from the beginning a modality called "other" with enough point. In this case, no correction should be applied
      
      last_group <- db_number[group_corrected != "other"][1,group] # last group to label as "other" to reach the minimum level
      db_number[group==last_group,group_corrected := "other"]
      
    }
    
  }
  
  # if(length(unique(db_number$group_corrected)) == 1){ # then we remove the variable because not varying after reclassification: NO! not when we use a precalibrated model, only when we calibrate a new model!
  #   
  #   
  #   db[,group := NULL] 
  #   variables_new <- NULL
  #   variables_not_varying <- variable
  #   
  #   
  # }else{
    
    db_temp <- merge(db_temp,db_number[,.(group,group_corrected)],by="group",all.x=T)[,.(stand,group_corrected)] # all.x=T to keep the NA (possible for "information variables")
    
    db_temp[,group_corrected:=as.character(group_corrected)] # to attribute a "no_value"  value, for NA cases
    
    db_temp[is.na(group_corrected),group_corrected:="no_value"]
    
    # create a variable per modality
    db_temp <- dcast(data = db_temp,formula="stand ~ group_corrected",value.var="group_corrected")
    
    names(db_temp)[-1] <- paste0(variable,"_",names(db_temp)[-1],"_categorical") # important to respect this order, since the name will be used in the variables selection process to identify if it is a categorical variable and which categorical variable it is
    
    variables_new <- names(db_temp)[-1]
    db_temp[,(variables_new):=lapply(.SD,function(x){ifelse(is.na(x),0,1)}),.SDcols=variables_new]
    
    # merge
    db <- merge(db,db_temp,by="stand")
    
    db[,group := NULL]
    
    variables_not_varying <- NULL
    
    
  # }
  
  return(list(db=db,variables_new=variables_new,variables_not_varying=variables_not_varying))
  
}
