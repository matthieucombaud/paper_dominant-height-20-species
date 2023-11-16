describe_nonclimatic.data <- function(
  species_list,
  db_species.name,
  dir.data.calibration,
  db.ifn.complete
){
  
  # get database
  list_stand <- lapply(species_list, function(species.selected){
    
    list_data.explanatory <- readRDS(paste0(dir.data.calibration,"/db_for_inference_sp",species.selected,".rds"))
    list_stand <- unique(list_data.explanatory$db_explanatory_normalized_complete$stand)
    return(list_stand)
    
  })
  list_stand <- unlist(list_stand)
  
  db.ifn.restricted <- db.ifn.complete[stand %in% list_stand]
  db.ifn.restricted[, radiation_annual := RAD_C01 + RAD_C02+RAD_C03 + RAD_C04 + RAD_C05  +RAD_C06 + RAD_C07+  RAD_C08 + RAD_C09+RAD_C10+RAD_C11+RAD_C12]
  
  
  # quantitative variables
  
  #create output format for quanti variables
  
    db_variables.quanti <- rbind(
      data.table(variable = "pH", unit = "-", name = "pH"),
      data.table(variable = "CN", unit = "-", name = "C:N"),
      # data.table(variable = "ST", unit = "-", name = "S:T"),
      data.table(variable = "P2O5", unit = "-", name = "P2O5"),
      data.table(variable = "soil_depth", unit = "dm", name = "soil depth"),
      data.table(variable = "affroc_perc", unit = "%", name = "rock emergence"),
      data.table(variable = "cailloux_perc", unit = "%", name = "rock presence"),
      data.table(variable = "slope", unit = "%", name = "slope"),
      # data.table(variable = "waterlogging_permanent", unit = "-", name = "permanent waterlogging"),
      # data.table(variable = "waterlogging_temporary", unit = "-", name = "temporary waterlogging"),
      data.table(variable = "expo_NS", unit = "1: North, -1 : South", name = "aspect (North-South)"),
      data.table(variable = "expo_EW", unit = "1: East, -1 : West", name = "aspect (East-West)"),
      data.table(variable = "SWHC", unit = "mm", name = "SWHC"),
      data.table(variable = "radiation_annual", unit = "MJ/m2", name = "annual radiations")
    )
  
    db_summary_quanti <- rbindlist(lapply(db_variables.quanti$variable, function(variable.selected){
      
      db.ifn.restricted_temp <- db.ifn.restricted[,variable.selected, with = FALSE]
      setnames(db.ifn.restricted_temp, variable.selected, "variable.selected.named")
      
      db_summary_variable.selected <- data.table(
        variable = variable.selected,
        min = round(db.ifn.restricted_temp[,min(variable.selected.named)], 1),
        median = round(db.ifn.restricted_temp[,median(variable.selected.named)], 1),
        max = round(db.ifn.restricted_temp[,max(variable.selected.named)], 1),
        sd = round(db.ifn.restricted_temp[,sd(variable.selected.named)], 1)
      )
      
    }))
    
    db_variables.quanti <- merge(db_variables.quanti, db_summary_quanti, by = "variable")
    
  # create output format for quali variables
  db_variables.quali <- rbind(
    data.table(variable = "ROCHE.Calc", distribution = character()),
    data.table(variable = "soil_type", distribution = character()),
    data.table(variable = "humus_type", distribution = character())
  )
  
  list_quali.tables <- lapply(db_variables.quali$variable, function(variable.selected){
    
    table(db.ifn.restricted[,variable.selected, with = FALSE])
    
  })
  names(list_quali.tables) <- db_variables.quali$variable
  
  return(list(
    quanti = db_variables.quanti, 
    quali = list_quali.tables
  ))
  
}
