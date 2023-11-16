identify_missing.variables <- function(
  db_species_raw,
  correct.age,
  age_interval,
  height_min,
  climatic_variables,
  non_climatic_variables,
  information_variables,
  quadratic_climatic_variables,
  quadratic_non_climatic_variables,
  qualitative_variables
  ){
  
  db_ifn <- db_species_raw
  
  db_ifn[, ":="(
    Sp1.AGE13.HTOT = as.character(Sp1.AGE13.HTOT),
    Sp2.AGE13.HTOT = as.character(Sp2.AGE13.HTOT)
    )]
  
  # keep in mind stand number before any modification
  length_IFN_data_initial <- length(unique(db_ifn$stand))
  
  sp <- unique(db_ifn[,Sp1.AGE13.HTOT])
  
  print(sp)
  
  ######################
  # prepare age info
  #######################
  
  # correct age (to include the first years)
  if(correct.age==T){
    
    db_age_correction <- as.data.table(read.csv("data/age_correction/age_correction.csv"))
    db_age_correction$species <- str_remove_all(db_age_correction$species,'"')
    
    initial.height=0.5
    
    if(sp%in%db_age_correction$species){
      age.correction<-db_age_correction[species==sp,correction]
    }else if(as.numeric(str_sub(sp,1,2))<50){
      age.correction<-db_age_correction[species=="other_deciduous",correction]
    }else if(as.numeric(str_sub(sp,1,2))>=50){
      age.correction<-db_age_correction[species=="other_coniferous",correction]
    }
    
  }else{
    
    initial.height<-1.3
    age.correction<-0
    
  }
  
  data_age_height <- format_data_height_age(
    data = db_ifn,
    age.correction = age.correction
  )
  
  ################################################################################
  # remove stands too old compared to climate data
  ###################################################
    
  limit_year <- min(db_ifn$climatic_year)
  stands_too_old <- data_age_height[year_first < limit_year,stand]
  db_ifn <- db_ifn[!stand%in%stands_too_old]
  print(paste0(length(stands_too_old)," removed because too old compared to climate data (",limit_year,")"))
  
  length_IFN_not_too_old <- length(unique(db_ifn$stand)) # keep in mind stand number
  
  #####################################
  # Remove stands outside the age interval considered
  #######################################################
  
  if(!is.null(age_interval)){ # case when we want to restrict to some age interval
    
    stand_ok_age <- data_age_height[age>= age_interval[1] & age <= age_interval[2],stand]
    db_ifn <- db_ifn[stand %in% stand_ok_age]
    
  }
  
  length_IFN_age_interval <- length(unique(db_ifn$stand)) # keep in mind stand number
  
  #############################################
  # Remove stands with dominant height below a certain value
  ##########################################
  
  if(!is.null(height_min)){ # case when we want to restrict to some age interval
    
    stand_ok_height <- data_age_height[hfinal>=height_min,stand]
    db_ifn <- db_ifn[stand %in% stand_ok_height]
    
  }
  
  length_IFN_height_min<- length(unique(db_ifn$stand)) # keep in mind stand number
  

  ###########################################################
  # Separate explanatory variables and information variables
  ##########################################################
  
  # save the information variables
  keep_information_variables <- c("stand","climatic_year",unique(information_variables,qualitative_variables)) # add qualitative variables here: even if they are used in the calibration process, this enables to keep them as a unique variable with modalities, and not a group of binary variables
  db_information_variables <- db_ifn[,..keep_information_variables]
  
  # list explanatory variables
  explanatory_variables <- get_list_explanatory_variables( # "develop" the list of climatic variables
    db_ifn=db_ifn,
    climatic_variables=climatic_variables,
    non_climatic_variables=non_climatic_variables,
    quadratic_climatic_variables=NULL,
    quadratic_non_climatic_variables=NULL
  )
  
  # reduce db to these variables
  keep_cols <- c("stand","climatic_year",explanatory_variables)
  db_ifn <- db_ifn[,..keep_cols]

  
  ########################################
  # remove stand with missing variables
  ############################################
  
    # put NA if missing variables (may be necessary if character string imported as "" instead of NA)
    db_ifn[,(qualitative_variables) := lapply(.SD,function(x){
      
      return(ifelse(x!="",x,NA_character_))
      
    }),.SDcols=qualitative_variables]
    
  
    db.missing.variables <- rbindlist(lapply(setdiff(names(db_ifn), c("stand", "climatic_year")), function(variable.selected){
      
      print(variable.selected)
      
      db_ifn_temp <- db_ifn[,c("stand", variable.selected), with = FALSE]
      setnames(db_ifn_temp, variable.selected, "variable.selected")
      
      nb.stand.na <- length(unique(db_ifn_temp[is.na(variable.selected), stand]))
      
      return(
        data.table(
          variable = variable.selected,
          nb.stand.na = nb.stand.na
        )
      )
      
      }))
      db.missing.variables <- db.missing.variables[nb.stand.na != 0]
      
  return(db.missing.variables)
  
}
