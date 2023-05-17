prepare_db_for_inference <- function(
  db_species_raw,
  correct.age,
  age_interval,
  height_min,
  climatic_variables,
  non_climatic_variables,
  information_variables,
  quadratic_climatic_variables,
  quadratic_non_climatic_variables,
  qualitative_variables,
  output_folder,
  db_greco_grouping
  ){
  
  dir.create(output_folder,recursive = T)
  
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
    
    uncomplete_stands <- unique(db_ifn[which(!complete.cases(db_ifn)),stand]) # need to exclude all the stand, not just some years for a given stand
    
    db_ifn <- db_ifn[!stand %in% uncomplete_stands]
    print(paste0(length(uncomplete_stands)," stands removed from the db because explanatory data are uncomplete"))
    length_IFN_data_final <- length(unique(db_ifn$stand)) 
    
    print(paste0("final nb of stands = ",length_IFN_data_final))
    
    if(length_IFN_data_final == 0){ # then we quit here
      return("no stand remains after filtering for missing variables")
    }
    
  
  ##################################
  # remove non-varying variables (else may hamper convergence)
  ###############################  
    
    # attention:
    # "no variation" should not be the case for quantitative variables, we do it for categorical variables.
    # here we remove only variables for which we have no variation, latter we do it for variables we "not enough variation", define after grouping some low-number categories (for categorical variables)
    
  count_unique <- sapply(explanatory_variables,function(x){
    
    dim(unique(db_ifn[,..x]))[1]
    
  })
  
  variables_not_varying <- names(count_unique[count_unique==1])
  
  if(length(variables_not_varying)>0){
    
    db_ifn[,(variables_not_varying):=NULL]
    explanatory_variables <- setdiff(explanatory_variables,variables_not_varying)
    qualitative_variables <- setdiff(qualitative_variables,variables_not_varying)
    
  }
  
  ###################################
  # Normalize quantitative variables
  #####################################
  
    # Attention:
    # 1/ do it before adding the square variable (so that that we can really interpret the square variable as the square of the variable in the model
    # 2/ do it on the database with all years for all stands, even if the age stand if inferior to the temporal depth of the data base, to ensure we have a balanced normalization)  
    # 3/ do the normalization after all stands that should be excluded are actually excluded
  
    col_to_normalize <- setdiff(explanatory_variables,qualitative_variables)
    
    # compute normalization parameters : mean and sd computed over the whole db (not the reduced one)
    db_normalization_constant <- db_ifn[,.(variable=col_to_normalize,mean=sapply(.SD, function(x) (mean(x,na.rm=T))),sd=sapply(.SD, function(x) (sd(x,na.rm=T)))), .SDcols = col_to_normalize]
    
    # do normalization
    db_ifn[,(col_to_normalize):=lapply(.SD, function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)), .SDcols = col_to_normalize]
    

  ###########################
  # add square variables
  ###################################
      
    add_square_variables( # no affectation required, directly modify db_explanatory_normalized
      db=db_ifn,
      quadratic_climatic_variables=quadratic_climatic_variables,
      quadratic_non_climatic_variables=quadratic_non_climatic_variables
    )
    
    
  ########################################
  # reclassify categorical variables
  ########################################
    
    
  # group GRECO
  if("GRECO" %in% non_climatic_variables){
    
    db_temp <- merge(db_ifn,db_greco_grouping,by="GRECO")
    db_temp <- db_temp[,GRECO := GRECO_group]
    db_temp <- db_temp[,GRECO_group := NULL]
    
    db_ifn <- db_temp
    
  }
    
  if(length(qualitative_variables) > 0){
    
    tab_categorical_var <- lapply(qualitative_variables,function(x){
      keep.cols=c("stand",x)
      table(unique(db_ifn[,..keep.cols])[,..x],useNA="always")
    })
    
    min_pct_per_group <- 0.0 # min share of the sample in each group
    min_abs_per_group <- 10 # min stand number per group
    variables_categorical <- character()
    
    for(variable_selected in qualitative_variables){ # not lapply, to modify db_ifn iteratively
      
      temp <- reclassify_categorical_variables(
        db=copy(db_ifn),
        variable=variable_selected,
        min_pct_per_group=min_pct_per_group,
        min_abs_per_group=min_abs_per_group
      )
      
      db_ifn <- copy(temp$db)
      
      variables_categorical <- c(variables_categorical,temp$variables_new)
      variables_not_varying <- c(variables_not_varying,temp$variables_not_varying)
      
    }
    
  }else{
    
    tab_categorical_var <- NULL
    variables_categorical <- NULL
    
  }
    
  # print variables that do not vary enough
  print(paste0("variables '",paste(variables_not_varying,collapse = " "),"' removed from the analysis because they do not vary enough"))
  
  # remove variables that do not vary
  explanatory_variables <- setdiff(explanatory_variables,variables_not_varying)
    
  ##########################
  # Final adjustements
  #########################
    
    # remove useless stands in data_age_height 
    data_age_height <- data_age_height[stand %in% unique(db_ifn$stand)]
    
    # keep only climatic years for which the stand was present, according to the measured trees (necessary for the c++ parameter inference algorithm)
    db_ifn_reduced <- filter_living_period(data_explanatory=db_ifn,data_age_height=data_age_height)
    
    # to avoid mistakes, ensure database are ordered by stands and then years
    keys_1<-c("stand","climatic_year")
    setkeyv(db_ifn,keys_1)
    setkeyv(db_ifn_reduced,keys_1)
    
    keys_2<-c("stand")
    setkeyv(data_age_height,keys_2)
    
  
  print("db preparation ok")
  
  output <- list(
    species = sp,
    correct.age = correct.age,
    age_interval = age_interval,
    initial.height = initial.height,
    explanatory_variables = explanatory_variables,
    db_explanatory_normalized_reduced=db_ifn_reduced,
    db_explanatory_normalized_complete=db_ifn,
    data_age_height=data_age_height,
    db_information_variables=db_information_variables,
    db_normalization_constant=db_normalization_constant,
    stand_number=c(
      length_IFN_data_initial = length_IFN_data_initial,
      length_IFN_not_too_old = length_IFN_not_too_old,
      length_IFN_age_interval = length_IFN_age_interval,
      length_IFN_height_min = length_IFN_height_min,
      length_IFN_data_final=length_IFN_data_final
    ),
    tab_categorical_var=tab_categorical_var,
    variables_not_varying=variables_not_varying
  )
  
  write_rds(output,paste0(output_folder,"/db_for_inference_sp",sp,".rds"))
  
  return(output)
  
}
