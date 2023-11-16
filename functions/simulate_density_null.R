simulate_density_null <- function(
  par, # parameter vector
  list_data_explanatory_formated,
  db.age.height,
  db_trait,
  parameter.pur,
  height.initial,
  no_cores,
  t_ori.force.1 = NULL, # to force origin year (must be activated at the same time as t_obs_force)
  t_ori.force.2 = NULL, # to force origin year (must be activated at the same time as t_obs_force)
  t_obs.force = NULL # to force observation year (must be activated at the same time as t_ori_force)
){
  
  # required packages
  {
    require(foreach)
    require(doParallel)
  }
  
  ### Idea of the code: I work stand by stand to make the code clearer (not problem since not a lot of stands)

  
  # stand list
  stand.all <- unique(db.age.height$stand) 
  
  # prepare parallelization
  
  no_cores <- no_cores
  cl <- makeCluster(no_cores)  
  registerDoParallel(cl) 
  required_functions <- c(
    "compute_height",
    "format_data_explanatory",
    "compute_alpha_beta_gamma"
  )
  required_packages<-c("data.table","stringr", "psych")
  
  db.modeled <- foreach(stand.selected = stand.all, .combine = "rbind",.packages=required_packages,.export=required_functions) %dopar% {
  
  # db.modeled <- rbindlist(lapply(stand.all, function(stand.selected){ # compute dynamics in mixed stands for each stand, assuming a given valu of the parameters. 

    # get sp name
    sp1.selected <- db.age.height[stand == stand.selected, sp1]
    sp2.selected <- db.age.height[stand == stand.selected, sp2]
    
    # get species proportion
    prop_sp1 <- db.age.height[stand == stand.selected, Gupperr_sp1]
    prop_sp2 <- db.age.height[stand == stand.selected, Gupperr_sp2]
    
    # get explanatory data for both species
    data_explanatory_formated_sp1 <- list_data_explanatory_formated[[sp1.selected]][stand == stand.selected]
    data_explanatory_formated_sp2 <- list_data_explanatory_formated[[sp2.selected]][stand == stand.selected]
    
    
    # prepare db giving species age
    
    if(!is.null(t_ori.force.1) | !is.null(t_ori.force.2) | !is.null(t_obs.force)){ # we force t_ori and t_obs
  
      db.age.height <- db.age.height[stand ==  stand.selected, ":="(
        year_observation = t_obs.force,
        age_sp1 = t_obs.force - t_ori.force.1,
        age_sp2 = t_obs.force - t_ori.force.2
      )][, setdiff(colnames(db.age.height), c("h_sp1", "h_sp2")), with = FALSE]
      
    }
    
    
    t_ori_sp1 <- db.age.height[stand == stand.selected, year_observation - age_sp1]
    t_ori_sp2 <- db.age.height[stand == stand.selected, year_observation - age_sp2]
    t_ori_oldest <- min(t_ori_sp1, t_ori_sp2)
    t_ori_youngest <- max(t_ori_sp1, t_ori_sp2)
    t_obs <- db.age.height[stand == stand.selected, year_observation]
    
    sp.oldest <- ifelse(t_ori_sp1 < t_ori_sp2, sp1.selected, sp2.selected)
    sp.youngest <- ifelse(t_ori_sp1 < t_ori_sp2, sp2.selected, sp1.selected)
    if(t_ori_sp1 == t_ori_sp2){
      sp.oldest <- sp.youngest <- NA
    }
    
    # prepare data_age_height for both species
    
    db_age_height_sp1 <- db.age.height[stand ==  stand.selected, .(
      stand, 
      year_observation,
      age = age_sp1,
      year_first = year_observation - age_sp1 + 1
    )
    ]
    
    db_age_height_sp2 <- db.age.height[stand ==  stand.selected, .(
      stand, 
      year_observation,
      age = age_sp2,
      year_first = year_observation - age_sp2 + 1
    )
    ]

    # initialize dyna
    DH_m_1 <- height.initial ######################
    DH_m_2 <- height.initial ######################

    # prepare vector to keep in mind dyna
    DH_m_1.dyna <- c(DH_m_1) ################
    DH_m_2.dyna <- c(DH_m_2) ################
    
    # iteration
    
    ### Idea of the code: on part des hauteurs simulées en mélange l'année t-1, et on utilise la formule d'incrément annuel en peuplement pur pour calculer l'accroissement associé en peuplement pur
    
    
    
    # years between the origin year of the oldest species and the origin year of the youngest species: we compute dynamics as if the oldest species was in pure stand
    
    if(t_ori_oldest != t_ori_youngest){
      
      if(sp.oldest == sp1.selected){
        
        dyna_pur_1 <- compute_height( # dyna sp1 in pure stand (not only incrcement, to be able to use the function "compute_height")
          db_parameter = parameter.pur[[sp1.selected]],
          data_explanatory = data_explanatory_formated_sp1,
          initial.height = DH_m_1, # height in mixed stand at the beginning of the period
          data_age_height = data.table(
            stand = stand.selected, 
            year_observation = t_ori_sp2,
            age = t_ori_sp2 - t_ori_sp1, # we look only one year after
            year_first = c(t_ori_sp1 + 1)
          )
        )
        
        DH_m_1 <- dyna_pur_1[year == t_ori_sp2, height]
        
        DH_m_1.dyna <- dyna_pur_1[, height] # update DH_m_1
        DH_m_2.dyna <- rep(DH_m_2, length = t_ori_sp2 - t_ori_sp1 + 1)
      }
      

      if(sp.oldest == sp2.selected){
        
        dyna_pur_2 <- compute_height( # dyna sp1 in pure stand (not only incrcement, to be able to use the function "compute_height")
          db_parameter = parameter.pur[[sp2.selected]],
          data_explanatory = data_explanatory_formated_sp2,
          initial.height = DH_m_2, # height in mixed stand at the beginning of the period
          data_age_height = data.table(
            stand = stand.selected, 
            year_observation = t_ori_sp1,
            age = t_ori_sp1 - t_ori_sp2, # we look only one year after
            year_first = c(t_ori_sp2 + 1)
          )
        )
        
        DH_m_2 <- dyna_pur_2[year == t_ori_sp1, height]
        
        DH_m_1.dyna <- rep(DH_m_1, length = t_ori_sp1 - t_ori_sp2 + 1)
        DH_m_2.dyna <- dyna_pur_2[, height] # update DH_m_2
      
      }
      
    }
    
    # years after the origin year of the youngest species
    
    for(year.selected in c(c(t_ori_youngest + 1): t_obs)){ # iterate over year (need to have a doulbe "c", else problem in the vector of year)
      
      # computation of mixture effect (before updating DH_m_i)
      
        # ME of sp2 over sp1
        gDH_1 <- (DH_m_2 - DH_m_1) / DH_m_1 # 'gDH' means "gap between dominant heights"

        ME_1 <- par["mu"] * (1 - prop_sp1)
        
        # ME of sp1 over sp2
        gDH_2 <- (DH_m_1 - DH_m_2) / DH_m_2
        
        ME_2 <- par["mu"] * (1 - prop_sp2)

      
      # species 1
      dyna_pur_1 <- compute_height( # dyna sp1 in pure stand (not only incrcement, to be able to use the function "compute_height")
        db_parameter = parameter.pur[[sp1.selected]],
        data_explanatory = data_explanatory_formated_sp1,
        initial.height = DH_m_1, # height in mixed stand at the beginning of the period
        data_age_height = data.table(
          stand = stand.selected, 
          year_observation = year.selected,
          age = 1, # we look only one year after
          year_first = year.selected
        )
      )
      
      iDH_pur_1 <- dyna_pur_1[2, height] - dyna_pur_1[1, height] # increment for sp1 in pure stand. 'iDH' means "increment of dominant height"

      DH_m_1 <- DH_m_1 + iDH_pur_1 * max(0, (1 + ME_1)) # max to ensure no height decrease
      # DH_m_1.dyna <- c(DH_m_1.dyna, DH_m_1)
      
      # species 2
      dyna_pur_2 <- compute_height( # dyna sp1 in pure stand (not only incrcement, to be able to use the function "compute_height")
        db_parameter = parameter.pur[[sp2.selected]],
        data_explanatory = data_explanatory_formated_sp2,
        initial.height = DH_m_2, # height in mixed stand at the beginning of the period
        data_age_height = data.table(
          stand = stand.selected, 
          year_observation = year.selected,
          age = 1, # we look only one year after
          year_first = year.selected
        )
      )
      
      iDH_pur_2 <- dyna_pur_2[2, height] - dyna_pur_2[1, height] # increment for sp1 in pure stand
      
      DH_m_2 <- DH_m_2 + iDH_pur_2  * max(0, (1 + ME_2)) # max to ensure no height decrease
      # DH_m_2.dyna <- c(DH_m_2.dyna, DH_m_2)
    
      
      # save dynamics
      DH_m_1.dyna <- c(DH_m_1.dyna, DH_m_1)
      DH_m_2.dyna <- c(DH_m_2.dyna, DH_m_2)
      
    } # end "for-loop"
    
    # return final values
    return(data.table( 
      stand = stand.selected,
      year = c(t_ori_oldest : t_obs),
      DH_m_1.dyna = DH_m_1.dyna,
      DH_m_2.dyna = DH_m_2.dyna
    ))
    
  }
  stopCluster(cl)
  
  return(db.modeled)
  
}
