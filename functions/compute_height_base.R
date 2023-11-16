compute_height_base <- function(
  param_A0,
  param_C0,
  param_beta0,
  param_alpha,
  param_gamma,
  names.param_alpha,
  names.param_gamma,
  data_explanatory,
  initial.height,
  data_age_height
  ){
  
  data_age_height[, year_first := year_observation - age + 1] # first year after stand establishment (ie first year for which we compute an new heght after the initial height)
  
  # check consistency between stand period and climate period
  if(max(data_age_height$year_observation) > max(data_explanatory$climatic_year) | min(data_age_height$year_first) < min(data_explanatory$climatic_year)){
    stop(" data_age_height covers a larger time period than climate data")
  }
  
  # filter for the relevant years
  data_explanatory <- merge(data_explanatory, data_age_height[, .(stand, year_first, year_observation)], by = "stand")
  data_explanatory <- data_explanatory[climatic_year >= year_first & climatic_year <= year_observation]
  
  # ensure that the envi db is order by stand and then by climatic year and that db_explanatory and data_age_height have the same stand order
  keyv <- c("stand", "climatic_year")
  setkeyv(data_explanatory, keyv)
  setkeyv(data_age_height, "stand")
  if(!identical(unique(data_explanatory$stand), as.integer(data_age_height$stand))){ stop("pb stand order")} # check stand order
  
  # compute alpha and gamma matrix (ie combining parameter values and explanatory variables values)
  
    # compute alpha matrix
    if(length(param_alpha) > 0){
      m.alpha <- as.matrix(data_explanatory)[, names.param_alpha, drop = FALSE]
      alpha <- logistic(as.matrix(m.alpha) %*% param_alpha) # "as.matrix" in case we have only one explanatory variable
    }else{
      alpha <- rep(1/2,times = nrow(data_explanatory)) 
    }
    
    # compute gamma matrix
    if(length(param_gamma)>0){
      m.gamma <- as.matrix(data_explanatory)[, names.param_gamma, drop = FALSE]
      gamma <- logistic(as.matrix(m.gamma) %*% param_gamma) # "as.matrix" in case we have only one explanatory variable
    }else{
      gamma <- rep(1/2, times = nrow(data_explanatory)) 
    }


  # about stands
  stand_name <- unique(data_explanatory$stand)
  stand_age <- data_age_height$age
  nb_stands <- length(stand_name)
  
  # initialization 

  db_Hp_dynamics<- data.table(stand=numeric(),
                                 year=numeric(),
                                 Hp = numeric() # "Hp" stands for "(dominant) height, pure (stand)"
              )
  index_global <- 1
  
  for (index_stand in 1:nb_stands) { # loop over stands

    # initialization for this stand
    
    Hp_stand <- initial.height
    year_stand <- data_age_height$year_first[index_stand]-1
    Hp_dynamics_stand <- Hp_stand
    vector_year_stand <- year_stand

    for (i in 1:stand_age[index_stand]) { # loop over years

      increment <-   param_A0 * alpha[index_global] * Hp_stand ^ param_beta0 - param_C0 * gamma[index_global] * Hp_stand    
      Hp_stand <- Hp_stand + max(0, increment) # we consider that the increment in year t is due to the condition in year t
      year_stand <- year_stand + 1
      
      Hp_dynamics_stand <- c(Hp_dynamics_stand,Hp_stand)
      vector_year_stand <- c(vector_year_stand,year_stand)
      
      index_global<-index_global+1
    }
    
    db_Hp_dynamics_stand <- data.table(
      stand=stand_name[index_stand],
      year=vector_year_stand,
      Hp = Hp_dynamics_stand 
      )
    
    db_Hp_dynamics <- rbind(db_Hp_dynamics,db_Hp_dynamics_stand)

  }

  return(db_Hp_dynamics)
  
}
