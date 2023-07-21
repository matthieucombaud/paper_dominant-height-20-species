compute_height <- function(db_parameter,data_explanatory,initial.height,data_age_height){
  
  if(max(data_age_height$year_observation) > max(data_explanatory$climatic_year) | min(data_age_height$year_first) < min(data_explanatory$climatic_year)){
    
    stop(" data_age_height covers a larger time period than climate data")
    
  }
  
  # check stands are in the same order
  
  stand.order_explanatory <- unique(data_explanatory$stand)
  stand.order_height <- unique(data_age_height$stand)
  
  if(length(setdiff(stand.order_explanatory, stand.order_height)) > 0 |
     length(setdiff(stand.order_height, stand.order_explanatory)) > 0 ){
    stop("not the same stands")
  }
  
  if(!identical(stand.order_explanatory, stand.order_height)){ # reorder the database
    
    data_age_height <- data_age_height[match(stand.order_explanatory, stand)]
    print("data_age_height has been reordered to match data_explanatory stand order")
    
  }
  
  # filter for the relevant years
  data_explanatory_filtered <- rbindlist(lapply(data_age_height$stand, function(stand_selected){
    
    year_first <- data_age_height[stand == stand_selected,year_first]
    year_final <- data_age_height[stand == stand_selected,year_observation]
    
    return(data_explanatory[stand==stand_selected & climatic_year %in% c(year_first:year_final)])
    
  }))
  
  keys <- c("stand","climatic_year")
  setkeyv(data_explanatory_filtered,keys)
  
  # format data
  data_explanatory_formated <- format_data_explanatory(
    data = data_explanatory_filtered,
    param_alpha = db_parameter[param_class=="alpha",param],
    param_gamma = db_parameter[param_class=="gamma",param],
    param_beta = db_parameter[param_class=="beta" & param != "intercept",param] # intercept is added manually
  )
  m.alpha <- data_explanatory_formated$alpha
  m.gamma <- data_explanatory_formated$gamma
  m.beta <- data_explanatory_formated$beta
  
  # format intercept order to be able to use the "compute_alpha_beta_gamma" function
  param_intercept <- c(db_parameter[param=="A0",estimate],db_parameter[param=="C0",estimate]) # important to have the right order for this 3 param
  param_alpha = db_parameter[param_class=="alpha",estimate] # order not important, because the columns of data_explanatory_formated are ordered accordingly
  param_gamma = db_parameter[param_class=="gamma",estimate] # order not important, because the columns of data_explanatory_formated are ordered accordingly
  param_beta = c(db_parameter[param_class=="beta" & param == "intercept",estimate],db_parameter[param_class=="beta" & param != "intercept",estimate]) # to be sure intercept comes first (because it is added in first position in "format_data_explanatory")
  
  # compute alpha and gamma matrix (ie combining parameter values and explanatory variables values)
  alpha_beta_gamma <- compute_alpha_beta_gamma(
    param_intercept=param_intercept, # attention: param should be in the right order (A0, beta, C0): maybe not be the case, especially if a factor as been introduced in the datatable (reordering)
    param_alpha=param_alpha, 
    param_gamma=param_gamma,
    param_beta=param_beta,
    m.alpha=m.alpha,
    m.gamma=m.gamma,
    m.beta=m.beta
  )
  
  alpha<-alpha_beta_gamma$alpha
  beta<-alpha_beta_gamma$beta
  gamma<-alpha_beta_gamma$gamma
  
  # about stands
  stand_name<-unique(data_explanatory_filtered$stand)
  stand_age<-data_age_height$age
  nb_stands<-length(stand_name)
  
  # initilisation 

  db_height_dynamics<- data.table(stand=numeric(),
                                 year=numeric(),
                                 age=numeric(),
                                 height=numeric(),
                                 height_asymptotic=numeric()
              )
  index_global <- 1
  
  for (index_stand in 1:nb_stands) {

    # initialization for this stand
    
    height_stand <- initial.height
    year_stand <- data_age_height$year_first[index_stand]-1
    age_stand <- 0
    
    heights_dynamics_stand <- height_stand
    height_asymptotic_stand <- c(NA)
    vector_year_stand <- year_stand
    vector_age_stand <- age_stand

    
    for (i in 1:stand_age[index_stand]) {

      height_stand <- height_stand + max(0, alpha[index_global]*height_stand^beta[index_global] - gamma[index_global]*height_stand) # we consider that the increment in year t is due to the condition in year t
      age_stand <- age_stand + 1
      year_stand <- year_stand + 1
      
      heights_dynamics_stand <- c(heights_dynamics_stand,height_stand)
      height_asymptotic_stand <- c(height_asymptotic_stand,(alpha[index_global]/gamma[index_global])^(1/(1-beta[index_global])))
      vector_age_stand <- c(vector_age_stand,age_stand)
      vector_year_stand <- c(vector_year_stand,year_stand)
      
      index_global<-index_global+1
    }
    
    db_height_dynamics_stand <- data.table(
      stand=stand_name[index_stand],
      year=vector_year_stand,
      age=vector_age_stand,
      height=heights_dynamics_stand,
      height_asymptotic=height_asymptotic_stand
      )
    
    db_height_dynamics <- rbind(db_height_dynamics,db_height_dynamics_stand)

  }

  return(db_height_dynamics)
  
}
