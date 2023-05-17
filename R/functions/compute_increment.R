compute_increment<- function(db_parameter,data_explanatory,height){
  
  # format data
  data_explanatory_formated <- format_data_explanatory(
    data = data_explanatory,
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
  alpha_beta_gamma <- compute_alpha_beta_gamma(param_intercept=param_intercept, # attention: param should be in the right order (A0, beta, C0): maybe not be the case, especially if a factor as been introduced in the datatable (reordering)
                                             param_alpha=param_alpha, 
                                             param_gamma=param_gamma,
                                             param_beta=param_beta,
                                             m.alpha=m.alpha,
                                             m.gamma=m.gamma,
                                             m.beta = m.beta
  )
  
  alpha<-alpha_beta_gamma$alpha
  beta<-alpha_beta_gamma$beta
  gamma<-alpha_beta_gamma$gamma
  
  # vector of increment
  increment_brut <-  alpha%*%t(height^beta) - gamma%*%t(height) # we consider that the increment in year t is due to the condition in year t
  increment<-increment_brut
  increment[increment<0]<-0
  
  colnames(increment) <- height
  
  increment<-cbind(envi_condition=seq(1,dim(increment)[1],1),stand=data_explanatory$stand,climatic_year=data_explanatory$climatic_year,as.data.table(increment))
  increment<-melt(data=increment,id.vars=c("envi_condition","climatic_year","stand"),value.name="increment",variable.name = "height",variable.factor = F)
  increment$height<-as.numeric(increment$height)
  
  return(increment)
  
}
