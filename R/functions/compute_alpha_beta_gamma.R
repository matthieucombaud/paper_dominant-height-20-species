compute_alpha_beta_gamma <- function(param_intercept,param_alpha,param_gamma,param_beta,m.alpha,m.gamma,m.beta){
  
  if(length(param_alpha)>0){
    alpha <- param_intercept[1]*logistic(as.matrix(m.alpha) %*% param_alpha) # "as.matrix" in case we have only one explanatory variable
  }else{
    alpha <- rep(param_intercept[1],times=dim(m.alpha)[1]) 
  }
  
  if(length(param_gamma)>0){
    gamma <- param_intercept[2]*logistic(as.matrix(m.gamma) %*% param_gamma) # "as.matrix" in case we have only one explanatory variable
  }else{
    gamma <- rep(param_intercept[2],times=dim(m.gamma)[1]) 
  }
  
  beta <- as.matrix(m.beta) %*% param_beta
  
  return(list(alpha=alpha,beta=beta,gamma=gamma))
  
}
