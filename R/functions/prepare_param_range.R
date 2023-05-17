prepare_param_range <- function(A0,C0,alpha,gamma,beta,sigma,delta,parameters){
  
  # attention: order of parameter must be the same as in the likelihood c++ function

  output <- data.table(
    
    param_class = c(
      rep("intercept",times=2),
      rep("alpha",times=length(parameters$alpha)),
      rep("gamma",times=length(parameters$gamma)),
      rep("beta",times=length(parameters$beta)+1), # "+1" for the intercept
      "sigma",
      "delta"
    ),
    param = c(
      "A0",
      "C0",
      parameters$alpha,
      parameters$gamma,
      "intercept",
      parameters$beta,
      "sigma",
      "delta"
      ),
    start_par = c(
      A0[1],
      C0[1],
      rep(alpha[1],times=length(parameters$alpha)),
      rep(gamma[1],times=length(parameters$gamma)),
      rep(beta[1],times=length(parameters$beta)+1), # "+1" for the intercept
      sigma[1],
      delta[1]
      ),
    lower_par = c(
      A0[2],
      C0[2],
      rep(alpha[2],times=length(parameters$alpha)),
      rep(gamma[2],times=length(parameters$gamma)),
      rep(beta[2],times=length(parameters$beta)+1), # "+1" for the intercept
      sigma[2],
      delta[2]
      ),
    upper_par = c(
      A0[3],
      C0[3],
      rep(alpha[3],times=length(parameters$alpha)),
      rep(gamma[3],times=length(parameters$gamma)),
      rep(beta[3],times=length(parameters$beta)+1), # "+1" for the intercept
      sigma[3],
      delta[3]
      )
    
  )
  
  return(output)
  
}
