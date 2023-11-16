prepare_param_range <- function(range.limit, parameters){
  
  # attention: order of parameter must be the same as in the likelihood c++ function

  output <- data.table(
    
    param_class = c(
      "A0",
      "C0",
      "beta0",
      rep("alpha",times=length(parameters$alpha)),
      rep("gamma",times=length(parameters$gamma)),
      "sigma",
      "delta"
    ),
    param = c(
      "A0",
      "C0",
      "beta0",
      parameters$alpha,
      parameters$gamma,
      "sigma",
      "delta"
      ),
    start_par = c(
      range.limit[["A0"]][1],
      range.limit[["C0"]][1],
      range.limit[["beta0"]][1],
      rep(range.limit[["alpha"]][1],times=length(parameters$alpha)),
      rep(range.limit[["gamma"]][1],times=length(parameters$gamma)),
      range.limit[["sigma"]][1],
      range.limit[["delta"]][1]
      ),
    lower_par = c(
      range.limit[["A0"]][2],
      range.limit[["C0"]][2],
      range.limit[["beta0"]][2],
      rep(range.limit[["alpha"]][2],times=length(parameters$alpha)),
      rep(range.limit[["gamma"]][2],times=length(parameters$gamma)),
      range.limit[["sigma"]][2],
      range.limit[["delta"]][2]
      ),
    upper_par = c(
      range.limit[["A0"]][3],
      range.limit[["C0"]][3],
      range.limit[["beta0"]][3],
      rep(range.limit[["alpha"]][3],times=length(parameters$alpha)),
      rep(range.limit[["gamma"]][3],times=length(parameters$gamma)),
      range.limit[["sigma"]][3],
      range.limit[["delta"]][3]
      )
    
  )
  
  return(output)
  
}
