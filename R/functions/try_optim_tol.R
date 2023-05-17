try_optim_tol <- function(param_range,f,relative_tolerance){
  
  return(try(optim(
    par = param_range$start_par,
    fn = f$fn,
    method = "BFGS",
    control = list(trace=0,maxit=500),
    hessian=F
  )))
  
}
