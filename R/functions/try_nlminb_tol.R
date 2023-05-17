try_nlminb_tol <- function(param_range,f,relative_tolerance){
  
  return(try(nlminb(
    start = param_range$start_par,
    objective=f$fn,
    f$gr,
    f$he,
    lower=param_range$lower_par,
    upper=param_range$upper_par,
    control=list(trace=F,rel.tol=relative_tolerance,iter.max=150,eval.max=200)
  )))
  
}
