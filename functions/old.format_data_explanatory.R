old.format_data_explanatory <- function(data,param_alpha,param_gamma,param_beta){

  data.matrix <- list()
  
  # data.matrix$nb.stand <- as.matrix(data)[, "stand", drop = FALSE]
  data.matrix$alpha <- as.matrix(data)[, param_alpha, drop = FALSE] # if param_alpha==NULL gives an empty matrix, but not an issue
  data.matrix$gamma <- as.matrix(data)[, param_gamma, drop = FALSE] # if param_alpha==NULL gives an empty matrix, but not an issue
  data.matrix$beta <- as.matrix(data)[, param_beta, drop = FALSE] # if param_alpha==NULL gives an empty matrix, but not an issue
  
  # add intercept in beta
  data.matrix$beta <- cbind(intercept = 1,data.matrix$beta)
  
  return(data.matrix)

}
