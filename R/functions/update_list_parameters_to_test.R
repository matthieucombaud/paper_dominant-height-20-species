update_list_parameters_to_test<-function(list_parameters_to_test,list_parameters_selected){
  
  list_parameters_to_test$alpha <- setdiff(list_parameters_to_test$alpha,list_parameters_selected$alpha)
  list_parameters_to_test$gamma <- setdiff(list_parameters_to_test$gamma,list_parameters_selected$gamma)
  list_parameters_to_test$beta <- setdiff(list_parameters_to_test$beta,list_parameters_selected$beta)
  
  return(list_parameters_to_test)
  
}
