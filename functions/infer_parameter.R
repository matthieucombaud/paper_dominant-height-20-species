infer_parameter <- function(
  
){
  
  
  
  
  


  
  model.fit <- optim(
    par = c(alpha_1 = 0.1, alpha_2 = 0.1),
    fn = get_likelihood,
    db.explanatory = ,
    db.age.height,
    model.pur,
    parameter.pur,
    height.initial = 1.3
  )
  

  
  
  
  
}