add_quantile <- function(vector, quantiles.selected){
  
  
  quantile.value <- quantile(vector,probs = quantiles.selected)
  
  db <- data.table(variable = vector)
  
  if(length(unique(quantile.value)) == 1){ # "if" disjunction to take into account the case where there is no climate impact (because of no climate variable in the growht equation)
    
    db[,group := factor("intermediary", levels = c("low","intermediary","high"))]
    
  }else{
    
    db[,group := cut(variable, breaks = quantile.value, include.lowest=TRUE, labels = c("low","intermediary","high"))]
    db[,group := as.factor(group)]
    
  }
  
  return(db[,group])
  
  
}
