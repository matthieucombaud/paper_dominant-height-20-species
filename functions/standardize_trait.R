standardize_trait <- function(
  db_trait
){
  
  db_trait[, WD_st := (WD - mean(WD))/sd(WD)]  
  db_trait[, sla_st := (sla - mean(sla))/sd(sla)] 
  db_trait[, ST_st := (ST - mean(ST))/sd(ST)] 
  
  return(db_trait)
  
}