get_stand.complete.data <- function(
  db_trait_selected, 
  db_stand
){
  
  sp.uncomplete <- db_trait_selected[!complete.cases(db_trait_selected), code]
  
  stand.selected <- db_stand[!(sp1 %in% sp.uncomplete) & !(sp2 %in% sp.uncomplete), stand]
  
  return(stand.selected)
  
}