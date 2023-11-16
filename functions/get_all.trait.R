get_trait <- function(
  list_db, 
  db_species.name
){
  
  db_trait <- Reduce(function(x, y) merge(x = x, y = y, by = "species", all = TRUE), list_db)
  
  db_trait <- merge(db_trait, db_species.name[, .(code, species = name)], by = "species")
  
  return(db_trait)
}