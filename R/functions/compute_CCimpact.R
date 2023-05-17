compute_CCimpact <- function(
  species_code,
  list_db_dynamics.ref,
  list_db_dynamics.interest,
  db_species.name,
  comparison.age
  ){

  db_dynamics.ref <- list_db_dynamics.ref[[species_code]][age == comparison.age, .(stand, height)]
  db_dynamics.interest <- list_db_dynamics.interest[[species_code]][age == comparison.age, .(stand, height)]
  db_dynamics.comparison <- merge(db_dynamics.ref, db_dynamics.interest, by = "stand", suffixes = c("_ref", "_interest"))


  db_dynamics.comparison[, height.gap.rel := (height_interest - height_ref) / height_ref]
  
  db_dynamics.comparison[, species_code := species_code]
    
  return(db_dynamics.comparison)

}
