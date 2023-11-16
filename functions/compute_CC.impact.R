compute_CC.impact <- function(
  db.dynamics_climate.ref,
  db.dynamics_climate.actual,
  comparison.age
  ){
  
  db.dynamics_climate.ref[, age := year - min(year), by = c("stand", "sp")]
  db.dynamics_climate.actual[, age := year - min(year), by = c("stand", "sp")]
  
  db.dynamics_climate.ref <- db.dynamics_climate.ref[age == comparison.age, .(stand, sp, Hp)]
  db.dynamics_climate.actual <- db.dynamics_climate.actual[age == comparison.age, .(stand, sp, Hp)]
  db_dynamics.comparison <- merge(db.dynamics_climate.ref, db.dynamics_climate.actual, by = c("stand", "sp"), suffixes = c("_ref", "_actual"))

  db_dynamics.comparison[, impact := Hp_actual / Hp_ref - 1]
    
  return(db_dynamics.comparison)

}
