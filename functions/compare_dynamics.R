compare_dynamics <- function(
  list_model,
  list_parameter.SDH.model,
  list_data_explanatory_formated,
  db_stand,
  db.trait,
  height.initial, 
  t_ori.force, 
  t_obs.force, 
  db_species.name,
  no_cores
){
  
  # dynamics in pure stand
  db_dyna.pur <- simulate_DH_generic(
    par = NULL, # param mixture effect
    parameter.pur = list_parameter.SDH.model, # param Dh in pure stand
    list_data_explanatory_formated = list_data_explanatory_formated,
    db.age.height = db_stand,
    db.trait = db.trait,
    height.initial = height.initial,
    t_ori.force = t_ori.force, # to force origin year (must be activated at the same time as t_obs_force)
    t_obs.force = t_obs.force, # to force observation year (must be activated at the same time as t_ori_force)
    no_cores = no_cores
  )
  
  # format database
  db_dyna.pur <- melt(db_dyna.pur, id.vars = c("stand", "year"))
  db_dyna.pur[, species := str_split(variable, "_", simplify = TRUE)[,3]]
  db_dyna.pur[, type := str_split(variable, "_", simplify = TRUE)[,2]]
  db_dyna.pur[, model := "no mixture effect"]
  
  
  # dynamics in mixtures
  db_dyna.mix <- rbindlist(lapply(1:length(list_model), function(index){
    
    model.selected <- list_model[[index]]  
    name.model.selected <- names(list_model)[index]
    
    # compute dynamics in mixture
    db_dyna.mix.selected <- simulate_DH_generic(
      par = model.selected$par, # param mixture effect
      parameter.pur = list_parameter.SDH.model, # param Dh in pure stand
      list_data_explanatory_formated = list_data_explanatory_formated,
      db.age.height = db_stand,
      db.trait = db.trait,
      height.initial = height.initial,
      t_ori.force = t_ori.force, # to force origin year (must be activated at the same time as t_obs_force)
      t_obs.force = t_obs.force, # to force observation year (must be activated at the same time as t_ori_force)
      no_cores = no_cores
    )
    
    # format database
    db_dyna.mix.selected <- melt(db_dyna.mix.selected, id.vars = c("stand", "year"))
    db_dyna.mix.selected[, species := str_split(variable, "_", simplify = TRUE)[,3]]
    db_dyna.mix.selected[, type := str_split(variable, "_", simplify = TRUE)[,2]]
    
    # add model name
    db_dyna.mix.selected[, model := name.model.selected]
    
    # return
    return(db_dyna.mix.selected)
    
  }))  
  
  # bind mixture and pure dynamics
  
  db_dynamics <- rbind(db_dyna.pur, db_dyna.mix)
  
  # add species names
  db_dynamics <- merge(db_dynamics, db_stand[, .(stand, sp1, sp2)], by = "stand")
  db_dynamics[, species.code := ifelse(species == 1, sp1, sp2)]
  db_dynamics <- merge(db_dynamics, db_species.name[, .(species.code = code, species.name = name)], by = "species.code")

  # return
  return( db_dynamics)
  
}
