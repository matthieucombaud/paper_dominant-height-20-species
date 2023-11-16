compute_height_base_2 <- function(
  list.species,
  db.stand, # db with columns stand, sp, year_observation, age
  db.envi, # unnormalized data
  cat.var_names.gen,
  list.db.param,
  list.db.normalization,
  initial.height
){
  
  db.Hp <- rbindlist(lapply(list.species, function(sp.sel){ # we do the computation species per species, to prepare to data only once for each species
    
    print(sp.sel)
    
    # db stand and envi
    db.stand.sel <- db.stand[sp == sp.sel & year_first >= min(db.envi$climatic_year), setdiff(names(db.stand), "sp"), with = FALSE]
    db.envi.sel <- db.envi[stand %in% unique(db.stand.sel$stand)]
    
    # db.par (nlminb model)
    db.parameter.sel <- list.db.param[[sp.sel]]
    
    # db.normalization
    db.normalization.sel <- list.db.normalization[[sp.sel]]
    
    # normalize environmental data
    db.envi.norm.sel <- prepare_data_height_computation(
      db.parameter = db.parameter.sel,
      db.normalization = db.normalization.sel,
      cat.var_names.gen = cat.var_names.gen,
      db.envi = db.envi.sel,
      c.stand = unique(db.stand.sel$stand)
    )
    
    # get db of initial year and age
      db.Hp.sel <- compute_height_base(
        param_A0 = db.parameter.sel[param_class == "A0", estimate],
        param_C0 = db.parameter.sel[param_class == "C0", estimate],
        param_beta0 = db.parameter.sel[param_class == "beta0", estimate],
        param_alpha = db.parameter.sel[param_class == "alpha", estimate],
        param_gamma = db.parameter.sel[param_class == "gamma", estimate],
        names.param_alpha = db.parameter.sel[param_class == "alpha", param],
        names.param_gamma = db.parameter.sel[param_class == "gamma", param],
        data_explanatory = db.envi.norm.sel,
        initial.height = initial.height,
        data_age_height = db.stand.sel
      )
      
      db.Hp.sel[, sp := sp.sel]
      
    # return
      return(db.Hp.sel)    
    
  }))
  
  return(db.Hp)
  
}
