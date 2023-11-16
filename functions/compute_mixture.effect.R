# Description
# For each stand and each species dominating the stand,
# this function computes the mixture effect as the relative difference between the observed dominant height (DH)
# and the (simulated) dominant height that would have been observed in pure stands

# Argument
# db.stand: dt with columns for stand, species 1, species 2, and observed DH for each species
# db.envi: dt with environmental conditions for each stand
# list_calibration.SDH.model: list with calibration input of the SDH model for each species
# list_parameter.SDH.model: list with parameter values of the SDH model for each species
# list_species: list of species considered


# Value
# a dt giving for each stand and each species the mixture effect

compute_mixture.effect <- function(
  db_DH.mix, 
  db_DH.pur
){
  
  
  # merge 
  
  db_DH_merged <- merge(db_DH.mix, db_DH.pur[, .(stand, age, species, DH_p_sp1 = DH_p)], # merge for sp1
        by.x = c("stand", "sp1", "age_sp1"), by.y = c("stand", "species", "age"))
  db_DH_merged <- merge(db_DH_merged, db_DH.pur[, .(stand, age, species, DH_p_sp2 = DH_p)], # merge for sp2
                        by.x = c("stand", "sp2", "age_sp2"), by.y = c("stand", "species", "age"))
  
  # add number of stand of the corresponding mixture
  db_DH_merged[, N := .N, by = c("sp1", "sp2")] # ok to compute it like that because for a given pair, sp1 corresponds always to the same species (the first one in alphabetical order)
  
  
  # create two lines per stand, depending on the focal and the companion species
  
  db_DH_focal_sp1 <- db_DH_merged[, .(
      stand,
      year_observation,
      N,
      sp_focal = sp1,
      name_focal = name_sp1,
      name.short_focal = name.short_sp1,
      Gupper_focal = Gupperr_sp1,
      age_focal = age_sp1,
      DH_focal_mix = h_sp1,
      DH_focal_pur = DH_p_sp1,
      sp_companion = sp2,
      name_companion = name_sp2,
      name.short_companion = name.short_sp2,
      Gupper_companion = Gupperr_sp2,
      age_companion = age_sp2,
      DH_companion_mix = h_sp2,
      DH_companion_pur = DH_p_sp2     
    )
  ]
  
  db_DH_focal_sp2 <- db_DH_merged[, .(
    stand,
    year_observation,
    N,
    sp_focal = sp2,
    name_focal = name_sp2,
    name.short_focal = name.short_sp2,
    Gupper_focal = Gupperr_sp2,
    age_focal = age_sp2,
    DH_focal_mix = h_sp2,
    DH_focal_pur = DH_p_sp2,
    sp_companion = sp1,
    name_companion = name_sp1,
    name.short_companion = name.short_sp1,
    Gupper_companion = Gupperr_sp1,
    age_companion = age_sp1,
    DH_companion_mix = h_sp1,
    DH_companion_pur = DH_p_sp1     
  )
  ]
  
  db_ME <- rbind(db_DH_focal_sp1, db_DH_focal_sp2)
  
  # compute ME and relative height gap in pur stand
  db_ME[, ":="(
    ME = (DH_focal_mix - DH_focal_pur) / DH_focal_pur,
    delta_DH_pur = (DH_companion_pur - DH_focal_pur) /DH_focal_pur
  )
  ]
  
  return(db_ME)
}
