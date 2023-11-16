get_trait_carmona <- function(
  file,
  species.selected
){
  
  db_trait_carmona <- fread(file)
  
  setnames(db_trait_carmona, "V1", "initial_name")
  
  # add species name
  species_carmona <- str_split(db_trait_carmona$initial_name, "_", simplify = TRUE)
  db_trait_carmona[, species := paste0(species_carmona[,1], " ", species_carmona[,2])]
  
  # restrict to interesting species
  db_carmona_restricted <- db_trait_carmona[species %in% species.selected]
  # two different pinus sylvestris, not Pinus nigra
  
  # Deal with subspecies
  
  # Pinus nigra
  db_trait_carmona[str_detect(db_trait_carmona$species, "Pinus nigra")] # Pinus nigra not available at the subspecies level
  db.pinus.nigra.nigra <- db_trait_carmona[species == "Pinus nigra", .(species = "Pinus nigra subsp. nigra", la, ln, ph, sla, ssd, sm)]
  db.pinus.nigra.corsicana <- db_trait_carmona[species == "Pinus nigra", .(species = "Pinus nigra var. corsicana", la, ln, ph, sla, ssd, sm)]
  
  # Pinus sylvestris
  db_carmona_restricted[str_detect(db_carmona_restricted$species, "Pinus sylvestris")] # remove the row Pinus sylvestris var. mongolica
  db_carmona_restricted <- db_carmona_restricted[initial_name != "Pinus_sylvestris_var._mongolica"]
  
  # final db
  db_final <- rbind(
    db_carmona_restricted[, .(species, la, ln, ph, sla, ssd, sm)], 
    db.pinus.nigra.nigra, 
    db.pinus.nigra.corsicana
  )
  
  return(db_final)
  
}