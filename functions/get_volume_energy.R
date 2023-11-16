get_volume_energy <- function(
  file_diam,
  file_ratio,
  file_height,
  species.selected,
  db.species.name
){
  
  # get allometry coefficients
  
    # allometries
    db.allo_diam <- fread(file_diam)
    db.allo_ratio <- fread(file_ratio)
    db.allo_height <- fread(file_height)
    
    db.allo_diam.restricted <- db.allo_diam[species %in% species.selectede, .(species, mean_a1, sd_a1, mean_a2, sd_a2)]
    db.allo_ratio.restricted <- db.allo_ratio[species %in% species.selected, .(species, mean_a1, sd_a1)]
    db.allo_height.restricted <- db.allo_height[species %in% species.selected, .(species, mean_a1, sd_a1, mean_a2, sd_a2, mean_a3, sd_a3)]
  
    # add Pïnus nigra
    db.allo_diam.pinus.nigra.nigra <- db.allo_diam[species == "Pinus nigra", .(species = "Pinus nigra subsp. nigra", mean_a1, sd_a1, mean_a2, sd_a2)]
    db.allo_diam.pinus.nigra.corsicana <- db.allo_diam[species == "Pinus nigra", .(species = "Pinus nigra var. corsicana", mean_a1, sd_a1, mean_a2, sd_a2)]
    
    db.allo_ratio.pinus.nigra.nigra <- db.allo_ratio[species == "Pinus nigra", .(species = "Pinus nigra subsp. nigra", mean_a1, sd_a1)]
    db.allo_ratio.pinus.nigra.corsicana <- db.allo_ratio[species == "Pinus nigra", .(species = "Pinus nigra var. corsicana", mean_a1, sd_a1)]
    
    db.allo_height.pinus.nigra.nigra <- db.allo_height[species == "Pinus nigra", .(species = "Pinus nigra subsp. nigra", mean_a1, sd_a1, mean_a2, sd_a2, mean_a3, sd_a3)]
    db.allo_height.pinus.nigra.corsicana <- db.allo_height[species == "Pinus nigra", .(species = "Pinus nigra var. corsicana", mean_a1, sd_a1, mean_a2, sd_a2, mean_a3, sd_a3)]
    
    # final db
    db.allo_diam.final <- rbind(db.allo_diam.restricted, db.allo_diam.pinus.nigra.nigra, db.allo_diam.pinus.nigra.corsicana)
    db.allo_ratio.final <- rbind(db.allo_ratio.restricted, db.allo_ratio.pinus.nigra.nigra, db.allo_ratio.pinus.nigra.corsicana)
    db.allo_height.final <- rbind(db.allo_height.restricted, db.allo_height.pinus.nigra.nigra, db.allo_height.pinus.nigra.corsicana)
    
  # computation of metrics at DBH = 15 cm
    
    # computation of diameter, crown ratio and height
    db.allo_diam.final[, CD15 := mean_a1 * 15 ^ mean_a2]
    db.allo_ratio.final[, ratio := exp(mean_a1) / (1 + exp(mean_a1))]
    db.allo_height.final[, H15 := 1.3 + mean_a1 * (1 - exp(- mean_a2 * 15)) ^ mean_a3]
    
    db.metrics <- Reduce(function(x, y) merge(x = x,  y = y, by = "species", all = TRUE), list(
      db.allo_diam.final[, .(species, CD15)],
      db.allo_ratio.final[, .(species, ratio)],
      db.allo_height.final[, .(species, H15)]
    ))
    
    # add if angiosperm or gymnosperm
    db.metrics <- merge(db.metrics, db_species.name[, .(species = name, code)], by = "species")
    db.metrics[, type := ifelse(as.numeric(str_sub(code, 1,2)) > 50, "gymno", "angio")]
    
    # volume computation
    db.metrics[type == "angio", V15 := 4/3 * pi * CD15/2 * CD15/2 * ratio * H15 / 2]
    db.metrics[type == "gymno", V15 := 1/2 * pi * CD15/2 * CD15/2 * ratio * H15]
    
    
    return(db.metrics)
  
  
}