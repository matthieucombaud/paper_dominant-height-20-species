plot_dynamics_group_new <- function(
  species_code,
  list_db_dynamics.ref,
  list_db_dynamics.interest,
  age.productivity,
  label.ref,
  label.interest,
  db_species.name
  ){
  
  print(species_code)
  
  # get species names
  species_name <- db_species.name[code == species_code, name]
  
  # get climate
  db_dynamics.ref <- list_db_dynamics.ref[[species_code]]
  db_dynamics.ref[, climate.scenario.renamed := label.ref]
  
  db_dynamics.interest <- list_db_dynamics.interest[[species_code]]
  db_dynamics.interest[, climate.scenario.renamed := label.interest]
  
  # identify stand in the Q5-Q15, Q45-Q55, Q85-Q95 of the SDH at 70 yearsunder reference climate
  db_dynamics.ref_temp <- db_dynamics.ref[age == age.productivity]

  quantiles <- db_dynamics.ref_temp[, quantile(height, probs = c(0, 0.05, 0.15, 0.45, 0.55, 0.85, 0.95, 1))]
  
  if(length(unique(quantiles)) == length(quantiles)){
    
    db_dynamics.ref_temp[, group := cut(
      x = height,
      breaks = quantiles,
      labels = c("0-5","5-15","15-45","45-55","55-85","85-95","95-100"),
      include.lowest = TRUE
    )
    ]
    
  }else{
    
    thresh_5 <- quantiles["5%"]
    thresh_15 <- quantiles["15%"]
    thresh_45 <- quantiles["45%"]
    thresh_55 <- quantiles["55%"]
    thresh_85 <- quantiles["85%"]
    thresh_95 <- quantiles["95%"]
    
    
    db_dynamics.ref_temp[
      height > thresh_5 & height <= thresh_15, group := "5-15"
    ]
    db_dynamics.ref_temp[
      height > thresh_45 & height <= thresh_55, group := "45-55"
    ]
    db_dynamics.ref_temp[
      height > thresh_85 & height <= thresh_95, group := "85-95"
    ]
    
  }
  
  # prepare db to plot
  
  db_plot <- rbind(db_dynamics.ref, db_dynamics.interest)
  db_plot <- merge(db_plot, db_dynamics.ref_temp[, .(stand, group)], by = "stand")
  db_plot <- db_plot[group %in% c("5-15", "45-55", "85-95")]
    
  # add site index category
  db_plot[group == "5-15" , category := "low"]
  db_plot[group == "45-55" , category := "medium"]
  db_plot[group == "85-95" , category := "high"]
  db_plot$category <- factor(db_plot$category, levels = c("low", "medium", "high"))

  # order climate
  db_plot$climate.scenario.renamed <- factor(
    db_plot$climate.scenario.renamed,
    levels = c(
      label.interest,
      label.ref
    ))
    
  # create a variable representing both climate scenario and productivity category
  db_plot[, clim.prod := paste0(climate.scenario, " - ", category, " productivity")]
  db_plot[, clim.stand := paste0(climate.scenario, " - ", stand)]
    
  # reference climate db
  
  db_plot_reference_min_max <- db_plot[
    climate.scenario.renamed == label.ref
    ][,
      .(
        height.min = min(height),
        height.max = max(height)
        ),
      by = c("age", "category")]
  
  db_plot_reference_min_max <- 
    merge(
      db_plot_reference_min_max,
      unique(db_plot[, .(age,year)]), 
      by = "age"
    )
  
  # plot
  graph_dynamics <- ggplot(
  ) +
    geom_line(
      data = db_plot[climate.scenario.renamed == label.interest],
      aes(
        x = year,
        y = height,
        color = category,
        group = stand
      ),
      linetype = "solid",
      linewidth = 0.1,
      alpha = 1
    ) +
    geom_ribbon(
      data = db_plot_reference_min_max,
      aes(
        x = year,
        ymin = height.min,
        ymax = height.max,
        fill = category
      ),
      alpha = 0.8
    ) +
    scale_color_manual(
      name = paste0(label.interest, " climate"),
      values = c("#FF0000","#001AFF", "chartreuse4"),
      breaks = c("high","medium","low"),
      labels = c("high productivity","medium productivity","low productivity")
    )   +
    scale_fill_manual(
      name = paste0(label.ref, " climate"),
      values = c("#FF9900", "#00B2EE", "chartreuse3"),
      breaks = c("high","medium","low"),
      labels = c("high productivity","medium productivity","low productivity")
    )

  return(
    list(
      species = species_code,
      graph = graph_dynamics
    )
  )
  
}
