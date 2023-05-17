plot_CCimpactVShabitat <- function(
  list_db_impact, 
  list_climate_ref,
  graph.name,
  db_species.name,
  variable.focus,
  label.x.axis,
  species_list,
  dir.saving
){
  

  # get climate
  
  db_climate.ref <- rbindlist(lapply(species_list, function(species.selected){
    
    print(species.selected)
    
    # initial climate
    db_climate.ref_species.selected <- list_climate_ref[[species.selected]][[1]]$year[, lapply(.SD, function(x){mean(x)}), by = "stand", .SDcols = variable.focus]
    db_climate.ref_species.selected <- melt(db_climate.ref_species.selected, id.vars = "stand", variable.name = "variable", value.name = "value")
    
    # add species
    db_climate.ref_species.selected[, species_code := species.selected]
    
    return(db_climate.ref_species.selected)
    
  }))
  db_climate.ref_median <- db_climate.ref[, .(value.median = median(value)), by = c("species_code", "variable")]
  
  
  # get impact
  db_impact <- rbindlist(list_db_impact)
  db_impact_median <- db_impact[, .(height.gap.rel.median = median(height.gap.rel)), by = "species_code"]
  
  # merge
  db_climate_impact <- merge(db_impact_median, db_climate.ref_median, by = "species_code")
  db_climate_impact <- merge(db_climate_impact, db_species.name, by.x = "species_code", by.y = "code")

  # for all variable of interest
  
  list_graph <- lapply(variable.focus, function(variable.selected){

    y.min <- 100 * min(db_climate_impact[variable == variable.selected, height.gap.rel.median]) - 1
    y.max <- 100 * max(db_climate_impact[variable == variable.selected, height.gap.rel.median]) + 1
    
    graph <- ggplot(
      data = db_climate_impact[variable == variable.selected], 
      aes(
        x = value.median,
        y = 100 * height.gap.rel.median,
        label = name.short
      )
    ) +
      geom_point(
        size = 0.2
      ) +
      geom_text_repel(
        color = "black",
        alpha = 0.7,
        size = 2,
        box.padding = unit(0.3, "lines")
      ) +
      labs(
        x = label.x.axis[variable.selected], 
        y = paste0("Median of climate change impact on SDH (%)")
      )+   
      theme(
        axis.title = element_text(size = 5), 
        axis.text = element_text(size = 5)
      ) +
      coord_cartesian(
        ylim = c(y.min, y.max)
        ) +
      scale_y_continuous(
        breaks = round(seq(y.min, y.max, 5)),
        minor_breaks = NULL,
        labels = paste0(round(seq(y.min, y.max, 5)), "%")
      )
    
    ggsave(
      filename = paste0(graph.name,"_",variable.selected, ".jpg"),
      plot = graph,
      path = dir.saving,
      height = 10, width = 10, units = "cm"
    )
    
    return(NULL)
    
  })

  
  return(NULL)
  
}
