plot.optimism <- function(
  list_metrics,
  db_number.stand,
  db_species.name,
  db_stand.number, 
  dir.saving
){
  
  list_species <- sapply(list_metrics, function(analysis.rmse.selected){
    return(analysis.rmse.selected$species_code)
  })
  names(list_metrics) <- list_species
  
  
  db_optimism <- rbindlist(lapply(list_species, function(species.selected){
    
    analysis.rmse.selected <- list_metrics[[species.selected]]
    
    optimism <- analysis.rmse.selected$list_metrics.submodel[, mean(pmax(rmse.validation - rmse.calibration, 0)/rmse.calibration)]
    
    return(
      data.table(
        species.code = species.selected,
        optimism = optimism
      )
    )
    
  }))
  
  db_optimism <- db_optimism %>%
    merge(db_stand.number, by = "species.code") %>%
    merge(db_species.name, by.x = "species.code", by.y = "code")
  
  # plot
  graph_optimism_stand.nb <- ggplot(
    data = db_optimism, 
    aes(
      x = N, 
      y = optimism * 100,
      label = name
    )
  ) +
    geom_point(
      size = 0.5
    ) +
    geom_text_repel(
      color = "black",
      alpha = 0.7,
      size=2,
      box.padding = unit(0.5, "lines")
    ) +
    labs(
      x = "Number of stands",
      y = "Optimism"
    )+
    theme(
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(
      breaks = round(seq(0, 100 * db_optimism[, max(optimism)] + 10, 10)),
      minor_breaks = NULL,
      labels = paste0(round(seq(0, 100 * db_optimism[, max(optimism)] + 10, 10)), "%")
    )
  
  ggsave(
    filename = "graph_optimism_stand.nb.jpg",
    plot = graph_optimism_stand.nb,
    path = dir.saving,
    height = 15, 
    width = 17, 
    units = "cm"
  )
  
  return(db_optimism)
  
}