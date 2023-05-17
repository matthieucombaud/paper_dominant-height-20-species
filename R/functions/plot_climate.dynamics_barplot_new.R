plot_climate.dynamics_barplot_new <- function(
  list_climate.ref,
  list_climate.interest,
  list_species,
  plotting.period,
  averaging.period,
  variable.focus,
  title,
  graph.name, 
  dir.saving, 
  height,
  width, 
  color.positive, 
  color.negative
){
  
  # param plot
  height <- 10
  width <- 15
  
  # ref climate
  
  db_climate.ref <- rbindlist(lapply(list_species, function(species.selected){
    
    db_climate.ref_species.selected <- list_climate.ref[[species.selected]][[1]]$year
    db_climate.ref_species.selected[,species.code := species.selected]
    
    return(db_climate.ref_species.selected)
    
  }))
  
  climate.value.ref <- as.numeric(db_climate.ref[
    climatic_year %in% averaging.period, lapply(.SD, function(x){mean(x)}), .SDcols = variable.focus
  ])
  
  # interest climate
  db_climate.interest <- rbindlist(lapply(list_species, function(species.selected){
    
    db_climate.interest_species.selected <- list_climate.interest[[species.selected]][[1]]$year
    db_climate.interest_species.selected[,species.code := species.selected]
    
    return(db_climate.interest_species.selected)
    
  }))
  
  db_climate.anomaly <- db_climate.interest[
    climatic_year %in% plotting.period, lapply(.SD, function(x){mean(x)}), by = c("climatic_year"), .SDcols = variable.focus
  ]
  setnames(db_climate.anomaly, variable.focus, "value")

  db_climate.anomaly[, anomaly := value - climate.value.ref]
  db_climate.anomaly[, sign := ifelse(anomaly >= 0, "positive", "negative")]
  
  
  # plot ----

  graph_climate <- ggplot(
      data = db_climate.anomaly, 
      aes(
        x = climatic_year, 
        y = anomaly, 
        fill = sign
      )
    ) +
    geom_col() +
    labs(
      x = "Year",
      title = title
    ) +
    theme(
      axis.text.x = element_text(angle = 0),
      axis.title.y = element_blank()
    ) +
    scale_fill_manual(
      name = "Climate anomaly",
      values = c(color.positive, color.negative),
      breaks = c("positive", "negative"), 
      labels = c("positive", "negative")
    )
  
  ggsave(
    filename = paste0(graph.name, ".jpg"),
    plot = graph_climate,
    path = dir.saving,
    height = height,
    width = width,
    units = "cm",
    limitsize = FALSE
  )

  return(graph_climate)
  
}
