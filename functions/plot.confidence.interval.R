plot.confidence.interval <- function(
  db.confidence.interval,
  q.inf,
  q.sup,
  species.selected,
  nb.param.set
){
  
  # format quantile
  q.inf <- paste0("q", q.inf)
  q.sup <- paste0("q", q.sup)
  
  # ylim
  y.inf <- min(min(db.confidence.interval[, q.inf, with = FALSE]), unique(db.confidence.interval$Hp_obs))
  y.sup <- max(max(db.confidence.interval[, q.sup, with = FALSE]), unique(db.confidence.interval$Hp_obs))
  
  # stands
  c.stand <- unique(db.confidence.interval$stand)
  
  # list graph
  list_graph <- lapply(c.stand, function(stand.selected){
    
    db.confidence.interval.plot <- unique(db.confidence.interval[stand == stand.selected, c("year", q.inf, "q50", q.sup, "Hp_obs"), with = FALSE])
    
    graph <- ggplot() +
      geom_ribbon(
        data = db.confidence.interval.plot, 
        aes_string(x = "year", ymin = q.inf, ymax = q.sup), fill = "orange"
      ) +
      geom_line(
        data = db.confidence.interval.plot,
        aes(x = year, y = q50), col = "black"
      ) +
      geom_point(
        data = unique(db.confidence.interval.plot[, .(year_obs = max(year), Hp_obs)]), 
        aes(x = year_obs, y = Hp_obs)
      ) +
      ylim(c(y.inf, y.sup)) +
      labs(y  = "Dominant height (m)") + 
      ggtitle(paste0("Stand ", stand.selected))
    
    return(graph)
    
  })
  
  title <- ggpubr::text_grob(paste0("Species ", species.selected, ", nb.param.set = ", nb.param.set, " , int. ", q.inf, " - ", q.sup))
  
  graph.final <- plot_grid(plotlist = list_graph)
  graph.final <- plot_grid(plotlist = list(title, graph.final), nrow = 2, rel_heights = c(1, 10))
  
  return(graph.final)
  
}
