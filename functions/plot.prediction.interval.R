plot.prediction.interval <- function(
  db.prediction.interval.err
){
  
  # plot graph - without error
  
  list.graph.confidence.interval.pur.no.err <- lapply(unique(db.prediction.interval.err$species), function(
    species.selected,
    nb.param.set = length(unique(db.prediction.interval.err$param_set)),
    variable = "Hp_pred"
  ){
    
    print(species.selected)
    
    db.selected <- db.prediction.interval.err[species == species.selected]
    
    setnames(db.selected, variable, "variable.selected")
    
    db.quantile <- db.selected[, .(
      q2.5 = quantile(variable.selected, probs = 0.025),
      q10 = quantile(variable.selected, probs = 0.1),
      q50 = quantile(variable.selected, probs = 0.5),
      q90 = quantile(variable.selected, probs = 0.9),
      q97.5 = quantile(variable.selected, probs = 0.975), 
      Hp_obs
    ), by = c("stand", "year")]
    
    graph.confidence.interval.selected <- plot.confidence.interval(
      db.confidence.interval = db.quantile,
      q.inf = "2.5",
      q.sup = "97.5",
      species.selected = species.selected, 
      nb.param.set = nb.param.set
    )
    
    return(graph.confidence.interval.selected)
    
  })
  names(list.graph.confidence.interval.pur.no.err) <- unique(db.prediction.interval.err$species)
  
  # plot graph - with residual error
  list.graph.confidence.interval.pur.err <- lapply(unique(db.prediction.interval.err$species), function(
    species.selected,
    nb.param.set = length(unique(db.prediction.interval.err$param_set)),
    variable = "Hp_pred.err"
  ){
    
    print(species.selected)
    
    db.selected <- db.prediction.interval.err[species == species.selected]
    
    setnames(db.selected, variable, "variable.selected")
    
    db.quantile <- db.selected[, .(
      q2.5 = quantile(variable.selected, probs = 0.025),
      q10 = quantile(variable.selected, probs = 0.1),
      q50 = quantile(variable.selected, probs = 0.5),
      q90 = quantile(variable.selected, probs = 0.9),
      q97.5 = quantile(variable.selected, probs = 0.975), 
      Hp_obs
    ), by = c("stand", "year")]
    
    graph.confidence.interval.selected <- plot.confidence.interval(
      db.confidence.interval = db.quantile,
      q.inf = "2.5",
      q.sup = "97.5",
      species.selected = species.selected, 
      nb.param.set = nb.param.set
    )
    
    return(graph.confidence.interval.selected)
    
  })
  names(list.graph.confidence.interval.pur.err) <- unique(db.prediction.interval.err$species)
  
  
  # combine graphs
  
  list.graph.combined <- lapply(names(list.graph.confidence.interval.pur.err), function(species.selected){
    
    graph.no.err <- list.graph.confidence.interval.pur.no.err[[species.selected]] +
      theme(plot.margin = unit(c(1,0,0,0), "cm"))
    graph.err <- list.graph.confidence.interval.pur.err[[species.selected]] +
      theme(plot.margin = unit(c(1,0,0,0), "cm"))
    
    graph.combined <- plot_grid(plotlist = list(graph.no.err, graph.err), labels = c("Without residual error", "With residual error"))
    
    return(graph.combined)
    
  })
  names(list.graph.combined) <- names(list.graph.confidence.interval.pur.err)
  
  return(list.graph.combined)
  
}
