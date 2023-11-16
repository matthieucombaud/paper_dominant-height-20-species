# The name of the function includes "new" to avoid confusion with the name of the function in the coda package

plot.mcmc.new <- function(
  mat.par,
  db.mcmc,
  title = NULL,
  max_treedepth.effective = "",
  Rhat.max = ""
){

  list.graph.div.par <- lapply(unique(mat.par$par), function(par.sel){

    db.mcmc.plot <- copy(db.mcmc[, c(par.sel, "divergent"), with = FALSE])
    setnames(db.mcmc.plot, old = par.sel, new = "par.sel") # to call aes more easoly in the plotting function
    
    graph.div.par.sel <- ggplot() +
      geom_histogram(
        data = db.mcmc.plot[divergent == TRUE],
        aes(x = par.sel, y = -after_stat(count)),
        fill = "red", 
        bins = 100
      ) +
      geom_histogram(
        data = db.mcmc.plot,
        aes(x = par.sel, y = after_stat(count)),
        fill = "blue",
        bins = 100
      ) +
      geom_vline(xintercept = 0, col = "black", lty = "dotted") +
      geom_vline(xintercept = mat.par[par == par.sel, estimate], col = "black") +
      xlab(par.sel) +
      theme_bw() +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 6),
        axis.text =  element_text(size = 4),
        axis.text.x =  element_text(size = 6)
      )
    
    if("lower" %in% names(mat.par) | "upper" %in% names(mat.par)){
      graph.div.par.sel <- graph.div.par.sel +
        geom_vline(xintercept = mat.par[par == par.sel, lower], col = "black", lty = "dashed") +
        geom_vline(xintercept = mat.par[par == par.sel, upper], col = "black", lty = "dashed")
    }
    
    return(graph.div.par.sel)
    
  })
  
  graph.div.par <- cowplot::plot_grid(plotlist = list.graph.div.par)
  
  if(!is.null(title)){
    graph.div.par <- cowplot::plot_grid(
      plotlist = list(
        ggpubr::text_grob(title, size = 10, hjust = 0),
        graph.div.par
      ),
      nrow = 2,
      rel_heights = c(0.1, 1)
    )
  }
  
  graph.div.par.leg <- cowplot::plot_grid(
    plotlist = list(
      graph.div.par,
      ggpubr::text_grob("blue : MCMC distribution (all chains, all transitions after warm up). Divergent transitions in red", size = 8, just = "left"),
      ggpubr::text_grob("solid black line: parameter value infered by nlminb", size = 8, just = "left"),
      ggpubr::text_grob("parameter range authorized (dashed line, none if absent), x=0 (dotted line)", size = 8, just = "left"),
      ggpubr::text_grob(paste0(nrow(db.mcmc[divergent == TRUE]), " divergent transitions over ", nrow(db.mcmc), " transitions"), size = 8, just = "left"),
      ggpubr::text_grob(paste0("max_treedepth.effective = ", max_treedepth.effective, " ; Rhat max = ", Rhat.max), size = 8, just = "left")
    ),
    nrow = 6,
    rel_heights = c(1, 0.04, 0.04, 0.04, 0.04, 0.04),
    hjust = 0
  )
  
  return(graph.div.par.leg)
  
}
