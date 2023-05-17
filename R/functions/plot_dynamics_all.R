plot_dynamics_all <- function(
  plotlist,
  species_list,
  y.max,
  db_species.name,
  graph.name,
  text.size.main,
  height,
  width,
  dir.saving
  ){
  
  # get legend
  
  graph.temp <- plotlist[[1]]$graph + theme(
    legend.title = element_text(size = text.size.main), #change legend title font size
    legend.text = element_text(size = text.size.main), #change legend text font size
    legend.direction = "vertical",
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.7, "cm")
  )  +
    theme(
      legend.direction = "horizontal",
      legend.spacing.y = unit(0.05, "cm")
    )
    
    
  legend <- get_legend(graph.temp)
  
  # remove title
  plotlist.prepared <- lapply(1 : length(species_list), function(index){
    
    graph.selected <- plotlist[[index]]$graph
    
    graph.selected <- graph.selected + theme(
      plot.title = element_blank(),
      legend.position="none",
      axis.title = element_blank(),
      axis.text = element_text(size = text.size.main)
    ) +
      ylim(c(0,y.max))
    
    return(graph.selected)
    
  })
  names(plotlist.prepared) <- species_list
  
  
  # order
  plotlist.prepared <- plotlist.prepared[db_species.name[order(name), code]]
  
  # plot grid
  
  margins <- c(0,0,0,0)
  scale <- 1
  
  graph.arranged <- plot_grid(
    plotlist = plotlist.prepared,
    labels = db_species.name[match(names(plotlist.prepared), code), name],
    ncol = 4,
    nrow = 5,
    scale = scale,
    label_size = text.size.main,
    label_x = 0.05,
    label_y = 1,
    hjust = 0,
    vjust = 1.5
  ) +
    draw_label("age (years)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = 1.2 * text.size.main) +
    draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = 1.2 * text.size.main) +
    theme(plot.margin = unit(margins, "cm"))
  
  graph.arranged <- plot_grid(
    plotlist = list(graph.arranged, legend),
    nrow = 2,
    rel_heights = c(1, 0.1)
  )
  
  
  # save

  ggsave(
    filename = paste0(graph.name, ".jpg"),
    plot = graph.arranged,
    path = dir.saving,
    height = 24,
    width = 17,
    units = "cm",
    limitsize = FALSE
  )
  
  # return
  return(NULL)
  
}
