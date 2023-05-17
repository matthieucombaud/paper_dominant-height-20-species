plot_dynamics_single <- function(
  plotlist,
  species_list,
  species_code,
  db_species.name,
  graph.name,
  text.size.main,
  dir.saving, 
  height,
  width
  ){

  # get legend
  
  graph.temp <- plotlist[[1]]$graph + theme(
    legend.title = element_text(size = text.size.main), #change legend title font size
    legend.text = element_text(size = text.size.main) #change legend text font size
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
      ylim(c(0,35)) +
      theme(plot.margin = unit(c(0.1,0,0,0), "cm"))
    
    return(graph.selected)
    
  })
  names(plotlist.prepared) <- species_list
  
  # plot grid
  
  margins <- c(0.1,0.1,0,0)

  graph.arranged_1 <- plot_grid(
    plotlist = list(plotlist.prepared[[species_code]]),
    labels = db_species.name[match(species_code, code), name],
    ncol = 1,
    nrow = 1,
    scale = 0.9,
    label_size = text.size.main,
    label_x = 0,
    label_y = 1,
    hjust = - 0.5,
    vjust = 4
  ) 
    # draw_label("Date", x = 0.5, y = 0, hjust = 0.5, vjust = -5, angle = 0, size = text.size.main)
  
  graph.arranged <- plot_grid(
    graph.arranged_1,
    legend,
    ncol = 2,
    rel_widths = c(8,3)
  ) +
    draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0.5, vjust = 2, angle = 90, size = text.size.main) +
    theme(plot.margin = unit(margins, "cm"))
  
  # save

  ggsave(
    filename = paste0(graph.name, ".jpg"),
    plot = graph.arranged,
    path = dir.saving,
    height = height,
    width = width,
    units = "cm",
    limitsize = FALSE
  )
  
  # return
  return(graph.arranged)
  
}
