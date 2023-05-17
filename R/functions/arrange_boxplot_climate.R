arrange_boxplot_climate <- function(
  plotlist,
  variable.to.plot,
  graph.name,
  text.size.main,
  height, 
  width,
  dir.saving
){

  plotlist <- plotlist[names(plotlist) %in% variable.to.plot]
  
  # remove title
  plotlist.prepared <- lapply(plotlist, function(graph.selected){
    
    graph.selected <- graph.selected +
      geom_boxplot(
        lwd = 0.05,
        outlier.shape = NA
      ) +
      theme(
        plot.title = element_blank(),
        legend.position="none",
        axis.title = element_blank(),
        axis.text = element_text(size = text.size.main)
      )
    return(graph.selected)
    
  })
  
  # plot grid
  
  margins <- c(0.3,0,0,0)
  scale <- 1
  ratio.colnames <- 1/30
  
  
  ## column and row names
  
  colnames <- plot_grid(
    plotlist = lapply(c("Temperature (°C)", "Precipitations (mm)", "Climatic water balance (mm)"), function(colnames.selected){
      text_grob(colnames.selected, size = 1.2 * text.size.main)
    }),
    nrow = 1,
    ncol = 3
  )
  
  rownames <- plot_grid(
    plotlist = lapply(c("Year", "Autumn", "Winter", "Spring", "Summer"), function(season.selected){
      text_grob(season.selected, size = 1.2 * text.size.main)
    }),
    nrow = 5,
    ncol = 1
  )
  
  rownames <- plot_grid(
    plotlist = list(
      NULL,
      rownames
    ),
    nrow = 2,
    ncol = 1,
    rel_heights = c(ratio.colnames, 1)
  )
  
  ## plots
  
  graph.arranged_Tmean <- plot_grid(
    plotlist = plotlist.prepared[c("Tmean_9_8", "Tmean_9_11", "Tmean_12_2", "Tmean_3_5", "Tmean_6_8")],
    labels = NULL,
    ncol = 1,
    nrow = 5,
    scale = scale,
    label_size = text.size.main,
    label_x = 0.05,
    label_y = 1,
    hjust = 0,
    vjust = 1.5
  ) +
    # draw_label("Mean temperature (°C)", x = 0.5, y = 1, hjust = 0.5, vjust = 0, angle = 0, size = 1.2 * text.size.main)+
    theme(plot.margin = unit(margins, "cm"))
  
  graph.arranged_precipitation <- plot_grid(
    plotlist = plotlist.prepared[c("precipitation_9_8", "precipitation_9_11", "precipitation_12_2", "precipitation_3_5", "precipitation_6_8")],
    labels = NULL,
    ncol = 1,
    nrow = 5,
    scale = scale,
    label_size = text.size.main,
    label_x = 0.05,
    label_y = 1,
    hjust = 0,
    vjust = 1.5
  ) +
    # draw_label("Precipitations (mm)", x = 0.5, y = 1, hjust = 0.5, vjust = 0, angle = 0, size = 1.2 * text.size.main)+
    theme(plot.margin = unit(margins, "cm"))
  
  graph.arranged_cwb <- plot_grid(
    plotlist = plotlist.prepared[c("cwb_9_8", "cwb_9_11", "cwb_12_2", "cwb_3_5", "cwb_6_8")],
    labels = NULL,
    ncol = 1,
    nrow = 5,
    scale = scale,
    label_size = text.size.main,
    label_x = 0.05,
    label_y = 1,
    hjust = 0,
    vjust = 1.5
  ) +
    # draw_label("Climatic water balance (mm)", x = 0.5, y = 1, hjust = 0.5, vjust = 0, angle = 0, size = 1.2 * text.size.main)+
    theme(plot.margin = unit(margins, "cm"))
  
  # arrange
  
  graph.arranged <- plot_grid(
    plotlist = list(
      graph.arranged_Tmean,
      graph.arranged_precipitation,
      graph.arranged_cwb
    ),
    ncol = 3, 
    nrow = 1
  )
  
  graph.arranged <- plot_grid(
    plotlist = list(
      colnames,
      graph.arranged
    ),
    ncol = 1, 
    nrow = 2,
    rel_heights = c(ratio.colnames, 1)
  )
  
  graph.arranged <- plot_grid(
    plotlist = list(
      rownames,
      graph.arranged
    ),
    ncol = 2, 
    nrow = 1,
    rel_widths = c(1/10, 1)
  )
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
  return(NULL)
  
}