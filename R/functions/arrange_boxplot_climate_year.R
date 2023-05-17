arrange_boxplot_climate_year <- function(
  plotlist,
  graph.name,
  dir.saving
){

  height = 12
  width = 17
  text.size.main = 5
  
  variable.to.plot <- c("Tmean_9_8", "precipitation_9_8", "cwb_9_8")
  plotlist <- plotlist[names(plotlist) %in% variable.to.plot]
  
  # remove title
  plotlist.prepared <- lapply(names(plotlist), function(name.graph.selected){
    
    graph.selected <- plotlist[[name.graph.selected]] +
      geom_boxplot(
        lwd = 0.05,
        outlier.shape = NA
      ) +
      geom_vline(xintercept = 0, lty = "dashed", linewidth = 0.2) +
      theme(
        plot.title = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = text.size.main),
        axis.text = element_text(size = text.size.main)
      )
    return(graph.selected)
    
  })
  names(plotlist.prepared) <- names(plotlist)
  
  
  graph.arranged <- plot_grid(
    plotlist = list(
      plotlist.prepared[[1]] +
        theme(
          axis.title.y = element_blank()
        ) +
        xlab("Annual mean temperatures (°C)"),
      plotlist.prepared[[2]] +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
        ) +
        xlab("Annual precipitation (mm)"),
      plotlist.prepared[[3]] +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
          ) +
        xlab("Annual climatic water balance (mm)")
    ),
    ncol = 3,
    nrow = 1,
    rel_widths = c(1.4, 1, 1)
  ) + theme(
    plot.margin = unit(c(0.4,0,0.2,0.1), "cm")
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