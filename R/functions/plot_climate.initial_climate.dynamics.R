plot_climate.initial_climate.dynamics <- function(
  list_boxplot_climate.initial, 
  graph_Tmean,
  graph_precipitation,
  graph_cwb,
  graph.name,
  dir.saving
){

  # size parameters
  text.size.main = 7
  height = 15
  width = 19
  
  # graph reference period
  
  graph_climate.reference <- plot_grid(
    plotlist = list(
      list_boxplot_climate.initial[["Tmean_9_8"]] +
        xlim(c(0,NA)) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = text.size.main),
          plot.margin = unit(c(0.4,0,0,0), "cm")
        ) +
        geom_vline(xintercept = 0, lty = "dashed", linewidth = 0.2),
      list_boxplot_climate.initial[["precipitation_9_8"]] +
        xlim(c(0,NA)) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = text.size.main),
          axis.text.y = element_blank(),
          plot.margin = unit(c(0.4,0,0,0), "cm")
        ) +
        geom_vline(xintercept = 0, lty = "dashed", linewidth = 0.2),
      list_boxplot_climate.initial[["cwb_9_8"]] +
        # xlim(c(0,16)) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = text.size.main),
          axis.text.y = element_blank(),
          plot.margin = unit(c(0.4,0,0,0), "cm")
        ) +
        geom_vline(xintercept = 0, lty = "dashed", linewidth = 0.2)
    ),
    labels = c(
      "Annual mean temperature (°C)",
      "Annual precipitation (mm)",
      "Annual climatic water balance (mm)"
    ),
    ncol = 3,
    rel_widths = c(1.4,1,1),
    label_size = text.size.main,
    label_x = c(0.4, 0.1, 0.1),
    label_y = 1,
    hjust = 0,
    vjust = 1.5
  ) + theme(
    plot.margin = unit(c(0.4,0,0.2,0.1), "cm")
  )
  
  # graph climate dynamics ----
    
    # get legend
    graph_Tmean_temp <- graph_Tmean
    graph_Tmean_temp <- graph_Tmean_temp + theme(
      legend.direction ="horizontal",
      legend.text = element_text(size = text.size.main), 
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.title = element_text(size = text.size.main) #change legend title font size
      ) +
      scale_fill_manual(
        name = "Climate anomaly",
        values = c("red", "blue"),
        breaks = c("positive", "negative"), 
        labels = c("positive", "negative")
      )
    legend.dynamics.Tmean<- get_legend(graph_Tmean_temp)
    
    graph_precipitation_temp <- graph_precipitation
    graph_precipitation_temp <- graph_precipitation_temp + theme(
      legend.direction ="horizontal",
      legend.text = element_text(size = text.size.main), 
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.title = element_text(size = text.size.main) #change legend title font size
    )+
      scale_fill_manual(
        name = "Climate anomaly",
        values = c("green", "orange"),
        breaks = c("positive", "negative"), 
        labels = c("positive", "negative")
      )
    legend.dynamics.precipitation <- get_legend(graph_precipitation_temp)
    
    # plot
    graph_climate.dynamics <- plot_grid(
      plotlist = list(
          graph_Tmean +
            # geom_line(aes(y=data.table::frollmean(value.recent, 5, align = "center", na.rm = T)), col = "red") +
            theme(
              legend.position = "none",
              plot.title = element_blank(),
              plot.margin = unit(c(0.4,0,0,0), "cm"),
              axis.text = element_text(size = text.size.main),
              axis.title = element_text(size = text.size.main)
            ) +
            scale_fill_manual(
              name = "Climate anomaly",
              values = c("red", "blue"),
              breaks = c("positive", "negative"), 
              labels = c("positive", "negative")
            ) ,
          graph_precipitation +
            # geom_line(aes(y=data.table::frollmean(value.recent, 5, align = "center", na.rm = T)), col = "red") +
            theme(
              legend.position = "none",
              plot.title = element_blank(),
              plot.margin = unit(c(0.4,0,0,0), "cm"),
              axis.text = element_text(size = text.size.main),
              axis.title = element_text(size = text.size.main)
            ) +
            scale_fill_manual(
              name = "Climate anomaly",
              values = c("green", "orange"),
              breaks = c("positive", "negative"), 
              labels = c("positive", "negative")
            ),
          graph_cwb +
            # geom_line(aes(y=data.table::frollmean(value.recent, 5, align = "center", na.rm = T)), col = "red") +
            theme(
              legend.position = "none",
              plot.title = element_blank(),
              plot.margin = unit(c(0.4,0,0,0), "cm"),
              axis.text = element_text(size = text.size.main),
              axis.title = element_text(size = text.size.main)
            ) +
            scale_fill_manual(
              name = "Climate anomaly",
              values = c("green", "orange"),
              breaks = c("positive", "negative"), 
              labels = c("positive", "negative")
            )
          ), 
      labels = c(
        "Annual mean temperature anomaly (°C)",
        "Annual precipitation anomaly (mm)",
        "Annual climatic water balance anomaly (mm)"
        ),
      ncol = 3,
      label_size = text.size.main,
      label_x = 0.1,
      label_y = 1,
      hjust = 0,
      vjust = 1.5
    )
    
    # legend
    
    graph_climate.dynamics_legend <- plot_grid(
      plotlist = list(legend.dynamics.Tmean, legend.dynamics.precipitation), 
      ncol = 2, 
      rel_widths = c(1,2)
    )
  
  # all
  graph_all <- plot_grid(
    plotlist = list(
      graph_climate.reference,
      graph_climate.dynamics + theme(
        plot.margin = unit(c(0.5,0,0,0), "cm")
      ),
      graph_climate.dynamics_legend
    ),
    nrow = 3,
    labels = c(
      "Reference climate (average 1891- 1920)",
      "Climate anomaly compared to reference climate (average 1891-1920)", 
      NULL
      ),
    label_size = 1.3 * text.size.main,
    label_x = 0,
    label_y = 1,
    hjust = 0,
    vjust = 1.5,
    rel_heights = c(1,1,0.1)
  )
  
  # save
  ggsave(
    filename = paste0(graph.name, ".jpg"),
    plot = graph_all,
    path = dir.saving,
    height = height,
    width = width,
    units = "cm",
    limitsize = FALSE
  )
  
}
