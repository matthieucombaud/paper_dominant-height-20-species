plot_direct.partial.impact_vertical <- function(
  species_list,
  list_variable.impact,
  db_species.name,
  graph.name,
  dir.saving
){
  
  # parameters
  graph.name = "graph_direct.partial.impact_vertical"
  text.size.main = 5
  height = 25
  width = 17
  ymin = 10
  ymax = 45
  
  # enter the function
  
  db_variable_impact <- rbindlist(list_variable.impact)[species %in% species_list]
  db_variable_impact <- merge(db_variable_impact, db_species.name[, .(code, name, name.short)], by.x  = "species", by.y = "code")
  
  db_variable_impact$name <- as.factor(db_variable_impact$name)
  
  db_variable_impact[, season := paste0(str_split(variable, "_", simplify = T)[,2],"_",str_split(variable, "_", simplify = T)[,3])]
  db_variable_impact[, season.name := character()]
  db_variable_impact[ season == "9_11", season.name := "Autumn"]
  db_variable_impact[ season == "12_2", season.name := "Winter"]
  db_variable_impact[ season == "9_2", season.name := "Autumn + winter"]
  db_variable_impact[ season == "3_5", season.name := "Spring"]
  db_variable_impact[ season == "6_8", season.name := "Summer"]
  db_variable_impact[ season == "3_8", season.name := "Spring + summer"]
  db_variable_impact[ season == "1_12", season.name := "Whole year"]
  
  # build legend
  
  # vec.color <- rainbow(length(levels(db_variable_impact$name)))
  vec.color <-  c(
    "#FF0000",
    "#FF4D00",
    "#FF9900",
    "#8B8878",
    "#458B00",
    "#6E8B3D",
    "#8B6914",
    "#008B00",
    "#8B3A62",
    "#008B8B",
    "#FF8C69",
    "#00B3FF",
    "#0066FF",
    "#001AFF",
    "#3300FF",
    "#7F00FF",
    "#CC00FF",
    "#FF00E6",
    "#FF0099",
    "#FF004D"
  )
  
  vec.color <- vec.color[1 : length(levels(db_variable_impact$name))] # in case we deal with a subsample of species
  
  db_legend <- db_species.name[name %in% levels(db_variable_impact$name)]
  db_legend[, ":="(color = vec.color, label = paste0(name, " (", name.short, ")"))]
  
  barplot(1 : length(levels(db_variable_impact$name)), col = vec.color)
  
  # variables to create loop on
  list_season.name <- c("Autumn","Winter","Autumn + winter","Spring","Summer","Spring + summer","Whole year")
  variable.type <- c("Tmean", "precipitation", "cwb", "sgdd")
  
  list_graph_partial.impact <- lapply(variable.type, function(variable.type.selected){
    
    list_graph_partial.impact_variable.type.selected <- lapply(list_season.name, function(season.name.selected){
      
      db_variable_impact.selected <- db_variable_impact[
        str_detect(variable, variable.type.selected) & season.name == season.name.selected
      ]
      
      if(nrow(db_variable_impact.selected) == 0){return(NULL)} # case no significant variable
      
      db_label.selected <- unique(db_variable_impact.selected[, .(
        x = value_abs[3] + 0.2 * abs(value_abs[1]),
        y = SDH[3] + 2,
        name = name, 
        name.short = name.short
      ),
      by = c("species", "variable")]
      )
      
      db_legend.selected <- db_legend[name %in% db_variable_impact.selected$name]
      
      
      graph_partial.impact_variable.type.selected_season.selected <- ggplot(
      ) +
        geom_line(
          data = db_variable_impact.selected, 
          aes(
            x = value_abs,
            y = SDH, 
            color = name
          ),
          linewidth = 0.1
        ) +
        geom_text_repel(
          data = db_label.selected,
          aes(
            x = x,
            y = y, 
            color = name,
            label = name.short
          ),
          size = text.size.main / 2,
          min.segment.length = unit(1, "cm"), 
          arrow = arrow()
        ) +
        ylim(ymin, ymax) +
        scale_linetype_manual(
          values = c("TRUE" = "solid", "FALSE" = "dashed")
        ) +
        scale_color_manual(
          values = db_legend.selected$color,
          breaks = db_legend.selected$name, 
          labels = db_legend.selected$label
        ) +
        guides(
          color = guide_legend(title = "Species", nrow = 5, byrow = TRUE),
          # color = "none",
          # linetype = guide_legend(title = NULL, keywidth = 2),
          linetype = "none",
          size = "none"
        ) +
        labs(
          x = variable.type.selected, 
          y = "SDH at 70 years"
        ) + 
        theme(legend.direction ="horizontal")
      
      return(graph_partial.impact_variable.type.selected_season.selected)
      
    })
    names(list_graph_partial.impact_variable.type.selected) <- list_season.name
    
    return(list_graph_partial.impact_variable.type.selected)
    
  })
  names(list_graph_partial.impact) <- variable.type
  
  # plot
  
  # margins.graph <- c(0,-0.1,0.1,-0.1)
  margins.graph <- c(0, 0, 0.1, 0)
  angle.graph <- 45
  
  # get legend ----
  
  graph.temp <- ggplot(
    data = db_variable_impact,
    aes(
      x = name,
      y = SDH, 
      color = name
    )
  ) +
    geom_boxplot(
    ) +
    scale_color_manual(
      values = db_legend$color,
      name = db_legend$name, 
      label = db_legend$label
    ) +
    guides(
      color = guide_legend(title = "Species", ncol = 3, byrow = TRUE),
      linetype = "none",
      size = "none"
    )+
    theme(
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.title = element_text(size = text.size.main), #change legend title font size
      legend.text = element_text(size = text.size.main)
    )
  
  legend <- get_legend(graph.temp)
  
  # get variables and seasons ----
  
  ratio.season.graph <- 1/20
  
  text.season <- plot_grid(
    plotlist = lapply(list_season.name, function(season.name.selected){
      text_grob(season.name.selected, rot = 90, size = 1.2 * text.size.main)
    }),
    ncol = 1,
    nrow = length(list_season.name)
  )
  
  text.variable <- plot_grid(
    plotlist = lapply(c("Mean temperature", "Precipitation", "Climatic water balance"), function(season.name.selected){
      text_grob(season.name.selected, size = 1.2 * text.size.main, just = "top")
    }),
    ncol = 3,
    nrow = 1
  )
  
  text.variable <- plot_grid(
    plotlist = list(
      NULL,
      text.variable
    ),
    ncol = 2,
    nrow = 1,
    rel_widths = c(ratio.season.graph, 1)
  )
  
  
  # graph temperature and sgdd ----
  
  # attention we plot together tempeture for non whole year period and gdd for the whole year period, since no whole yera temperature is never significant
  
  graph_sgdd <- plot_grid(
    plotlist = lapply("Whole year", function(season.selected){
      
      x <- list_graph_partial.impact$sgdd[[season.selected]]
      
      if(is.null(x)){
        
        x <- text_grob("No effect", size = 1.2 * text.size.main)
        
        return(x)
        
      }
      
      x <- x + 
        labs(y = "SDH (m)") +
        theme(
          legend.position = "none",
          plot.title = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust = 0.5, vjust = 0, angle = 90, size = text.size.main),
          axis.text = element_text(size = text.size.main),
          axis.text.x = element_text(angle = angle.graph, hjust = 1)
        )
      
      return(x)
      
    }),
    nrow = 1,
    ncol = 1
  ) +
    draw_label("SGDD (°C)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main)
  
  graph_temperature <- plot_grid(
    plotlist = lapply(setdiff(list_season.name, "Whole year"), function(season.selected){
      
      x <- list_graph_partial.impact$Tmean[[season.selected]]
      
      if(is.null(x)){
        
        x <- text_grob("No effect", size = 1.2 * text.size.main)
        
        return(x)
        
      }
      
      
      x <- x +
        labs(y = "SDH (m)") +
        theme(
          legend.position = "none",
          plot.title = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust = 0.5, vjust = 0, angle = 90, size = text.size.main),
          axis.text = element_text(size = text.size.main),
          axis.text.x = element_text(angle = angle.graph, hjust = 1)
        )
      
      return(x)
      
    }),
    ncol = 1,
    nrow = 6
  ) +
    draw_label("Temperature (°C)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main)
  
  graph_temperature_sgdd <- plot_grid(
    plotlist = list(
      graph_temperature,
      graph_sgdd
    ),
    ncol = 1,
    nrow = 2,
    rel_heights = c(6, 1)
  ) +
    theme(plot.margin = unit(margins.graph, "cm"))
  
  
  # precipitations ----
  
  graph_precipitations <- plot_grid(
    plotlist = lapply(setdiff(list_season.name, "Whole year"), function(season.selected){
      
      x <- list_graph_partial.impact$precipitation[[season.selected]]
      
      if(is.null(x)){
        
        x <- text_grob("No effect", size = 1.2 * text.size.main)
        
        return(x)
        
      }
      
      x <- x + theme(
        legend.position = "none",
        plot.title = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_text(size = text.size.main),
        axis.text.x = element_text(angle = angle.graph, hjust = 1)
      )
      
      return(x)
      
    }),
    ncol = 1,
    nrow = 6
  ) +
    draw_label("Precipitations (mm)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main) +
    # draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = text.size.main)+
    theme(plot.margin = unit(margins.graph, "cm"))
  
  
  # cwb ----
  
  graph_cwb <- plot_grid(
    plotlist = lapply(setdiff(list_season.name, "Whole year"), function(season.selected){
      
      x <- list_graph_partial.impact$cwb[[season.selected]]
      
      if(is.null(x)){
        
        x <- text_grob("No effect", size = 1.2 * text.size.main)
        
        return(x)
        
      }
      
      x <- x + theme(
        legend.position = "none",
        plot.title = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_text(size = text.size.main),
        axis.text.x = element_text(angle = angle.graph, hjust = 1)
      )
      
      return(x)
      
    }),
    ncol = 1,
    nrow = 6
  ) +
    draw_label("Climatic water balance (mm)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main) +
    # draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = text.size.main)+
    theme(plot.margin = unit(margins.graph, "cm"))
  
  # arrange ----
  
  graph_precipitations_cwb <- plot_grid(
    plotlist = list(
      graph_precipitations,
      graph_cwb
    ),
    ncol = 2,
    nrow = 1
  )
  
  graph_precipitations_cwb_legend <- plot_grid(
    plotlist = list(
      graph_precipitations_cwb,
      legend
    ),
    ncol = 1,
    nrow = 2, 
    rel_heights = c(6, 1)
  )
  
  graph_precipitations_cwb_legend_temperature <- plot_grid(
    plotlist = list(
      graph_temperature_sgdd,
      graph_precipitations_cwb_legend
    ),
    ncol = 2,
    nrow = 1,
    rel_widths = c(1, 2)
  )
  
  graph_all <- plot_grid(
    plotlist = list(
      text.season,
      graph_precipitations_cwb_legend_temperature
    ),
    ncol = 2,
    nrow = 1,
    rel_widths = c(ratio.season.graph, 1)
  )
  
  graph_all <- plot_grid(
    plotlist = list(
      text.variable,
      graph_all
    ),
    ncol = 1,
    nrow = 2,
    rel_heights = c(1/30, 1)
  ) +
    theme(plot.margin = unit(c(0,0,0.1,0), "cm"))
  
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
  
  return(graph_all)
  
}