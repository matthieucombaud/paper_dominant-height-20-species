plot_indirect.partial.impact <- function(
  species_list,
  list_partial.impact,
  db_species.name,
  ymin,
  ymax,
  text.size.main,
  graph.name,
  dir.saving, 
  height,
  width, 
  dir.model,
  model.type,
  batch
){
  
  
  # correspondance season name - season month
  db_season <- rbind(
    data.table(season = "9_11", season.name = "Autumn"),
    data.table(season = "12_2", season.name = "Winter"),
    data.table(season = "9_2", season.name = "Autumn + winter"),
    data.table(season = "3_5", season.name = "Spring"),
    data.table(season = "6_8", season.name = "Summer"),
    data.table(season = "3_8", season.name = "Spring + summer"),
    data.table(season = "9_8", season.name = "Whole year")
  )

  
  # identify relevant variable per species
  
  list_climate.variables <- lapply(species_list, function(species.selected){
    
    model <- readRDS(paste0(dir.model,"/sp",species.selected,"_",model.type,".rds"))[[batch]]
    db_parameter <- model$db_parameter_normalized
    db_parameter$param_class <- factor(db_parameter$param_class,levels=c("intercept","alpha","gamma","beta","sigma","delta"))
    climate.variable <- unlist(sapply(c("Tmean", "cwb", "precipitation", "sgdd"), function(variable.climate.generic.selected){
      
      climate.variable.selected <- db_parameter[stringr::str_detect(param,variable.climate.generic.selected),param]
      return(climate.variable.selected)
      
    }))
    climate.variable <- unname(climate.variable)
    climate.variable_no.square <- unique(str_remove(climate.variable,"_square")) # required for next steps. Attention: also keep "climate.variable" with square
    
    
    climate.variable_no.square <- list(
      Tmean = str_remove(climate.variable_no.square[str_detect(climate.variable_no.square, "Tmean")], "Tmean_"), 
      precipitation = str_remove(climate.variable_no.square[str_detect(climate.variable_no.square, "precipitation")], "precipitation_"), 
      cwb = str_remove(climate.variable_no.square[str_detect(climate.variable_no.square, "cwb")], "cwb_"), 
      sgdd = str_remove(climate.variable_no.square[str_detect(climate.variable_no.square, "sgdd")], "sgdd_")
    )
    
    if(length(climate.variable_no.square$sgdd) > 0 ){ climate.variable_no.square$sgdd <- "9_8"} # correction to use the "whole year" variable in list_partial.impact, which is indexd by "9_8"
    
    return(climate.variable_no.square)
    
  })
  names(list_climate.variables) <- species_list
  
  
  # deal with partial impact
  
  db_partial.impact <- rbindlist(list_partial.impact)[species %in% species_list]
  
  # delete case with no impact
  db_partial.impact[, variation := (length(unique(SDH)) > 1), by = c("species", "variable")]
  db_partial.impact <- db_partial.impact[variation == TRUE]
  
  
  db_partial.impact <- merge(db_partial.impact, db_species.name[, .(code, name, name.short)], by.x  = "species", by.y = "code")
  db_partial.impact$name <- as.factor(db_partial.impact$name)
  
  db_partial.impact[, season := paste0(str_split(variable, "_", simplify = T)[,2],"_",str_split(variable, "_", simplify = T)[,3])]
  
  db_partial.impact <- merge(db_partial.impact, db_season, by = "season")

  # build legend

  # vec.color <- rainbow(length(levels(db_partial.impact$name)))
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
  
  vec.color <- vec.color[1 : length(levels(db_partial.impact$name))] # in case we deal with a subsample of species
  
  db_legend <- db_species.name[name %in% levels(db_partial.impact$name)]
  db_legend[, ":="(color = vec.color, label = paste0(name, " (", name.short, ")"))]

  barplot(1 : length(levels(db_partial.impact$name)), col = vec.color)
  
  # variables to create loop on
  list_season.name <- c("Autumn", "Winter", "Autumn + winter", "Spring", "Summer", "Spring + summer", "Whole year")
  variable.type <- c("Tmean", "precipitation")
  
  
  
  
  
  list_graph_partial.impact <- lapply(variable.type, function(variable.type.selected){
    
    list_graph_partial.impact_variable.type.selected <- lapply(list_season.name, function(season.name.selected){
      
      db_partial.impact.selected <- db_partial.impact[
        str_detect(variable, variable.type.selected) & season.name == season.name.selected
      ]

      # we plot only the species for which the selected season explicitly appear in the parameter database for either "variable.type.selected" or cwb : avoid to plot indirect effect (such as attributing an effect to Tmean_3_5 while it is an effect of Tmean_3_8) 
    
      season_month <- db_season[season.name == season.name.selected, season]
      
      # check if we plot the species or not
      species.plot <- unlist(lapply(unique(db_partial.impact.selected$species), function(species.selected){
        
        if(
          season_month %in% list_climate.variables[[species.selected]][[variable.type.selected]] |
          season_month %in% list_climate.variables[[species.selected]][["cwb"]]  |
          (season_month %in% list_climate.variables[[species.selected]][["sgdd"]] & variable.type.selected == "Tmean")
          ){return(species.selected)}
        
        return(NULL)
        
      }))
      
      
      db_partial.impact.selected <- db_partial.impact.selected[species %in% species.plot]
      
      
      if(nrow(db_partial.impact.selected) == 0){return(NULL)} # case no significant variable
      
      # plot
      
      db_label.selected <- unique(db_partial.impact.selected[, .(
        x = value_abs[3] + 0.2 * abs(value_abs[1]),
        y = SDH[3] + 2,
        name = name, 
        name.short = name.short
        ),
        by = c("species", "variable")]
        )
      
      db_legend.selected <- db_legend[name %in% db_partial.impact.selected$name]
      
      
      graph_partial.impact_variable.type.selected_season.selected <- ggplot(
      ) +
        geom_line(
          data = db_partial.impact.selected, 
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
        data = db_partial.impact,
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
        color = guide_legend(title = "Species", ncol = 1, byrow = TRUE),
        linetype = "none",
        size = "none"
      )+
      theme(
        legend.key.height = unit(0.2, 'cm'), #change legend key height
        legend.key.width = unit(0.2, 'cm'), #change legend key width
        legend.title = element_text(size = 0.8 * text.size.main), #change legend title font size
        legend.text = element_text(size = 0.8 * text.size.main)
      )
      
    legend <- get_legend(graph.temp)
    
    # get variables and seasons ----

    ratio.season.graph <- 1/30
    
    text.season <- plot_grid(
      plotlist = lapply(list_season.name, function(season.name.selected){
        text_grob(season.name.selected, size = 1.2 * text.size.main)
      }),
      nrow = 1,
      ncol = length(list_season.name)
    )
    
    text.variable <- plot_grid(
      plotlist = lapply(c("Temperature", "Precipitations"), function(season.name.selected){
        text_grob(season.name.selected, size = 1.2 * text.size.main, rot = 90, just = "top")
      }),
      nrow = 2,
      ncol = 1
    )
    
    text.variable <- plot_grid(
      plotlist = list(
        NULL,
        text.variable
      ),
      nrow = 2,
      ncol = 1,
      rel_heights = c(ratio.season.graph, 1)
    )
    
    
    # graph temperature ----
    
    
    graph_temperature <- plot_grid(
      plotlist = lapply(list_season.name, function(season.selected){
          
          x <- list_graph_partial.impact$Tmean[[season.selected]]
          
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
      nrow = 1,
      ncol = length(list_season.name)
      ) +
      draw_label("Temperature (°C)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main) +
      draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = text.size.main) +
      theme(plot.margin = unit(margins.graph, "cm"))
    
    # precipitations ----
  
    graph_precipitations <- plot_grid(
      plotlist = lapply(list_season.name, function(season.selected){
        
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
      nrow = 1,
      ncol = length(list_season.name)
    ) +
      draw_label("Precipitations (mm)", x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main) +
      draw_label("SDH (m)", x = 0, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = text.size.main)+
      theme(plot.margin = unit(margins.graph, "cm"))
  

    # arrange ----
    
    graph_all <- plot_grid(
      plotlist = list(
        graph_temperature,
        graph_precipitations
      ),
      nrow = 2,
      ncol = 1
    )
    
    graph_all <- plot_grid(
      plotlist = list(
        text.season,
        graph_all
      ),
      nrow = 2,
      ncol = 1,
      rel_heights = c(ratio.season.graph, 1)
    )
    
    graph_all <- plot_grid(
      plotlist = list(
        text.variable,
        graph_all
      ),
      nrow = 1,
      ncol = 2,
      rel_widths = c(1/15, 1)
    )
    
    graph_all <- plot_grid(
      plotlist = list(
        graph_all,
        legend
      ),
      nrow = 1,
      ncol = 2, 
      rel_widths = c(4, 1)
    )
    
    graph_all <- graph_all +
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
