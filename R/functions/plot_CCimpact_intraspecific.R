plot_CCimpact_intraspecific <- function(
  species_list,
  db_species.name,
  list_climate,
  period.initial,
  list_db_impact, 
  variable.focus, 
  interval.length, 
  label.x, 
  label.y, 
  text.size.main,
  text.size.axis,
  angle.x,
  graph.name, 
  height,
  width,
  dir.saving
){
  

  # prepare climate
  db_climate <- rbindlist(lapply(species_list, function(species.selected){
    
    db_climate.selected <- list_climate[[species.selected]][[1]]$year[, c("stand", "climatic_year", variable.focus), with = FALSE]
  
    db_climate.selected <- db_climate.selected[climatic_year %in% period.initial,
                                     lapply(.SD, mean), by = "stand", , .SDcols = variable.focus]
    
    return(data.table(db_climate.selected, species.code = species.selected))
    
  }))  
  setnames(db_climate, variable.focus, "variable.focus")
    
  # prepare impact
  db_impact <- rbindlist(lapply(species_list, function(species.selected){
    
    db_impact.selected <- list_db_impact[[species.selected]][,.(stand, impact = height.gap.rel, species.code = species_code)]
  
    return(db_impact.selected)
    
  }))
    
  # merge
  db_impact_climate <- merge(db_climate, db_impact, by = c("stand", "species.code"))
  
  # species list in alphabetical order
  db_species.name <- db_species.name[code %in% species_list][order(name)]
  species_list_ordered <- db_species.name$code
  
  # build graph axis
  
    # x-axis

    min.climate <- min(db_climate[, variable.focus])
    max.climate <- max(db_climate[, variable.focus])
  
    min.x <- interval.length * min.climate %/% interval.length
    max.x <- interval.length * max.climate %/% interval.length + interval.length 
    breaks <- seq(min.x - interval.length/2 , max.x + interval.length/2, interval.length)
    labels <- seq(min.x , max.x, interval.length)
    
    # y-axis
    y.min <- -0.10
    y.max <- 0.35
    
  # enter species specific code
    
    # plot
    
    boxplot_impact_climate <- lapply(species_list_ordered, function(species.selected){
      
      db_impact_species.selected <- db_impact_climate[species.code == species.selected]
  
      # create climate groups
      db_impact_species.selected[, group := as.numeric(as.character(cut( # as.numeric to be able to manipulate the x-scale, as.character to have a correct conversion from factor to numeric (avoid going to 1, 2..., n)
        variable.focus,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      )))]
      
      # summary (median, Q1, Q3, etc)
  
      db_impact_species.selected_summary <- db_impact_species.selected[
        , .(
          stand.number = length(unique(stand)),
          median = median(impact),
          Q1 = quantile(impact, 0.25),
          Q3 = quantile(impact, 0.75)
        ), by = group
      ]
      db_impact_species.selected_summary[, ":="(
        whisker_low_min = Q1 - 1.5 *(Q3 - Q1),
        whisker_high_max = Q3 + 1.5 *(Q3 - Q1)
      )]
      
      # proper whisker identification
      
      for(group.selected in db_impact_species.selected_summary$group){
        
        whisker_high_max <- db_impact_species.selected_summary[group == group.selected, whisker_high_max]
        whisker_high.computed <- db_impact_species.selected[group == group.selected & impact <= whisker_high_max, max(impact)]
        
        whisker_low_min <- db_impact_species.selected_summary[group == group.selected, whisker_low_min]
        whisker_low.computed <- db_impact_species.selected[group == group.selected & impact >= whisker_low_min, min(impact)]
        
        db_impact_species.selected_summary[group == group.selected, ":="(
          whisker_low = whisker_low.computed,
          whisker_high = whisker_high.computed
        )]
        
      }
      
      # identify cases with whiskers are out of the box
      db_whisker_info_low <- db_impact_species.selected_summary[whisker_low < y.min - 0.02, .(group, label = whisker_low)]
      db_whisker_info_high <- db_impact_species.selected_summary[whisker_high > y.max + 0.02, .(group, label = whisker_high)]
      
      # identify group with less than 10 stands
      db_impact_species.selected <- merge(
        db_impact_species.selected,
        db_impact_species.selected_summary[, .(group, stand.number)],
        by = "group"
      )
      db_impact_species.selected[, nb.stands.group := ifelse(stand.number >= 10, "high", "low")]

      # plot
      boxplot_impact_climate.selected <- ggplot(
        data = db_impact_species.selected, 
        aes(
          x = group,
          y = impact * 100, 
          group = group
        )
      ) + 
        geom_boxplot(
          aes(
            color = nb.stands.group
          ),
          outlier.shape = NA,
          lwd = 0.1
        ) + 
        geom_text(
          data = db_impact_species.selected_summary,
          aes(
            x = group,
            y = 100 * Q1,
            label = stand.number
          ),
          size = 0.2 * text.size.main,
          angle = 0,
          nudge_y = - 1
        ) +
        geom_text(
          data = db_whisker_info_low,
          aes(
            x = group,
            y = 100 * y.min,
            label = paste0(round(100 * label)," %")
          ),
          size = 0.15 * text.size.main,
          angle = 90,
          nudge_y = 0,
          nudge_x = -0.15
        ) +
        geom_text(
          data = db_whisker_info_high,
          aes(
            x = group,
            y = 100 * y.max,
            label = paste0(round(100 * label)," %")
          ),
          size = 0.15 * text.size.main,
          angle = 90,
          nudge_y = 0.5,
          nudge_x = -0.15
        ) +
        geom_abline(
          slope = 0, 
          intercept = 0,
          color = "red", 
          lwd = 0.1
        ) +
        coord_cartesian(
          ylim = c(100 * y.min, 100 * y.max)
        ) +
        scale_x_continuous(
          limits = c(min(labels), max(labels)),
          breaks = labels, 
          minor_breaks = F
          ) +
        scale_y_continuous(
          breaks = seq(100 * y.min, 100 * y.max, 5),
          minor_breaks = NULL,
          labels = paste0(seq(100 * y.min, 100 * y.max, 5), "%")
        ) +
        scale_color_manual(
          name = "Stand number",
          values = c("grey", "black"),
          breaks = c("low", "high"),
          labels = c("less than 10 stands", "more than 10 stands")
        ) +
        labs(
          x = label.x, 
          y = label.y,
          title = db_species.name[code == species.selected, name]
        ) +
        theme(
          axis.text.x = element_text(angle = angle.x, size = text.size.axis, hjust = 1),
          axis.text.y = element_text(size = text.size.axis), 
          axis.title = element_blank(),
          plot.title = element_text(vjust = 0, size = text.size.axis),
          legend.key.height = unit(0.5, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=text.size.main), #change legend title font size
          legend.text = element_text(size=text.size.main),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      
      # boxplot_impact_climate.selected <- ggExtra::ggMarginal(
      #   boxplot_impact_climate.selected,
      #   type = "histogram",
      #   binwidth = 1
      #   )
      
      return(boxplot_impact_climate.selected)
    })
    names(boxplot_impact_climate) <- species_list_ordered
    
    # arrange plot
    graph.arranged_1_16 <- plot_grid(
      plotlist = lapply(boxplot_impact_climate[1:16], function(x){
        x <- x +
          theme(
            legend.position="none",
            plot.margin = unit(c(0,0.1,0.1,0.1), "cm"),
            plot.title = element_text(vjust = 0, size = 1.2 * text.size.main)
          )
        }),
      ncol = 4,
      nrow = 4
    ) +
    draw_label(label.x, x = 0.5, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main) +
    draw_label(label.y, x = 0, y = 0.25, hjust = 0, vjust = 0.5, angle = 90, size = text.size.main) 
    
    graph.arranged_17_18 <- plot_grid(
      plotlist = lapply(boxplot_impact_climate[17:18], function(x){
        x <- x +
          theme(
            legend.position="none",
            plot.margin = unit(c(0,0.1,0.1,0.1), "cm"),
            plot.title = element_text(vjust = 0, size = 1.2 * text.size.main)
          )
      }),
      ncol = 4,
      nrow = 1
    ) +
      draw_label(label.x, x = 0.25, y = 0, hjust = 0.5, vjust = 0, angle = 0, size = text.size.main)
    
    
    graph.arranged <- plot_grid(
      plotlist = list(graph.arranged_1_16, graph.arranged_17_18), 
      nrow = 2, 
      rel_heights = c(4,1)
    ) +
    theme(plot.margin = unit(c(0.1,0,0.2,0.1), "cm"))
    
    # add legend
    # legend <- get_legend(boxplot_impact_climate[[1]])
    # 
    # graph.arranged <- plot_grid(
    #   graph.arranged,
    #   legend,
    #   nrow = 2,
    #   rel_heights = c(1, .07)
    #   )+
    #   theme(plot.margin = unit(c(0,0,0,00), "cm"))
    
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
  
  return(boxplot_impact_climate)
  
}
