compare_SI <- function(
  list_list_db_impact,
  db_species.name,
  species_list, 
  comparison.age
){
  
  db_impact_comparison <- rbindlist(lapply(1 : length(list_list_db_impact), function(index){
    
    name_temp <- names(list_list_db_impact)[index]
    db_temp <- rbindlist(list_list_db_impact[[index]])
    db_temp <- db_temp[, .(stand, height.gap.rel, species_code, period = name_temp)]
    
    return(db_temp)
    
  }))
  
  db_impact_comparison <- merge(
    db_impact_comparison, 
    db_species.name[, .(code, name)], 
    by.x = "species_code",
    by.y = "code"
  )
  
  
  # summary (median, Q1, Q3, etc)
  
  db_impact_summary <- db_impact_comparison[
    , .(
      stand.number = length(unique(stand)),
      median = median(height.gap.rel),
      Q1 = quantile(height.gap.rel, 0.25),
      Q3 = quantile(height.gap.rel, 0.75) 
    ), by =  c("name", "period", "species_code")
  ]
  db_impact_summary[, ":="(
    whisker_low_min = Q1 - 1.5 *(Q3 - Q1),
    whisker_high_max = Q3 + 1.5 *(Q3 - Q1)
  )]
  # db_impact_summary <- db_impact_summary[order(median, decreasing = F)]
  
  # factor.ordered <- db_impact_summary$name.short # 'all.CC_year' is the observed climate
  # db_impact$name.short <- factor(db_impact$name.short, levels = factor.ordered)
  
  # proper whisker identification
  
  for(species.selected in species_list){
    
    for(period.selected in names(list_list_db_impact)){
      
      whisker_high_max <- db_impact_summary[species_code == species.selected & period == period.selected, whisker_high_max]
      whisker_high.computed <- db_impact_comparison[species_code == species.selected & period == period.selected & height.gap.rel <= whisker_high_max, max(height.gap.rel)]
      
      whisker_low_min <- db_impact_summary[species_code == species.selected & period == period.selected, whisker_low_min]
      whisker_low.computed <- db_impact_comparison[species_code == species.selected & period == period.selected & height.gap.rel >= whisker_low_min, min(height.gap.rel)]
      
      db_impact_summary[species_code == species.selected  & period == period.selected, ":="(
        whisker_low = whisker_low.computed,
        whisker_high = whisker_high.computed
      )]
      
    }
    
  }
  
  # ploting parameters
  text.size.main = 7
  y.min = -0.2
  y.max = 0.2
  
  # identify cases with whiskers are out of the box
  db_whisker_info_low <- db_impact_summary[whisker_low < y.min - 0.02, .(name, species_code, period, label = whisker_low)]
  db_whisker_info_high <- db_impact_summary[whisker_high > y.max + 0.02, .(name, species_code, period, label = whisker_high)]
  
  list_boxplot <- lapply(species_list, function(species.selected){
    
    species.name <- db_species.name[code == species.selected, name]
    
    boxplot_species.selected <- ggplot(
      data = db_impact_comparison[species_code == species.selected], 
      aes(
        x = period,
        y = height.gap.rel * 100
      )
    ) +
      geom_abline(
        slope = 0, 
        intercept = 0,
        color = "red"
      ) +
      geom_boxplot(
        outlier.shape = NA,
        lwd = 0.1
      ) + 
      geom_text(
        data = db_impact_summary[species_code == species.selected],
        aes(
          x = period,
          y = 100 * Q3,
          label = stand.number
        ),
        size = 0.3 * text.size.main,
        angle = 0,
        nudge_y = 0.5
      ) +
      geom_text(
        data = db_whisker_info_low[species_code == species.selected],
        aes(
          x = period,
          y = 100 * y.min,
          label = paste0(round(100 * label)," %")
        ),
        size = text.size.main / 4,
        angle = 90,
        nudge_y = 0,
        nudge_x = -0.15
      ) +
      geom_text(
        data = db_whisker_info_high[species_code == species.selected],
        aes(
          x = period,
          y = 100 * y.max,
          label = paste0(round(100 * label)," %")
        ),
        size = text.size.main / 4,
        angle = 90,
        nudge_y = 0.5,
        nudge_x = -0.15
      ) +
      labs(
        title = species.name,
        x = "Period", 
        y = paste0("Impact of climate change on SDH at ",comparison.age," years (%)")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, size = text.size.main, hjust = 1),
        axis.text.y = element_text(size = text.size.main), 
        axis.title = element_blank()
      ) +     
      coord_cartesian(
        ylim = c(100 * y.min, 100 * y.max)
      ) +
      scale_y_continuous(
        breaks = round(seq(y.min*100, y.max*100, 5)),
        minor_breaks = NULL,
        labels = paste0(round(seq(y.min*100, y.max*100, 5)), "%")
      )
    
    return(boxplot_species.selected)
  
  })
  names(list_boxplot) <- species_list
  
  return(list_boxplot)

}