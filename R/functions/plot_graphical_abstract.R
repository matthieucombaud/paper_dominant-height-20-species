plot_graphical_abstract <- function(
  list_db_impact,
  db_species.name,
  comparison.age,
  y.min,
  y.max,
  graph.name,
  dir.saving
  ){
  
  # plotting parameters
  text.size.main = 7
  height = 12
  width = 17
  
  # get a single db with species
  db_impact <- rbindlist(list_db_impact)
  
  # species as factor
  db_impact[, species_code := as.factor(species_code)]
  
  # add species names
  db_impact <- merge(
    db_impact, 
    db_species.name, 
    by.x = "species_code",
    by.y = "code"
    )
  
  # summary (median, Q1, Q3, etc)
  
  db_impact_summary <- db_impact[
      , .(
        stand.number = length(unique(stand)),
        median = median(height.gap.rel),
        Q1 = quantile(height.gap.rel, 0.25),
        Q3 = quantile(height.gap.rel, 0.75)
        ), by = name
      ]
  db_impact_summary[, ":="(
    whisker_low_min = Q1 - 1.5 *(Q3 - Q1),
    whisker_high_max = Q3 + 1.5 *(Q3 - Q1)
  )]
  db_impact_summary <- db_impact_summary[order(median, decreasing = F)]
  
  factor.ordered <- db_impact_summary$name # 'all.CC_year' is the observed climate
  db_impact$name <- factor(db_impact$name, levels = factor.ordered)
  
  # proper whisker identification

  for(name.selected in factor.ordered){
    
    whisker_high_max <- db_impact_summary[name == name.selected, whisker_high_max]
    whisker_high.computed <- db_impact[name == name.selected & height.gap.rel <= whisker_high_max, max(height.gap.rel)]
    
    whisker_low_min <- db_impact_summary[name == name.selected, whisker_low_min]
    whisker_low.computed <- db_impact[name == name.selected & height.gap.rel >= whisker_low_min, min(height.gap.rel)]
    
    db_impact_summary[name == name.selected, ":="(
      whisker_low = whisker_low.computed,
      whisker_high = whisker_high.computed
    )]
    
  }

  # identify cases with whiskers are out of the box
  db_whisker_info_low <- db_impact_summary[whisker_low < y.min - 0.02, .(name, label = whisker_low)]
  db_whisker_info_high <- db_impact_summary[whisker_high > y.max + 0.02, .(name, label = whisker_high)]

  
  # plot boxplot ----
  
  boxplot_impact_interspecific <- ggplot(
      data = db_impact, 
      aes(
        x = name,
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
      data = db_impact_summary,
      aes(
        x = name,
        y = 100 * Q3,
        label = stand.number
      ),
      size = 0.3 * text.size.main,
      angle = 0,
      nudge_y = 0.5
    ) +
    geom_text(
      data = db_whisker_info_low,
      aes(
        x = name,
        y = 100 * y.min,
        label = paste0(round(100 * label)," %")
      ),
      size = text.size.main / 4,
      angle = 90,
      nudge_y = 0,
      nudge_x = -0.15
    ) +
    geom_text(
      data = db_whisker_info_high,
      aes(
        x = name,
        y = 100 * y.max,
        label = paste0(round(100 * label)," %")
      ),
      size = text.size.main / 4,
      angle = 90,
      nudge_y = 0.5,
      nudge_x = -0.15
    ) +
    labs(
      x = "Species", 
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
      breaks = round(seq(-10, 20, 5)),
      minor_breaks = NULL,
      labels = paste0(round(seq(-10, 20, 5)), "%")
    )
  

    ggsave(
      filename = paste0(graph.name, ".jpg"),
      plot = boxplot_impact_interspecific,
      path = dir.saving,
      height = height, 
      width = width, 
      units = "cm",
      limitsize = FALSE
    )
    
  return(NULL)
  
}



