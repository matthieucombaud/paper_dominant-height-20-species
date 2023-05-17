plot_climate.initial <- function(
  list_climate, # list of climate per species and varying variables
  species_list, 
  period.initial, # initial period of the comparison
  variable.focus, # variables for which to plot the comparison
  db_species.name # db containing the correspondence species name / species code
){
  
  
  # compute
  
  db_climate <- rbindlist(lapply(species_list, function(species.selected){
    
    print(species.selected)
    
    # initial climate
    db_climate_species.selected <- list_climate[[species.selected]]$all.CC$year[climatic_year %in% period.initial, lapply(.SD, function(x){mean(x)}) , by = stand, .SDcols = variable.focus]
    db_climate_species.selected <- melt(db_climate_species.selected, id.vars = "stand", variable.name = "variable", value.name = "value.initial")
    
    # add species
    db_climate_species.selected[, species_code := species.selected]
    
    return(db_climate_species.selected)
    
  }))
  
  # plot
  
  db_climate <- merge(db_climate, db_species.name, by.x = "species_code", by.y = "code")
  
  period.initial.text <- paste0(min(period.initial), "-", max(period.initial))

  list_boxplot_climate <- lapply(variable.focus, function(variable.selected){
    
    db_climate_temp <- db_climate[variable == variable.selected]
    
    # order by value.initial (decreasing)
    db_value.initial.median <- db_climate_temp[, .(value.initial.median = median(value.initial)), by = name.short]
    db_value.initial.median <- db_value.initial.median[order(value.initial.median, decreasing = F)]
    
    factor.ordered <- db_value.initial.median$name.short # 'all.CC_year' is the observed climate
    db_climate_temp$name.short <- factor(db_climate_temp$name.short, levels = factor.ordered)
    
    # units
    if(str_detect(variable.selected, "Tmean")){ units = "°C"}
    if(str_detect(variable.selected, "sgdd")){ units = "°C"}
    if(str_detect(variable.selected, "precipitation")){ units = "mm"}
    if(str_detect(variable.selected, "cwb")){ units = "mm"}
    
    # summary
    db_climate_temp_summary <- db_climate_temp[,.(
      N = .N, 
      median = median(value.initial), 
      Q3 = quantile(value.initial, 0.75)
    ),
    by = c("name.short")][match(factor.ordered, name.short)]
    
    
    # plot
    boxplot_climate_variable.selected <-   ggplot(
      data = db_climate_temp,
      aes(
        x = name.short,
        y = value.initial
      )
    ) + 
      geom_boxplot(
        outlier.shape = NA
      ) +
      geom_text(
        data = db_climate_temp_summary,
        aes(
          x = name.short,
          y = Q3 + 1.5 * (Q3 - median),
          label = N
        ),
        size = 1,  # works, I don't know why...
        angle = 90,
        nudge_x = 0.3
        
      ) +
      # stat_summary(fun.data = function(x){return(c(y = min(x) - 0.05 * abs(min(x)),label = length(x)))}, geom = "text") +  # add number of points
      theme(axis.text.x = element_text(
        angle = 90
      )) +
      labs(
        x = NULL, 
        y = paste0("mean of ", variable.selected," over ",period.initial.text, " (", units,")")
      )
    
    return(boxplot_climate_variable.selected)
    
  })
  names(list_boxplot_climate) <- variable.focus

  return(list_boxplot_climate)

}