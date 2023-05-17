plot_climate.evolution.distribution <- function(
  list_climate.ref,
  list_climate.interest,
  period.averaging.ref,
  period.averaging.interest,
  species_list, 
  variable.focus, # variables for which to plot the comparison
  db_species.name # db containing the correspondence species name / species code
){
  
  # size parameters
  text.size.main = 7
  height = 15
  width = 19
  
  
  # compute
  
  db_climate <- rbindlist(lapply(species_list, function(species.selected){
    
    print(species.selected)
    
    # reference climate
    db_climate_ref_species.selected <- list_climate.ref[[species.selected]][[1]]$year[climatic_year %in% period.averaging.ref, lapply(.SD, function(x){mean(x)}) , by = stand, .SDcols = variable.focus]
    db_climate_ref_species.selected <- melt(db_climate_ref_species.selected, id.vars = "stand", variable.name = "variable", value.name = "value")
    
    # interest climate
    db_climate_interest_species.selected <- list_climate.interest[[species.selected]][[1]]$year[climatic_year %in% period.averaging.interest, lapply(.SD, function(x){mean(x)}) , by = stand, .SDcols = variable.focus]
    db_climate_interest_species.selected <- melt(db_climate_interest_species.selected, id.vars = "stand", variable.name = "variable", value.name = "value")
    
    # merge
    db_climate_species.selected <- merge(
      db_climate_ref_species.selected, 
      db_climate_interest_species.selected, 
      by = c("stand", "variable"),
      suffixe = c(".ref", ".interest")
    )
    
    # compute evolution
    db_climate_species.selected[, evolution := value.interest - value.ref]
    
    # add species
    db_climate_species.selected[, species_code := species.selected]
    
    return(db_climate_species.selected)
    
  }))
  
  
  # add name and order
  db_climate <- merge(db_climate, db_species.name, by.x = "species_code", by.y = "code")
  factor.ordered <- db_species.name[order(name, decreasing = TRUE), name]
  db_climate$name <- factor(db_climate$name, levels = factor.ordered)
  
  # plot
  list_boxplot_climate <- lapply(variable.focus, function(variable.selected){
    
    db_climate_temp <- db_climate[variable == variable.selected]

    # units
    if(str_detect(variable.selected, "Tmean")){ units = "°C"}
    if(str_detect(variable.selected, "sgdd")){ units = "°C"}
    if(str_detect(variable.selected, "precipitation")){ units = "mm"}
    if(str_detect(variable.selected, "cwb")){ units = "mm"}
    
    # summary
    db_climate_temp_summary <- db_climate_temp[,.(
      N = .N, 
      median = median(evolution), 
      Q3 = quantile(evolution, 0.75)
    ),
    by = c("name")][match(factor.ordered, name)]
    
    
    # plot
    boxplot_climate_variable.selected <-   ggplot(
      data = db_climate_temp,
      aes(
        y = name,
        x = evolution
      )
    ) + 
      geom_boxplot(
        outlier.shape = NA,
        lwd = 0.1
      ) +
      geom_vline(xintercept = 0, lty = "dashed", linewidth = 0.2) +
      geom_text(
        data = db_climate_temp_summary,
        aes(
          y = name,
          x = Q3 + 1.5 * (Q3 - median),
          label = N
        ),
        size = 1.2,  # works, I don't know why...
        angle = 0,
        nudge_y = 0.3
      ) +
      labs(
        y = NULL, 
        x = paste0("Evolution of ", variable.selected, "(", units,")")
      )
    
    return(boxplot_climate_variable.selected)
    
  })
  names(list_boxplot_climate) <- variable.focus


  return(list_boxplot_climate)

}