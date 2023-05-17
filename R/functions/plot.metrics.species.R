plot.metrics.species <- function(
  list_metrics,
  dir.saving
){
  
  list_species <- sapply(list_metrics, function(analysis.metrics.selected){
    return(analysis.metrics.selected$species_code)
  })
  names(list_metrics) <- list_species
  
  
  list_graph <- lapply(list_species, function(species.selected){
    
    analysis.metrics.selected <- list_metrics[[species.selected]]
    
    db_metrics_plot <- rbind(
      analysis.metrics.selected$list_metrics.submodel,
      data.table(
        species_code = species.selected,
        batch = "full_sample",
        rmse.calibration = analysis.metrics.selected$list_metrics.model.main$rmse_model.main,
        rmse.validation = numeric(),
        bias.calibration = analysis.metrics.selected$list_metrics.model.main$bias_model.main,
        bias.validation = numeric(),
        bias.abs.calibration = analysis.metrics.selected$list_metrics.model.main$bias.abs_model.main,
        bias.abs.validation = numeric()
      )
    )
    
    
    # rmse
    db_rmse_plot <- melt(
      data = db_metrics_plot[,.(species_code, batch, rmse.calibration, rmse.validation)],
      id.vars =  c("species_code", "batch"),
      value.var = c("rmse.calibration", "rmse.validation"),
      variable.name = "type",
      value.name = "value"
    )
    
    db_rmse_plot[, batch.prepared := str_remove(batch, "batch_")]
    db_rmse_plot[batch == "full_sample", batch.prepared := "all"]
    
    graph_rmse <- ggplot(
      data = db_rmse_plot,
      aes(
        x = batch.prepared,
        y = value,
        fill = type
      )
    ) +
      geom_col(
        position = position_dodge2()
      )+
      scale_fill_manual(
        values = c("grey", "black"),
        breaks = c("rmse.calibration", "rmse.validation"), 
        labels = c("calibration", "validation")
      )+
      labs(y = "rmse (m)")+
      theme(
        # axis.title.x = element_blank(),
        axis.text.x = element_text()
      )
    
    # bias
    db_bias_plot <- melt(
      data = db_metrics_plot[,.(species_code, batch, bias.calibration, bias.validation)],
      id.vars =  c("species_code", "batch"),
      value.var = c("bias.calibration", "bias.validation"),
      variable.name = "type",
      value.name = "value"
    )
    
    db_bias_plot[, batch.prepared := str_remove(batch, "batch_")]
    db_bias_plot[batch == "full_sample", batch.prepared := "all"]
    
    graph_bias <- ggplot(
      data = db_bias_plot,
      aes(
        x = batch.prepared,
        y = value,
        fill = type
      )
    ) +
      geom_col(
        position = position_dodge2()
      )+
      scale_fill_manual(
        values = c("grey", "black"),
        breaks = c("bias.calibration", "bias.validation"), 
        labels = c("calibration", "validation")
      )+
      labs(y = "bias (m)")+
      theme(
        # axis.title.x = element_blank(),
        axis.text.x = element_text()
      )
    
    # bias.abs
    db_bias.abs_plot <- melt(
      data = db_metrics_plot[,.(species_code, batch, bias.abs.calibration, bias.abs.validation)],
      id.vars =  c("species_code", "batch"),
      value.var = c("bias.abs.calibration", "bias.abs.validation"),
      variable.name = "type",
      value.name = "value"
    )
    
    db_bias.abs_plot[, batch.prepared := str_remove(batch, "batch_")]
    db_bias.abs_plot[batch == "full_sample", batch.prepared := "all"]
    
    graph_bias.abs <- ggplot(
      data = db_bias.abs_plot,
      aes(
        x = batch.prepared,
        y = value,
        fill = type
      )
    ) +
      geom_col(
        position = position_dodge2()
      )+
      scale_fill_manual(
        values = c("grey", "black"),
        breaks = c("bias.abs.calibration", "bias.abs.validation"), 
        labels = c("calibration", "validation")
      )+
      labs(y = "bias.abs (m)")+
      theme(
        # axis.title.x = element_blank(),
        axis.text.x = element_text()
      )
    
    return(list(
      graph_rmse = graph_rmse, 
      graph_bias = graph_bias,
      graph_bias.abs = graph_bias.abs
    ))
    
  })
  names(list_graph) <- list_species
  
  
  # arrange
  
  
    # rmse
  
    legend <- get_legend(list_graph[[1]]$graph_rmse)
    
    list_graph_prepared_rmse <- lapply(list_graph, function(graph){
      
      graph <- graph$graph_rmse +
        theme(
          axis.title.x = element_blank()
        ) +
        guides(
          fill = "none"
        ) +
        ylim(c(0,5))
      
      return(graph)
      
    })
    
    graph_rmse <- plot_grid(
      plotlist = list_graph_prepared_rmse,
      nrow = 5,
      ncol = 4, 
      labels = list_species, 
      hjust = 0.1
    )
    
    graph_rmse <- plot_grid(
      plotlist = list(graph_rmse, legend),
      nrow = 2,
      ncol = 1,
      rel_heights = c(1,0.1)
    )
    
    ggsave(
      filename = "graph_rmse_species.jpg",
      plot = graph_rmse,
      path = dir.saving,
      height = 22, 
      width = 17, 
      units = "cm"
    )
  
    # bias
    
    legend <- get_legend(list_graph[[1]]$graph_bias)
    
    list_graph_prepared_bias <- lapply(list_graph, function(graph){
      
      graph <- graph$graph_bias +
        theme(
          axis.title.x = element_blank()
        ) +
        guides(
          fill = "none"
        ) +
        ylim(c(-2.5,2.5))
      
      return(graph)
      
    })
    
    graph_bias <- plot_grid(
      plotlist = list_graph_prepared_bias,
      nrow = 5,
      ncol = 4, 
      labels = list_species, 
      hjust = 0.1
    )
    
    graph_bias <- plot_grid(
      plotlist = list(graph_bias, legend),
      nrow = 2,
      ncol = 1,
      rel_heights = c(1,0.1)
    )
    
    ggsave(
      filename = "graph_bias_species.jpg",
      plot = graph_bias,
      path = dir.saving,
      height = 22, 
      width = 17, 
      units = "cm"
    )
    
    # bias.abs
    
    legend <- get_legend(list_graph[[1]]$graph_bias.abs)
    
    list_graph_prepared_bias.abs <- lapply(list_graph, function(graph){
      
      graph <- graph$graph_bias.abs +
        theme(
          axis.title.x = element_blank()
        ) +
        guides(
          fill = "none"
        ) +
        ylim(c(0,5))
      
      return(graph)
      
    })
    
    graph_bias.abs <- plot_grid(
      plotlist = list_graph_prepared_bias.abs,
      nrow = 5,
      ncol = 4, 
      labels = list_species, 
      hjust = 0.1
    )
    
    graph_bias.abs <- plot_grid(
      plotlist = list(graph_bias.abs, legend),
      nrow = 2,
      ncol = 1,
      rel_heights = c(1,0.1)
    )
    
    ggsave(
      filename = "graph_bias.abs_species.jpg",
      plot = graph_bias.abs,
      path = dir.saving,
      height = 22, 
      width = 17, 
      units = "cm"
    )
    
    
  return(NULL)

}