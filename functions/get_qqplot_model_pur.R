get_qqplot_model_pur <- function(
  dir.model, 
  species_list
){
  
  
  list_qqplot <- lapply(species_list, function(species.selected){
    
    model <- readRDS(paste0(dir.model,"/sp",species.selected,"_no_batch.rds"))
    
    # get error parameter
    sigma <- model$batch_1$db_parameter_normalized[param == "sigma", estimate]
    delta <- model$batch_1$db_parameter_normalized[param == "delta", estimate]
    
    # compute standardized residuals
    data <- data.table(h_obs = model$batch_1$height_observed, h_sim = model$batch_1$height_simulated)
    data[, res := h_obs - h_sim]
    data[, res_std := res / (sigma * h_sim ^ delta)]
    
    # qq plot
    data <- data[order(res_std)]
    n <- nrow(data)
    
    for(i in 1 : n){
      
      data[i, q_theo := qnorm(p = i/n, mean = 0, sd = 1)]
      
    }
    
    qqplot.selected <- ggplot(data = data) + 
      geom_point(aes(x = q_theo, y = res_std)) +
      geom_abline(intercept = 0 , slope = 1) + 
      labs(x = "theorerical quantiles", y = "standardized residuals") + 
      ggtitle(paste0("Species ", species.selected))
    
    
    return(qqplot.selected)
    
  })
  names(list_qqplot) <- species_list
  
  return(list_qqplot)

}