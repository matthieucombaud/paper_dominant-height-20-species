# output
# list of
# a db "db.significance" with summary statistics (incl.significance) for pairs with more than "n_threshold" observations
# a db "db.small.sample" with all observations for pairs with less than "n_threshold" observations

summarize_ME <- function(
  db_ME, 
  n_threshold, # min number of stand to analyze significance,
  list_species
){
  
  db.significance <- rbindlist(lapply(list_species, function(species.focal.selected){
    
    print(species.focal.selected)
    
    db.small.sample.selected <- rbindlist(lapply(list_species, function(species.companion.selected){
      
      print(paste0("***", species.companion.selected))
      
      db_ME.selected <- db_ME[sp_focal == species.focal.selected & sp_companion == species.companion.selected]
      N <- nrow(db_ME.selected)
      
      if(N == 0){ # especially when species.focal.selected == species.companion.selected
        
        db_temp <- unique(db_ME[sp_focal == species.focal.selected, .(sp_focal,name_focal, name.short_focal)])
        
        
        return(
          data.table(
            sp_focal = db_temp$sp_focal,
            name_focal = db_temp$name_focal,
            name.short_focal = db_temp$name.short_focal,
            sp_companion = db_temp$sp_focal,
            name_companion = db_temp$name_focal,
            name.short_companion = db_temp$name.short_focal,
            N = N,
            p.value = NA_real_, 
            star = NA_character_, 
            conf.int.lower = NA_real_, 
            conf.int.upper = NA_real_,
            mean = NA_real_,
            sd = NA_real_,
            Q1 = NA_real_,
            Q2 = NA_real_,
            Q3 = NA_real_
          )
        )
      }
      
      db_summary <- unique(db_ME.selected[, .(sp_focal,name_focal, name.short_focal, sp_companion, name_companion, name.short_companion, N)])
      
      if(N < n_threshold){
        return(
          data.table(
            db_summary,
            p.value = NA_real_, 
            star = NA_character_, 
            conf.int.lower = NA_real_, 
            conf.int.upper = NA_real_,
            mean = NA_real_,
            sd = NA_real_,
            Q1 = NA_real_,
            Q2 = NA_real_,
            Q3 = NA_real_
          )
        )
      }
      
      # analyze significance and distribution
      c.quantile <- quantile(db_ME.selected[, ME], c(0.25, 0.5, 0.75))
      
      t.test.output <- t.test(db_ME.selected[, ME])
      
      p.value <- t.test.output$p.value
      
      star <- case_when(
        p.value < 10^-3 ~ "***",
        p.value < 10^-2 ~ "**",
        p.value < 5*10^-2 ~ "*",
        p.value >= 5*10^-2 ~ "n.s."
      )
      
      return(data.table(
        db_summary,
        p.value = p.value, 
        star = star, 
        conf.int.lower = t.test.output$conf.int[1], 
        conf.int.upper = t.test.output$conf.int[2],
        mean = db_ME.selected[, mean(ME)],
        sd = db_ME.selected[, sd(ME)],
        Q1 = c.quantile[1],
        Q2 = c.quantile[2],
        Q3 = c.quantile[3]
      ))
      
    }))
    
    return(db.small.sample.selected)
    
  }))
  
  # set as factor
  db.significance$name_focal <- factor(db.significance$name_focal)
  db.significance$name_companion <- factor(db.significance$name_companion)

  return(db.significance)
  
}
