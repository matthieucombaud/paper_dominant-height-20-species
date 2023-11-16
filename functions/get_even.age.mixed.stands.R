get_even.age.mixed.stands <- function(
  db.ifn.complete,
  stands.even.aged,
  list.species,
  db.species.name
){

  # subselect even-aged stands
  db.ifn.modified <- db.ifn.complete[stand %in% stands.even.aged]
  
  # rename
  setnames(db.ifn.modified, c("Sp1.AGE13.HTOT","Sp2.AGE13.HTOT"), c("sp1", "sp2"))
  setnames(db.ifn.modified, c("Value1.AGE13","Value2.AGE13"), c("age_sp1", "age_sp2"))
  setnames(db.ifn.modified, c("Value1.HTOT","Value2.HTOT"), c("h_sp1", "h_sp2"))

  
  # add species names
  db.ifn.modified <- merge(db.ifn.modified, db.species.name[,.(code, name_sp1 = name, name.short_sp1 = name.short)], by.x  = "sp1", by.y = "code")
  db.ifn.modified <- merge(db.ifn.modified, db.species.name[,.(code, name_sp2 = name, name.short_sp2 = name.short)], by.x  = "sp2", by.y = "code")
  
  
  # subselect mixed stand in the sense of "age was measured on two different species"
  db.ifn.modified <- db.ifn.modified[sp1 %in% list.species & sp2 %in% list.species & sp1 != sp2]
  
  # harmonize sp1 and sp2
  
  db.ifn.modified <- db.ifn.modified[name_sp1 > name_sp2, ":="(
    name_sp1 = name_sp2,
    name_sp2 = name_sp1,
    name.short_sp1 = name.short_sp2,
    name.short_sp2 = name.short_sp1,
    sp1 = sp2,
    sp2 = sp1, 
    age_sp1 = age_sp2,
    age_sp2 = age_sp1,
    h_sp1 = h_sp2,
    h_sp2 = h_sp1
  )]
  
  # factor and order species
  order.species.name.short <- db.species.name[code %in% list.species][order(name.short), name.short]
  
  db.ifn.modified$name.short_sp1 <- factor(db.ifn.modified$name.short_sp1, levels = order.species.name.short)
  db.ifn.modified$name.short_sp2 <- factor(db.ifn.modified$name.short_sp2, levels = order.species.name.short)
  
  # table
  db.ifn.modified[, table(name.short_sp1, name.short_sp2)]
  
  
  # add TCL and / or Gupper criteria
  
    # appariate sp1 / sp2 and the corresponding TCL
    
    db.ifn.modified[Sp1.TCL == sp1, TCL.rel_sp1 := Value1.TCL/ TCL.tot]
    db.ifn.modified[Sp1.TCL == sp2, TCL.rel_sp2 := Value1.TCL/ TCL.tot]
    
    db.ifn.modified[Sp2.TCL == sp1, TCL.rel_sp1 := Value2.TCL/ TCL.tot]
    db.ifn.modified[Sp2.TCL == sp2, TCL.rel_sp2 := Value2.TCL/ TCL.tot]
    
    db.ifn.modified[Sp3.TCL == sp1, TCL.rel_sp1 := Value3.TCL/ TCL.tot]
    db.ifn.modified[Sp3.TCL == sp2, TCL.rel_sp2 := Value3.TCL/ TCL.tot]
    
    db.ifn.modified[, TCL.rel_other := 1- (TCL.rel_sp1 + TCL.rel_sp2)]
    
    # appariate sp1 / sp2 and the corresponding Gupper
    
    db.ifn.modified[Sp1.Gupper == sp1, Gupper.rel_sp1 := Value1.Gupper / Gupper]
    db.ifn.modified[Sp1.Gupper == sp2, Gupper.rel_sp2 := Value1.Gupper / Gupper]
    
    db.ifn.modified[Sp2.Gupper == sp1, Gupper.rel_sp1 := Value2.Gupper / Gupper]
    db.ifn.modified[Sp2.Gupper == sp2, Gupper.rel_sp2 := Value2.Gupper / Gupper]
    
    db.ifn.modified[Sp3.Gupper == sp1, Gupper.rel_sp1 := Value3.Gupper / Gupper]
    db.ifn.modified[Sp3.Gupper == sp2, Gupper.rel_sp2 := Value3.Gupper / Gupper]
    

    db.ifn.modified[, Gupper.rel_other := 1- (Gupper.rel_sp1 + Gupper.rel_sp2)]
    
    # add criteria on TCL
    db.ifn.modified[, mixed.TCL := (TCL.rel_sp1 + TCL.rel_sp2 > 0.75 & TCL.rel_sp1 < 0.75 & TCL.rel_sp2 < 0.75)]
    
      # # table
      # db.ifn.modified[mixed.TCL == TRUE][, table(name.short_sp1, name.short_sp2)]
      # 
      # # analyze gap with the table without selection
      # 
      # db.ifn.modified[, table(mixed.TCL, useNA = "always")]
      # 
      # db.ifn.modified[mixed.TCL == FALSE][,.(sp1, sp2, age_sp1, age_sp2, h_sp1, h_sp2, TCLr_sp1, TCLr_sp2, Gupperr_sp1, Gupperr_sp2)]
      # db.ifn.modified[is.na(mixed.TCL)][,.(sp1, sp2, age_sp1, age_sp2, h_sp1, h_sp2, TCLr_sp1, TCLr_sp2, Gupperr_sp1, Gupperr_sp2)]
    
    # add criteria on Gupper
    db.ifn.modified[, mixed.Gupper := (Gupper.rel_sp1 + Gupper.rel_sp2 > 0.80 & Gupper.rel_sp1 > Gupper.rel_other & Gupper.rel_sp2 > Gupper.rel_other)]
    
      # # table
      # db.ifn.modified[mixed.Gupper == TRUE][, table(name.short_sp1, name.short_sp2)]
      # 
      # # analyze gap with the table without selection
      # 
      # db.ifn.modified[, table(mixed.Gupper, useNA = "always")]
      # 
      # db.ifn.modified[mixed.Gupper == FALSE][,.(sp1, sp2, age_sp1, age_sp2, h_sp1, h_sp2, TCLr_sp1, TCLr_sp2, Gupperr_sp1, Gupperr_sp2)]
      # db.ifn.modified[is.na(mixed.Gupper)][,.(sp1, sp2, age_sp1, age_sp2, h_sp1, h_sp2, TCLr_sp1, TCLr_sp2, Gupperr_sp1, Gupperr_sp2)]
      
    # compare criteria
    # db.ifn.modified[, table(mixed.TCL, mixed.Gupper, useNA = "always")] 
      
  return(db.ifn.modified)
  
}

