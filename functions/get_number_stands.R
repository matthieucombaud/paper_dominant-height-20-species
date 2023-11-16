get_number_stands <- function(db_ifn, stands_selected, db_species_name, saving.dir){
  
  dir.create(saving.dir, recursive = T)
  
  db_stand.number <- as.data.table(db_ifn[stand %in% stands_selected, table(Sp1.TCL)])
  
  setnames(db_stand.number, old = c("Sp1.TCL"), new = "species.code")
  
  db_stand.number <- merge(db_stand.number, db_species_name, by.x = "species.code", by.y = "code", all.x = T)
  
  db_stand.number <- db_stand.number[order(db_stand.number$N, decreasing = T)]
  
  write.csv(db_stand.number, paste0(saving.dir,"/db_stand.number.csv"),row.names = F)
  
  return(db_stand.number)
  
}
