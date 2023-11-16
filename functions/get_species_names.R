get_species_names <- function(zip_file, file_metadata, saving.dir){
  
  dir.create(saving.dir, recursive = T)
  
  db_species_name <- fread(unzip(zip_file, file_metadata))
  
  setnames(db_species_name, old = c("// espar", "lib_espar", "lib_cdref") , new = c("code","name_fr", "name"))
  db_species_name[stringr::str_length(code) == 1, code := paste0("0", code)] # to avoid problem with the code inferior to 10
  
  write.csv(db_species_name, paste0(saving.dir,"/db_species_name.csv"),row.names = F)
  
  return(db_species_name)
  
}
  