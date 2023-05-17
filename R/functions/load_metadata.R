load_metadata<-function(zip.file){ # load metadata (stored in zip.file in input.directory)
  
  # first import a ugly file, juste to detet the good lines
  db.definition_all<-as.data.table(read_delim(unz(zip.file, "metadonnees.csv"),skip_empty_rows = F,col_names=F,
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE))
  
  db.definition_all<-db.definition_all[,1]
  names(db.definition_all)<-"names"
  
  begin.data<-which(str_detect(db.definition_all$names,"Définition des données"))
  begin.units<-which(str_detect(db.definition_all$names,"Definition des unités"))
  begin.modalities<-which(str_detect(db.definition_all$names,"Définition des modalités pour chaque unité"))

  # data
  db.definition_data<-as.data.table(read_delim(unz(zip.file, "metadonnees.csv"),skip_empty_rows = F,col_names=T,
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE, skip = begin.data,n_max=begin.units-begin.data-1-1))
  names(db.definition_data)<-c("code","libelle","definition")
  
  # units
  db.definition_units<-as.data.table(read_delim(unz(zip.file, "metadonnees.csv"),skip_empty_rows = F,col_names=T,
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE, skip = begin.units,n_max=begin.modalities-begin.units-1-1))
  names(db.definition_units)<-c("donnee","unite","campagnes","type","libelle","definition")
  
  # modalities
  db.definition_modalities<-as.data.table(read_delim(unz(zip.file, "metadonnees.csv"),skip_empty_rows = F,col_names=F, #colnames =F because several rows with headers
                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE, skip = begin.modalities+1)) # +1 to exclude rownames 
  
  names(db.definition_modalities)<-c("unite","code","libelle","definition","comments")
  db.definition_modalities<-db.definition_modalities[db.definition_modalities$code!="Code",] # delete useless lines (false headers)
  
  return(list(
    variable=db.definition_data,
    units=db.definition_units,
    modalities=db.definition_modalities
  ))
  
}







