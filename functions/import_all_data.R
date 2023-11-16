import_all_data <- function(zip.file,db_name,new.names) {
  
  data_list <- lapply(db_name, import_data, zip.file=zip.file)
  names(data_list) <- new.names
  
  return(data_list)
}
