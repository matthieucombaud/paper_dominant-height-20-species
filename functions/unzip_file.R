unzip_file<-function(input.directory,zip.folder,files,new.folder){
  
  unzip(
    zipfile=paste0(input.directory,"/",zip.folder),
    files = files,
    list = FALSE,
    overwrite = TRUE,
    junkpaths = FALSE,
    exdir = paste0(input.directory,"/",new.folder),
    unzip = "internal",
    setTimes = FALSE
    )
 
  return(T)
   
}


# unzip_file(
#   input.directory="C:/Users/matthieu.combaud/Documents/R_local/thesis_part1/data/climate/safran",
#   zip.folder="projet_d_extraction_876_20220131.zip",
#   files="siclima_extraction_876_20220131.csv",
#   new.folder="safran_db_unzip"
# )
# 
# 
# unzip(paste0(input.directory,"/",zip.folder), exdir = paste0(input.directory,"/",new.folder))
