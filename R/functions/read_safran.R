read_safran<-function(file){

  # attention: here I use directly the safran zip file, because extracting it is very costly in terms of memory. But then it is costly in time to modify the climate safran data, since you need to relaunch a vitrual extraction each time

  # the database is too heavy to load it directly, so I load site by site. This requires to know the exact number of row per site, which is determined by the three lines below
  nb.considered.years<-length(c(1959:2020))
  nb.bisextile.years<-length(bisextile.years<-seq(1960,2020,4))
  row.per.site<-nb.considered.years*365+nb.bisextile.years*1
    
  # import climatic data
  db.safran <- read_delim_chunked(
    file=file,
    delim=";",
    escape_double = FALSE,
    col_names = T,
    callback =DataFrameCallback$new(aggregate_safran_monthly),
    chunk_size = row.per.site,
    trim_ws = TRUE,
    progress=T
    )
  
  return(db.safran)
    
}
