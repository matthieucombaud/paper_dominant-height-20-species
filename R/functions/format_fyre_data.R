format_fyre_data <- function(data,start_date){
  
  # convert into dt
  db_fyre <- as.data.table(data) 
  
  # name db_fyre
  
    # colnames
    require(lubridate)
    c_date <- as.Date(start_date) + days(1:dim(db_fyre)[2])# get dates vector
    names(db_fyre) <- as.character(c_date)
    
    # site
    db_fyre <- cbind(site=rownames(db_fyre),db_fyre)
  
  # melt
  db_fyre <- melt(data=db_fyre,id.vars = "site",variable.name = "date",value.name = "value")
  
  return(db_fyre)
  
}
