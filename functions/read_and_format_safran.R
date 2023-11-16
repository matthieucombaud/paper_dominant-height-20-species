read_and_format_safran <- function(safran_zip,safran_file){
  
  db_safran <- fread(unzip(safran_zip, safran_file))

  # to have the save format as FYRE
  db_safran[,Month:=as.character(Month)]
  db_safran[,Month:=ifelse(Month%in%c(10,11,12),Month,paste0("0",Month))]
  
  db_safran[,date := paste0(Year,"_",Month)]
  db_safran[,precipitation := preliq_q + prenei_q]
  
  setnames(db_safran,old=c("t_q","Site"),new=c("temperature","cell"))
  
  db_safran_monthly <- db_safran[,.(temperature = mean(temperature), precipitation = sum(precipitation)),by=c("cell","date")]
  
  # wide format to keep homogeneity with FYRE and the function "combine_fyre_safran"
  db_safran_average_temperature_wide <- dcast(data=db_safran_monthly[,.(date,cell,temperature)],formula=date~cell) # by default, value.var will be the remaining variable
  db_safran_sum_precipitation_wide <- dcast(data=db_safran_monthly[,.(date,cell,precipitation)],formula=date~cell) # by default, value.var will be the remaining variable
  
  return(list(
    temperature = db_safran_average_temperature_wide,
    precipitation = db_safran_sum_precipitation_wide
  ))
  
}
