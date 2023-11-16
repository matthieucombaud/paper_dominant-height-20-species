import_data<-function(db_name,zip.file){
  
  db<-as.data.table(read.table(unz(zip.file, paste0(db_name,".csv")), header = TRUE, sep = ";", dec = ".", encoding = "UTF-8"))

  setnames(db, "CAMPAGNE", "YEAR",skip_absent=TRUE) # the variable "CAMPAGNE" is renamed "YEAR" to make clear it is the year of the visit, nevermind if it is the first or second visit
  db[,YEAR_FIRST_VISIT:=trunc(IDP/100000)+2005] # based on the IFN codification
  db[,VISIT_INDEX:=ifelse(YEAR_FIRST_VISIT==YEAR,"first",ifelse(YEAR_FIRST_VISIT==YEAR-5,"second","problem"))] # attention: this variable tells us if the year of the measurement corresponds to the (normal) year of the first visit or the (normal) year of the second visit, according to IDP number. However, for some stands no measurements were done during the (normal) year of the first visit because land use was not included in the survey at that time, but at the remeasurement time the land use is now is nt scope (due to land use change): then there are some measurement. IFN classifies that as "first visit", whereas I classify that as "second visit". Both info are present in db.stand, and it is possible to access discrepancies with db.stand[,table(VISITE,VISIT_INDEX,useNA="always")] 
  db[,YEAR_FIRST_VISIT:=NULL] # not useful once the visit index has been determinated (cf line supra)
  db[,X:=NULL] # rm the X variable (error in the importation)
  
  return(db)

}
