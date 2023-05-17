unbiase_safran <- function(db_safran,db_fyre){
  
  # reshape and merge
  db_fyre <- melt(db_fyre,id.vars="date",variable.name = "cell",value.name="value_fyre",variable.factor=F)
  db_safran <- melt(db_safran,id.vars="date",variable.name = "cell",value.name="value_safran",variable.factor=F)
  db <- merge(db_fyre,db_safran,by=c("date","cell"))
  db$year <- as.numeric(str_sub(db$date,1,4))
  
  # compute bias
  db_bias <- db[year %in% c(1990:2012),.(bias=mean(value_fyre-value_safran)),by="cell"]   # for each stand, compute the bias between 01/1990 and 12/2012 (not before because bias is not stationnary before + in S2M reanalysis, change in 1990 due to change in the station number) annd correct for that

  # compute debiased value
  db_safran_corrected <- merge(db_safran,db_bias,by="cell")
  db_safran_corrected[,value_safran_corrected:=value_safran+bias]
  db_safran_corrected[,":="(value_safran = NULL, bias = NULL)]
  setnames(db_safran_corrected,"value_safran_corrected","value_safran")

  # put in the wide format
  db_safran_corrected <- dcast(db_safran_corrected, date~cell,value.var="value_safran")
  
  return(db_safran_corrected)
  
}
