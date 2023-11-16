get_parameter_table <- function(
  species,
  dir.model
  ){
  
  # import model
  model_no_batch <- readRDS(paste0(dir.model,"/sp",species,"_no_batch.rds"))
  
  # get parameters
  db_parameter_model_main <- model_no_batch$batch_1$db_parameter_normalized
  db_parameter_model_main$param_class <- factor(db_parameter_model_main$param_class,levels=c("intercept","alpha","gamma","beta","sigma","delta"))
  
  # add significance
  db_parameter_model_main[,pvalue_class:=cut(pvalue,breaks=c(0,0.001,0.01,0.05,1))]
  db_significance <- data.table(pvalue_class=levels(db_parameter_model_main$pvalue_class),star=c("***","**","*","ns"))
  db_parameter_model_main <- merge(db_parameter_model_main,db_significance,by="pvalue_class",all.x=T,sort=F) # all=T to deal with is.na(pvalue)
  db_parameter_model_main[,value:=paste0(round(estimate,digits=2)," ",star)]
  
  # order the db
  setkey(db_parameter_model_main,param_class)
  
  db_parameter_model_main[, species_code := species]
  
  return(db_parameter_model_main)
  
}