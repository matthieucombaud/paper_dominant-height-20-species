rename_ifn<-function(db){
  
  # rename
  db_names<-rbind(
    
    data.table(old="IDP",new="stand"),
    data.table(old="XL",new="x"),
    data.table(old="YL",new="y"),
    data.table(old="PROF2",new="soil_depth"),
    data.table(old="PENT2",new="slope"),
    data.table(old="pH_2018_pHCN_decorreles",new="pH"),
    data.table(old="ST_2018_pHCN_decorreles",new="ST"),
    data.table(old="CN_2018_pHCN_decorreles",new="CN"),
    data.table(old="P2O5log_2018",new="P2O5"),
    data.table(old="Engorg_perm_2006",new="waterlogging_permanent"),
    data.table(old="Engorg_temp_2006",new="waterlogging_temporary")
    
  )
  
  setnames(db,db_names$old,db_names$new)
  
  # reformat (to remove the factor and the integer format, in order to be able to compute mean and SD)
  db[,ROCHE.Calc:=as.numeric(ROCHE.Calc)]
  db[,soil_depth:=as.numeric(soil_depth)]
  db[,affroc_perc :=as.numeric(affroc_perc)]
  db[,cailloux_perc  :=as.numeric(cailloux_perc )]
  db[,slope :=as.numeric(slope)]
  
  return(db)
  
}