prepare_db_cover<-function(db.cover){
  
  setnames(db.cover,"ESPAR_C","ESPAR")
  
  db.cover<-db.cover[STRATE=="R"] # focus on recensable strata
  
  # preliminary checks: TCL and TCA are never NA
  db.cover[,table(is.na(TCL))]
  db.cover[,table(is.na(TCA))]
  
  #sum of cover per species
  db.cover.aggregated<-db.cover[,.(TCL.tot=sum(TCL),TCA.tot=sum(TCA)),by=c("IDP","YEAR")]
  
  #identification of dominant species
  temp.TCL<-db.cover[,.(IDP,YEAR,ESPAR,TCL)][order(IDP,YEAR,-TCL)][,head(.SD,3),by=c("IDP","YEAR")]
  dominationTCL <- temp.TCL[, .(
    Sp1.TCL=ifelse(!is.na(TCL[1]),ESPAR[1],""),
    Value1.TCL = TCL[1],
    Sp2.TCL=ifelse(!is.na(TCL[2]),ESPAR[2],""),
    Value2.TCL = TCL[2],
    Sp3.TCL=ifelse(!is.na(TCL[3]),ESPAR[3],""),
    Value3.TCL = TCL[3]
  ), by = c("IDP","YEAR")]
  
  
  temp.TCA<-db.cover[,.(IDP,YEAR,ESPAR,TCA)][order(IDP,YEAR,-TCA)][,head(.SD,3),by=c("IDP","YEAR")]
  dominationTCA <- temp.TCA[, .(
    Sp1.TCA=ifelse(!is.na(TCA[1]),ESPAR[1],""),
    Value1.TCA = TCA[1],
    Sp2.TCA=ifelse(!is.na(TCA[2]),ESPAR[2],""),
    Value2.TCA = TCA[2],
    Sp3.TCA=ifelse(!is.na(TCA[3]),ESPAR[3],""),
    Value3.TCA = TCA[3]
  ), by = c("IDP","YEAR")]
  
  #merge
  db.cover.aggregated<-Reduce(function(x, y) merge(x=x, y=y, by=c("IDP","YEAR"), all.x=T, all.y=T), list(db.cover.aggregated,dominationTCL,dominationTCA))
  
  return(db.cover.aggregated)
  
}
