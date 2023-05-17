prepare_db_tree_height_age<-function(db.tree){
  
  db.tree<-db.tree[!is.na(AGE13)&!is.na(HTOT)&(is.na(ACCI)|!is.na(ACCI)&ACCI==0),.(IDP,ESPAR,AGE13,HTOT)] # since AGE13 is measured, we have only living trees
    
  db.tree.wide<-db.tree[, .(
    Sp1.AGE13.HTOT=ESPAR[1],
    Sp2.AGE13.HTOT=ESPAR[2],
    Value1.AGE13=AGE13[1],
    Value2.AGE13=AGE13[2],
    Value1.HTOT=HTOT[1],
    Value2.HTOT=HTOT[2]
  ), by = c("IDP")]

  return(db.tree.wide)
  
}
