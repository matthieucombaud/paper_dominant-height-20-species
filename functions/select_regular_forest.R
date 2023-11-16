select_regular_forest<-function(db,stand.preselected){
  
  # identify stand with "futaie regulière" based on IFN classification
  
    # For years 2005-2006 : futaie régulière =1
    # For years 2007-2013 : futaie régulière =1
    # For years 2014-2020 : futaie régulière=2,5,6
    
  stand.crit1<-db[!is.na(SFO)&SFO==1 | !is.na(SVER)&SVER%in%c(2,5,6),stand]
    
    
  # impose age difference inferior to 25% of the lowest height, to ensure that trees have indeed grown together (arbitrary criteria, as in Vallet & Pérot 2016). But do not impose age.gap=0 ("strict" even-aged fores)
    db[,age.gap:=ifelse(is.na(Value2.AGE13),0,abs(Value1.AGE13-Value2.AGE13)/min(Value1.AGE13,Value2.AGE13)),by=stand]
    stand.crit2<-db[age.gap<0.25,stand]
    
  return(db[stand%in%stand.crit1&stand%in%stand.crit2&stand%in%stand.preselected,stand])
    
}
