prepare_db_stand<-function(db.stand){
  
  #The variable PLISI has been recoded in 2008: 1,2,3 in 2007 means 0,1,2 in 2008-2020. Here I recode for 2007

  db.stand[,table(PLISI,YEAR)]
  if(3%in%unique(db.stand$PLISI)){db.stand[YEAR==2007&VISIT_INDEX=="first",PLISI:=PLISI-1]} # the if condition is here to avoid running twice this line
  
  # create a new binary variable edge_presence
  
  db.stand[,edge_presence:=ifelse(!is.na(PLISI),ifelse(PLISI>0,1,0),NA)]
  
  return(db.stand)
  
}
