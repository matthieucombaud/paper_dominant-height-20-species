select_pure_stand<-function(db,stand.preselected){
  
  # Several definitions of mixed forest can be used.
  # I use the IFN definition (based on TCL), and check that it is consistent with a definition based on BA or RDI.
  # Cls: when TCL is high, other "space" variables are high but sometimes another species is selected.
  # The latter concerns 900 stands over 21000, with often the same species poping out for all the variables expect TCL.
  # At this stage I do not consider these stands. After that, we are able to define the dominant species for the stand
  # (nevermind the variable under consideration among TCL, G, RDI and the strata under consideration)
  
  # identify stands for which both age measures were done on the same species and this species correspond to the species dominating TCL
  stand.crit1<-db[(is.na(Sp2.AGE13.HTOT)|Sp1.AGE13.HTOT==Sp2.AGE13.HTOT)&Sp1.AGE13.HTOT==Sp1.TCL,stand]
  
  # identify stands for which relative TCL of species 1 is above 0.75 (reminder : TCL1>TCL2 by construction)

  stand.crit2<-db[!is.na(Value1.TCL)&(Value1.TCL/TCL.tot>=0.75),stand] # this exclude YEAR==2005 since no TCL for this YEAR (but already exclude by the AGE13!=NA criteria)
  # 75% is the definition in the inventory
  
  # identify IDP where species preponderance order do not depend on the variable: TCL does not give the same order as other variables for 1002 stands
  stand.crit3<-db[Sp1.TCL==Sp1.Gtot&Sp1.TCL==Sp1.Gupper&Sp1.TCL==Sp1.rdiTot&Sp1.TCL==Sp1.rdiUpper,stand] # ambiguity for 862 stands

  return(db[stand%in%stand.crit1&stand%in%stand.crit2&stand%in%stand.crit3&stand%in%stand.preselected,stand])
  
}
