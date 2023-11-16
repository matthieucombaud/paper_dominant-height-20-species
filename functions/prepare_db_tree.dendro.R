prepare_db_tree.dendro<-function(db.tree,coefficientsRDI){
  
  # exclude dead trees
  #########################
  
  db.tree<-db.tree[VEGET%in%c(0)]
  
  
  # exclude stands with W==An or W==0
  ########################################
  
    # ESPAR variable
    db.tree[,table(is.na(ESPAR))] # no tree without ESPAR
    
    # W variable
    # detect W=NA or W=0
    db.tree[is.na(W),table(YEAR)] # 26 trees with W=NA
    stands.WNA<-unique(db.tree[is.na(W),IDP])
    length(stands.WNA) # this represent 7 stands
    db.tree[W==0,table(YEAR)] # 1398 trees with W=0
    stands.W0<-unique(db.tree[W==0,IDP])
    length(stands.W0) # this represents 447 stands

    # Explanation (IFN, personnal communication by Nathalie Derrière, 13/01/2022 & 17/01/2022)
    # W is a computed variable, and is not computed for trees affected by Klaus tempest in 2009. It explains the 48 W==NA. But why only trees from the campaign 2006 and 2009 are affected ? because method used for the points visited + hit by the tempest in 2009, and some rare points in 2006 where height was not available+  why trees of the campaign 2005-2007 have W=0? Because the whole tree is now considered as hit by the tempest (More generally, W may change between 2019 and 2020 release because of photointerpretation of damages)
    # big value for W are due to measure before 2009, when simplified trees where not included in the db, and the weight of non-simplified trees was increased to compensate for that
    
    # Because these NA and 0 can bias analysis at the stand level, we remove all the trees of all the stands concerned in the analysis (this may induce some bias, since for example we exclude some stands heavily hit by the Klaus tempest in 2009)
    
    exclude.stands.NA0<-unique(db.tree[is.na(W)|(!is.na(W)&W==0),.(IDP,YEAR)]) # we exclude 454 stands
    exclude.stands.NA0[,table(YEAR)] # those are mostly stands in 2005-2007, and all stands of these years will be excluded latter based on other criteria. Only 10 other stands are removed (YEAR==2009)
    
    db.tree<-db.tree[!IDP%in%exclude.stands.NA0$IDP]
  
  # add SIMPLIF variable
  ##########################

    #   Reminder regarding simplified trees:
    # - IR5 and HTOT are measured only for non-simplified trees
    # - 2005-2008 : 6 trees measured by stand x species x class size, other trees not included in the DB, but weight of the 6 trees ajusted upward to take into account non-included trees presence
    # - 2009-2013 : 6 trees measured by stand x species x class size, other trees included in the DB, weight of the 6 trees and the other trees is exactly 1*10000m2/area of the relevant stand
    # - 2014-2020 : 1 tree measured by stand x species x class size, other trees included in the DB, weight of the 1 tree and the other trees is exactly 1*10000m2/area of the relevant stand
    # dimension categories are given by IFN. We do not consider the "très petit bois" class, created in 2016
    
    
    # SIMPLIF variable
    db.tree[YEAR<=2008,SIMPLIF:=0] # no simplified tree before 2008
    db.tree[is.na(SIMPLIF)] # one single tree, apparently simplified
    db.tree<-db.tree[IDP==1324935&A==6,SIMPLIF:=1] # we consider this tree to obe a simplified tree (else we need to exclude the whole stand)
    
    
  # Create size categories
  ###########################
    
    #divide trees into circumference classes (used by IFN)
    db.tree[, DIMESS := cut(C13, breaks = c(0, 0.705, 1.175, 1.645, 10), labels = c("PB", "BM", "GB", "TGB"), right = FALSE)] #circumference class, in meters
    db.tree[YEAR>=2016 & C13<0.395, DIMESS := "TPB"]
    
  # imputation for "simplified trees" of the missing value for height , based on circumference class, species and stand
  ###################################
    
    db.tree[, HTOT := ifelse(SIMPLIF==1,mean(HTOT, na.rm = TRUE), HTOT),by = c("IDP", "ESPAR", "DIMESS")] #takes the mean only on living trees, and replace values only for living trees # mean is required for YEAR in 2009-2013, when 6 trees were measured by IDP*ESPAR*DIMESS and the other were included in the DB
    db.tree[, HTOT := ifelse(SIMPLIF==1,mean(HTOT, na.rm = TRUE), HTOT),by = c("IDP", "ESPAR", "DIMESS")] #takes the mean only on living trees, and replace values only for living trees # mean is required for YEAR in 2009-2013, when 6 trees were measured by IDP*ESPAR*DIMESS and the other were included in the DB
    
    #check no weird NA for HTOT 
    db.tree[is.na(HTOT)] ## 5 NA: for IDP==1100052, not clear why A=1 is simplified. for the other probably pb between PB and TPB
    IDP.HTOT.NA<-db.tree[is.na(HTOT),IDP]
    db.tree[IDP%in%IDP.HTOT.NA]
    # We delete these stands
    db.tree<-db.tree[!IDP%in%IDP.HTOT.NA]
    
    # lot of NA for IR5 because IR5=NA for some non-simplified trees
    db.tree[is.na(IR5),.N]
  
  # define upper and lower strata
    db.tree[,STRATA:=ifelse(HTOT>0.5*max(HTOT), "upper", "lower"),by="IDP"] 

  
  # tree basal area
    db.tree[,g:=pi*(C13/(2*pi))^2] # g is tree basal area # C13 and IR5 and G are in meter, only Dg is in cm (to be consistent with the coeff used for RDI computation)
    db.tree[,table(is.na(g))] #all living trees have a g
    
  # compute dendrometric variables
  ####################################

    #all species together
    # Basal area (G in m2)
    db.dendro<-db.tree[, 
                       .(
                         Gtot=sum(g*W), #basal area in all strata
                         Ntot=sum(W), #apply both to 1st and 2nd visits
                         Gupper=sum(g[STRATA=="upper"]*W[STRATA=="upper"]),#basal area in the higher strata
                         Nupper=sum(W[STRATA=="upper"])
                       ), #nb of trees per ha
                       by = c("IDP","YEAR")] # YEAR is not necessary if we work only with first visit, but I keep it in case second visit is added
    
    # check: no stand with weird values for G or N
    db.dendro[is.na(Gtot),.N]
    db.dendro[is.na(Ntot),.N]
    db.dendro[is.na(Gupper),.N]
    db.dendro[is.na(Nupper),.N]
    db.dendro[Ntot==0,.N]
    db.dendro[Gtot==0,.N]
    db.dendro[Nupper==0,.N]
    db.dendro[Gupper==0,.N]
    
    #mean quadratic diameter #attention : Dg in cm - keep cm to compare to the value per species below
    db.dendro[,c("DgTot","DgUpper"):=list(sqrt(4*Gtot/(pi*Ntot))*100,sqrt(4*Gupper/(pi*Nupper))*100)]
    
    #per species
    # Basal area (G in m2)
    db.dendro.sp<-db.tree[,
                          .(
                            Gtot=sum(g*W), #basal area in all strata
                            Ntot=sum(W), #apply both to 1st and 2nd visits
                            Gupper=sum(g[STRATA=="upper"]*W[STRATA=="upper"]),#basal area in the higher strata
                            Nupper=sum(W[STRATA=="upper"])
                          ), #nb of trees per ha
                          by = c("IDP","YEAR","ESPAR")]
    
    #mean quadratic diameter #attention : Dg in cm - keep cm to compare to the value per species below
    db.dendro.sp[,c("DgTot","DgUpper"):=list(sqrt(4*Gtot/(pi*Ntot))*100,sqrt(4*Gupper/(pi*Nupper))*100)]
    

  # compute RDI
  ##############
    
    # RDI coefficients are from Toigo et al 2017 - 2018)
    
    # default values (for species for wich we do not have info)
    default.rqIntercept.decid<-coefficientsRDI[substr(espece,1,2)<50,mean(rqIntercept)]
    default.rqSlope.decid<-coefficientsRDI[substr(espece,1,2)<50,mean(rqSlope)]
    default.rqIntercept.conif<-coefficientsRDI[substr(espece,1,2)>=50,mean(rqIntercept)]
    default.rqSlope.conif<-coefficientsRDI[substr(espece,1,2)>=50,mean(rqSlope)]
    
    # upper strata
    ##########################
    
    # merge dendro per species with RDI coefficient
    RDI.upper.sp <- merge(
      db.dendro.sp[!is.na(Nupper)&Nupper>0,], # !is.na(Nupper)&Nupper>0 to be sure we keep only relevant line (enable to avoid na.rm=T when summing, which helps identify problems)
      coefficientsRDI,
      by.x = "ESPAR", by.y = "espece", all.x=T
    ) 
    
    # deal with missing coeff
    {
      # deal with NA for deciduous species (species index<50 <=> deciduous)
      RDI.upper.sp[,rqIntercept:=ifelse(is.na(rqIntercept)&substr(ESPAR,1,2)<50,default.rqIntercept.decid,rqIntercept)]
      RDI.upper.sp[,rqSlope:=ifelse(is.na(rqSlope)&substr(ESPAR,1,2)<50,default.rqSlope.decid,rqSlope)]
      # deal with NA for coniferous species (species index>=50 <=> coniferous)
      RDI.upper.sp[,rqIntercept:=ifelse(is.na(rqIntercept)&substr(ESPAR,1,2)>=50,default.rqIntercept.conif,rqIntercept)]
      RDI.upper.sp[,rqSlope:=ifelse(is.na(rqSlope)&substr(ESPAR,1,2)>=50,default.rqSlope.conif,rqSlope)]
    }
    
    #compute RDI per species
    RDI.upper.sp[, rdiUpper :=Nupper/exp(rqIntercept  + rqSlope *log(DgUpper))]
    
    #merge with the dendro db (by summing RDI at the species level, which is an approximation)
    db.dendro <- merge(db.dendro, RDI.upper.sp[, .(rdiUpper = sum(rdiUpper)), by=c("IDP","YEAR")],by=c("IDP","YEAR"),all.x = T) #no na.rm=T in sum(rdiUpper) since all species should have coeff #all.x=T in case we have stands with no RDI upper (should not appear, but helps debugging)
    
    #check where we have rdiUpper==NA: OK
    db.dendro[is.na(rdiUpper),.N]
    
    # total vegetation
    ##########################
    
    # merge dendro per species with RDI coefficient
    RDI.tot.sp <- merge(
      db.dendro.sp[!is.na(Ntot)&Ntot>0,], # !is.na(Ntot)&Ntot>0 to be sure we keep only relevant line (enable to avoid na.rm=T when summing, which helps identify problems)
      coefficientsRDI,
      by.x = "ESPAR", by.y = "espece", all.x=T
    ) 
    
    # deal with missing coeff
    {
      # deal with NA for deciduous species (species index<50 <=> deciduous)
      RDI.tot.sp[,rqIntercept:=ifelse(is.na(rqIntercept)&substr(ESPAR,1,2)<50,default.rqIntercept.decid,rqIntercept)]
      RDI.tot.sp[,rqSlope:=ifelse(is.na(rqSlope)&substr(ESPAR,1,2)<50,default.rqSlope.decid,rqSlope)]
      # deal with NA for coniferous species (species index>=50 <=> coniferous)
      RDI.tot.sp[,rqIntercept:=ifelse(is.na(rqIntercept)&substr(ESPAR,1,2)>=50,default.rqIntercept.conif,rqIntercept)]
      RDI.tot.sp[,rqSlope:=ifelse(is.na(rqSlope)&substr(ESPAR,1,2)>=50,default.rqSlope.conif,rqSlope)]
    }
    
    #compute RDI per species
    RDI.tot.sp[, rdiTot :=Ntot/exp(rqIntercept  + rqSlope *log(DgTot))]
    
    #merge with the dendro db (by summing RDI as the species level, which is an approximation)
    db.dendro <- merge(db.dendro, RDI.tot.sp[, .(rdiTot = sum(rdiTot)), by=c("IDP","YEAR")],by=c("IDP","YEAR"),all.x = T) #no na.rm=T in sum(rditot) since all species should have coeff and variables!=NA here #all.x=T in case we have stands with no RDI tot (should not appear, but helps debugging)
    
    #check where we have rditot==NA: OK
    db.dendro[is.na(rdiTot),.N]
    
    
    #Species dominating BA and BA in the upper strata
    
      #Here I identify the 3 species with the highest Gtot (or highest Gupper).

      temp.Gtot<-db.dendro.sp[,.(IDP,YEAR,ESPAR,Gtot)][order(IDP,YEAR,-Gtot)][,head(.SD,3),by=c("IDP","YEAR")]
      dominanceGtot <- temp.Gtot[, .(
        Sp1.Gtot=ifelse(!is.na(Gtot[1]),ESPAR[1],""),
        Value1.Gtot = Gtot[1],
        Sp2.Gtot=ifelse(!is.na(Gtot[2]),ESPAR[2],""),
        Value2.Gtot = Gtot[2],
        Sp3.Gtot=ifelse(!is.na(Gtot[3]),ESPAR[3],""),
        Value3.Gtot = Gtot[3]
      ), by = c("IDP","YEAR")] #check !is.na(Gtot[1/2]) to avoid putting a species name when we are not able to determine the species'G (because of no measure of C13, even we we know the species is present)
      #attention: Sp1.Gtot and Sp2.Gtot are equal to "" if no species identified (lack of C13)
      
      temp.Gupper<-db.dendro.sp[,.(IDP,YEAR,ESPAR,Gupper)][order(IDP,YEAR,-Gupper)][,head(.SD,2),by=c("IDP","YEAR")]
      dominanceGupper <- temp.Gupper[, .(
        Sp1.Gupper=ifelse(!is.na(Gupper[1]),ESPAR[1],""),
        Value1.Gupper = Gupper[1],
        Sp2.Gupper=ifelse(!is.na(Gupper[2]),ESPAR[2],""),
        Value2.Gupper = Gupper[2],
        Sp3.Gupper=ifelse(!is.na(Gupper[3]),ESPAR[3],""),
        Value3.Gupper = Gupper[3]
      ), by = c("IDP","YEAR")] #check !is.na(Gupper[1/2]) to avoid putting a species name when we are not able to determine the species'G (because of no measure of C13, even we we know the species is present)
      
      #merge
      db.dendro<-Reduce(function(x, y) merge(x=x, y=y, by=c("IDP","YEAR"), all.x=T, all.y=T), list(db.dendro,dominanceGtot,dominanceGupper))
      
      
    # Species dominating RDI
      
      # I identify the 3 species with the highest rdiUpper or rdiTot.
      
      # upper strata
      ################
      
      temp.rdiUpper<-RDI.upper.sp[,.(IDP,YEAR,ESPAR,rdiUpper)][order(IDP,YEAR,-rdiUpper)][,head(.SD,3),by=c("IDP","YEAR")]
      dominationrdiUpper <- temp.rdiUpper[, .(
        Sp1.rdiUpper=ifelse(!is.na(rdiUpper[1]),ESPAR[1],""),
        Value1.rdiUpper = rdiUpper[1],
        Sp2.rdiUpper=ifelse(!is.na(rdiUpper[2]),ESPAR[2],""),
        Value2.rdiUpper = rdiUpper[2],
        Sp3.rdiUpper=ifelse(!is.na(rdiUpper[3]),ESPAR[3],""),
        Value3.rdiUpper = rdiUpper[3]
      ), by = c("IDP","YEAR")] #check !is.na(rdiUpper[1/2]) to avoid putting a species name when we are not able to determine the species'G (because of no measure of C13, even we we know the species is present)
      #attention: Sp1.Gtot and Sp2.Gtot are equal to "" if no species identified (lack of C13)
      
      #merge
      db.dendro<-merge(x=db.dendro, y=dominationrdiUpper, by=c("IDP","YEAR"), all.x=T, all.y=T)
      
      
      # all vegetation
      ################
      
      temp.rdiTot<-RDI.tot.sp[,.(IDP,YEAR,ESPAR,rdiTot)][order(IDP,YEAR,-rdiTot)][,head(.SD,3),by=c("IDP","YEAR")]
      dominationrdiTot <- temp.rdiTot[, .(
        Sp1.rdiTot=ifelse(!is.na(rdiTot[1]),ESPAR[1],""),
        Value1.rdiTot = rdiTot[1],
        Sp2.rdiTot=ifelse(!is.na(rdiTot[2]),ESPAR[2],""),
        Value2.rdiTot = rdiTot[2],
        Sp3.rdiTot=ifelse(!is.na(rdiTot[3]),ESPAR[3],""),
        Value3.rdiTot = rdiTot[3]
      ), by = c("IDP","YEAR")] #check !is.na(rdiTot[1/2]) to avoid putting a species name when we are not able to determine the species'G (because of no measure of C13, even we we know the species is present)
      #attention: Sp1.Gtot and Sp2.Gtot are equal to "" if no species identified (lack of C13)
      
      #merge
      db.dendro<-merge(x=db.dendro, y=dominationrdiTot, by=c("IDP","YEAR"), all.x=T, all.y=T)
      
      
    # return
      
      return(list(db.tree=db.tree,db.dendro=db.dendro))
      
      

}