prepare_db_flora<-function(db.flora,taxref,VI){
  
  # we use taxref for finding the match CD_REF / LB_NOM because the IFN data are not complete
  
  # simplify taxref
  taxref<-taxref[,.(CD_REF,RANG,LB_NOM,NOM_VALIDE)]
  
  # Prepare species list observed in db.flora, adding taxref name, identify trees
  species.list<-unique(db.flora[,.(CD_REF)])
  species.list<-merge(species.list[,CD_REF:=as.numeric(CD_REF)],taxref,by="CD_REF",all.x = T) # attention: merge creates several rows per CD_REF because subspecies & different ranks in taxref. We deal with the different rows later, at this stage we keep it to increase the change of merge over the LB_NOM variable
  species.list[is.na(LB_NOM),.N] #no NA
  
  # create variable useful for merging
  species.list$ID<-numeric()
  species.list$merged<-F # to say if a merge occured for this CD_REF
  species.list$check.name.JCG<-""
  
  # prepare JCG database
  setnames(VI,old=c("Noms actualisés"),new=c("updated.names"))
  VI<-VI[is.na(Essences)] # we keep only the rows for non-tree species
  VI[,ID:=seq(1:nrow(VI))] # we add a unique ID (legitimate because no redundancy for updated.names, cf infra)
  
  # check pb
  VI[is.na(updated.names)|is.na(Noms_TAXREFv60),.(updated.names,Noms_TAXREFv60)] # some NA for taxref, not for updated names
  VI[str_detect(updated.names,"\\(")|str_detect(updated.names,"\\)")|str_detect(Noms_TAXREFv60,"\\(")|str_detect(Noms_TAXREFv60,"\\)"),.(updated.names,Noms_TAXREFv60)] # unmatched parenthesis cause problems for detection with "grep, we correct
  VI[Noms_TAXREFv60 == "Care cuprina (Sandor ex",Noms_TAXREFv60 := "Carex cuprina"] # I also correct the "x" in carex
  
  # Other correction (missing letters)
  VI[Noms_TAXREFv60 == "Bromu erectus Huds. subsp.",Noms_TAXREFv60 := "Bromus erectus Huds. subsp."]
  VI[Noms_TAXREFv60 == "Care flacca Schreb. subsp.",Noms_TAXREFv60 := "Carex flacca Schreb. subsp."]
  VI[Noms_TAXREFv60 == "Plantag atrata Hoppe subsp.",Noms_TAXREFv60 := "Plantago atrata Hoppe subsp."]
  VI[Noms_TAXREFv60 == "Primul veris L. subsp.",Noms_TAXREFv60 := "Primula veris L. subsp."]
  VI[Noms_TAXREFv60 == "Rume obtusifolius L. subsp.",Noms_TAXREFv60 := "Rumex obtusifolius L. subsp."]
  VI[Noms_TAXREFv60 == "Saxifrag fragilis Schrank subsp.",Noms_TAXREFv60 := "Saxifraga fragilis Schrank subsp."]  
  VI[Noms_TAXREFv60 == "Solidag virgaurea L. subsp.",Noms_TAXREFv60 := "Solidag virgaurea L. subsp."] 
  
  #check redundancies
  dim(VI)[1]-length(unique(VI$updated.names)) # no redundancies
  dim(VI)[1]-length(unique(VI$Noms_TAXREFv60)) # some redundancies. We deal with them later, when we go in details into the merge over the Noms_TAXREFv60 variable. Basically, we do not take into account the corresponding lines in the merge
  VI[,occurence:=.N,by=Noms_TAXREFv60]
  VI[,table(occurence)]
  length(unique(VI[occurence>1,Noms_TAXREFv60])) # 13 Noms_TAXREFv60 appears several times
  
  
  # for merging, we look for the names (LB_NOM) in species.list we include the name used in JCG database appears, and we attribute the corresponding ID (of VI). We begin with the longest names in JCG database to attribute the ID at the most precise level of JCG names. We do that for both names used in JCG: updated.names and taxref6.0
  
  ## application on "updated.names"
  #################################################
  
  setkey(VI,"updated.names")
  
  species.list_v1<-copy(species.list)
  
  VI_split_v1 <-as.data.table(cbind(VI[,.(ID,updated.names)],str_split(VI$updated.names," ",simplify=T)))
  colnames(VI_split_v1) <- c("ID","names","generic1", "generic2", "generic3", "generic4")  
  # VI_split_v1[,ID:=as.numeric(ID)]
  
  VI_split_v1$nb.descriptors<-NA
  for(i in 1:dim(VI_split_v1)[1]){
    VI_split_v1$nb.descriptors[i] <- as.numeric((VI_split_v1$generic1[i]!="")) +as.numeric((VI_split_v1$generic2[i]!=""))+as.numeric((VI_split_v1$generic3[i]!="")) + as.numeric((VI_split_v1$generic4[i]!=""))
  }
  
  # detect pb (if end of name is "subsp.")
  VI_split_v1[nb.descriptors==4&generic4=="subsp."] # these species do not have complete names, but no pb, stil useful at the species level
  
  # do the merge
  for(nb in 4:2){
    
    for (id in VI_split_v1[nb.descriptors == nb,ID]) {
      sp <- VI_split_v1[ID == id,names]
      if(!is.na(sp)){
        species.list_v1[merged==F&grepl(sp, LB_NOM, ignore.case=T),c("ID","merged","check.name.JCG"):=list(id,T,sp)]
      }
    }
    
  }
  
  # check number of match
  success<-length(unique(species.list_v1[merged==T,CD_REF]))
  total<-length(unique(species.list_v1[,CD_REF]))
  print(paste0(success," matches over ",total," species in the flora database"))
  
  # attention: some CD_REF are associated to several ID (because of different LB_NOM): need to average (done below)
  result1<-unique(species.list_v1[merged==T,.(CD_REF,ID)])
  result1[,occurence:=.N,by=CD_REF]
  result1[occurence>1] # some CD_REF with more than 1 correspondance in JCG
  
  
  ## application on "Noms_TAXREFv60"
  ####################################################
  
  setkey(VI,"Noms_TAXREFv60")
  
  species.list_v2<-copy(species.list)
  
  VI_split_v2 <-as.data.table(cbind(VI[occurence==1,.(ID,Noms_TAXREFv60)],str_split(VI[occurence==1,Noms_TAXREFv60]," ",simplify=T))) #we filter on occurence==1 to avoid VI_split_v2 pb. This may lead to the loss of some species, but it avoids merging info concerning different species
  colnames(VI_split_v2) <- c("ID","names","generic1", "generic2", "generic3", "generic4")  
  # VI_split_v2[,ID:=as.numeric(ID)]
  
  VI_split_v2$nb.descriptors<-NA
  for(i in 1:dim(VI_split_v2)[1]){
    VI_split_v2$nb.descriptors[i] <- as.numeric((VI_split_v2$generic1[i]!="")) +as.numeric((VI_split_v2$generic2[i]!=""))+as.numeric((VI_split_v2$generic3[i]!="")) + as.numeric((VI_split_v2$generic4[i]!=""))
  }
  
  # detect pb
  VI_split_v2[nb.descriptors==4&generic4=="subsp."] # these species do not have complete names and therefore cannot be used (they are in the code but cannot be matched) # 7 problems...
    # pb : wring names...
  
  # do the merge  
  for(nb in 4:2){
    
    for (id in VI_split_v2[nb.descriptors == nb,ID]) {
      sp <- VI_split_v2[ID == id,names]
      
      if(!is.na(sp)){
        species.list_v2[merged==F&grepl(sp, LB_NOM, ignore.case=T),c("ID","merged","check.name.JCG"):=list(id,T,sp)]
      }
    }
    
  }  
  
  # check number of match
  success<-length(unique(species.list_v2[merged==T,CD_REF]))
  total<-length(unique(species.list_v2[,CD_REF]))
  print(paste0(success," matches over ",total," species in the flora database"))
  
  # attention: a CD_REF is associated to several ID
  result2<-unique(species.list_v2[merged==T,.(CD_REF,ID)])
  result2[,occurence:=.N,by=CD_REF]
  result2[occurence>1] # some CD_REF with more than 1 correspondance in JCG
  
  # combine results of the two kinds of merge (over updated.names and taxref60)
  ###########################################################
  
  results<-rbind(result1[,.(CD_REF,ID)],result2[,.(CD_REF,ID)])
  setkey(results,CD_REF)
  results<-unique(results) # because some merges are the same in results1 and results2
  results[,occurence.CD_REF:=.N,by=CD_REF]
  results[occurence.CD_REF>1] # some CD_REF with more than 1 correspondance in JCG
  # we also have several CD_REF for the same ID... (eg taxref13[CD_REF==80798] & taxref13[CD_REF==80564]). But not a problem, it will simply lead to CD_REF withthe same indicator values
  
  # check number of match in total
  success<-length(unique(results[,CD_REF]))
  total.IFN<-length(unique(species.list[,CD_REF]))
  total.JCG<-length(unique(VI[,updated.names]))
  print(paste0(success," matches over ",total.IFN," species in the flora database"))
  print(paste0(success," matches over ",total.JCG," species in JCG database"))
  
  # merge species names with indicator values
  results<-merge(results,VI,by="ID",all.x=T)
  setnames(results,old=c("L Ellenberg","T Ellenberg","K Ellenberg","F Ellenberg","R Ellenberg", "N Ellenberg"),new=c("L_Ellenberg","T_Ellenberg","K_Ellenberg","F Ellenberg","R_Ellenberg", "N_Ellenberg"))
  
  #variables of interest
  variables<-c("pH_sans_Essences_2006","pH_2018","pH_2018_pHCN_decorreles","ST_2018","ST_2018_pHCN_decorreles","CN_2018","CN_2018_pHCN_decorreles","P2O5log_2018","Engorg_temp_2006","Engorg_perm_2006","Tmin01","ETP03-05","BH07","TmoyA","VI_Foret","L_Ellenberg","T_Ellenberg","K_Ellenberg","F Ellenberg","R_Ellenberg", "N_Ellenberg")
  
  # take the mean of indicator values per CD_REF
  results.mean<-results[,lapply(.SD,function(x) mean(x,na.rm=T)),by=CD_REF,.SDcols=variables]
  
  # add species names
  results.mean<-merge(unique(species.list[,.(CD_REF,NOM_VALIDE)]),results.mean,by="CD_REF")
  

  ## compute indicated variable per stand
  
  # Attention:
  # - computation is done without weighting by species abundance
  # - depending on releve date, some species may be absent at the time of the releve but present at another moment. Such species are not taken into account in the indicator values
  

  db.flora<-merge(db.flora,results.mean,by="CD_REF",all=F)
  setkey(db.flora,IDP)
  
  db.bioindicators<-db.flora[,lapply(.SD,function(x) mean(x,na.rm=T)),by=c("IDP","YEAR"),.SDcols=variables] # no weight depending on abundance (JCG)
  
  # check NA
  summary(db.bioindicators)
  
  return(db.bioindicators)
  
}
