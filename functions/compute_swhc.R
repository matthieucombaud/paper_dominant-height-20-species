compute_swhc <- function(db.soil) {

  # encode variables thanks to IFN correspondance table (cf function "get_metadata")
  
    db_prof_code <- data.table(code=c(0,1,2,3,4,5,6,7,8,9),value=c(2.5, 10, 20, 30, 40, 50, 60, 70, 80, 90)) # IFN code is in 0:9 (depth in dm) Attention : if IFN code =9, means ">85 cm".... Here we assimilate ">85 cm" to "=90 cm"
    db_affroc_code <- data.table(code=c(0,1,2,3,4,5,6,7,8,9,10),value=c(2.5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 97.5)) # IFN code is in 0:10 (proportion in 1/10 of the surface) 
    db_cailloux_code <- data.table(code=c(0,1,2,3,4,5,6,7,8,9,10),value=c(2.5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 97.5)) # IFN code is in 0:10 (proportion in 1/10 of the surface) 
    
    # water content per unit of depth, depending on the texture (unit=water mm/ soil cm) (coeff "teneur" are from the paper by Piedallu et al, 2018, and correspond to a specific texture)
    db_teneur_A_code <- data.table(code=as.character(c(0,1,2,3,4,5,6,7,8,9)),value=c(0, 1.07, 1.28, 1.28, 1.50, 1.6, 1.69, 1.36, 1.35, 1.38)) # IFN code is in 1:9 ; "0" is added at the beginning to complete when text1 = 0 meaning no layer 
    db_teneur_EBC_code <- data.table(code=as.character(c(0,1,2,3,4,5,6,7,8,9)),value=c(0, 0.88, 1.09, 1.09, 1.31, 1.36, 1.40, 1.19, 1.20, 0.95)) # IFN code is in 1:9 ; "0" is added at the beginning to complete when text2 = 0 meaning no layer    
    
 # add depth and rock values to the db
    
    db.soil <- merge(db.soil,db_prof_code,by.x="PROF1",by.y="code",all.x=T)
    setnames(db.soil,"value","prof1_cm")
    db.soil <- merge(db.soil,db_prof_code,by.x="PROF2",by.y="code",all.x=T)
    setnames(db.soil,"value","prof2_cm")
    db.soil <- merge(db.soil,db_affroc_code,by.x="AFFROC",by.y="code",all.x=T)
    setnames(db.soil,"value","affroc_perc")
    db.soil <- merge(db.soil,db_cailloux_code,by.x="CAILLOUX",by.y="code",all.x=T)
    setnames(db.soil,"value","cailloux_perc")
    
    # add correction when AFFROC and CAILLOUX = NA
      #required because lot of NA, especially for AFFROC, so should be interpreted as "low AFFROC", especially since it is consistent with "low CAILLOUX")
      db.soil[,table(AFFROC,CAILLOUX,useNA="always")]
      
      db.soil[is.na(AFFROC),affroc_perc:=0] # 0 means "less that 5 %". Here we can have a confusion between true NA and low AFFROC
      db.soil[is.na(CAILLOUX),cailloux_perc:=0] # 0 means "less that 5 %". Here we can have a confusion between true NA and low CAILLOUX

  # add texture values to the db
    
    db.soil <- merge(db.soil,db_teneur_A_code,by.x="TEXT1",by.y="code",all.x=T)
    setnames(db.soil,"value","teneur_A_text1")
    db.soil <- merge(db.soil,db_teneur_A_code,by.x="TEXT2",by.y="code",all.x=T)
    setnames(db.soil,"value","teneur_A_text2")
    db.soil <- merge(db.soil,db_teneur_EBC_code,by.x="TEXT1",by.y="code",all.x=T)
    setnames(db.soil,"value","teneur_EBC_text1")
    db.soil <- merge(db.soil,db_teneur_EBC_code,by.x="TEXT2",by.y="code",all.x=T)
    setnames(db.soil,"value","teneur_EBC_text2")
    
    
  # correction for PROF1=NA : then we consider that prof1_cm=0 (to enable the computation of the thickness of layer 2 while ensuring it has no impact on the computation of SWHC)and we consider that teneur_A_text1=0 & teneur_EBC_text1=0 (o avoid get NA in the SWHC computation)
    db.soil[is.na(PROF1) & !OBSPEDO %in% c("5","X"),":="(prof1_cm=0,teneur_A_text1=0,teneur_EBC_text1=0)]
  
    # to keep in mind
      db.soil[!OBSPEDO %in% c("5","X"),table(PROF1,TEXT1,useNA="always")]
      # if PROF1=NA , then TEXT1=NA. In this case, we want the contribution to SWHC to be null but not to be NA (else prevent the computation of SWHC) 
      db.soil[!OBSPEDO %in% c("5","X"),table(PROF2,TEXT2,useNA="always")]
      # when PROF2=NA, that corresponds to no pedological fosse (except fo one case): in these case, consider that PROF2=NA : it will  prevent the computation of SWHC, because we really lack information
    
    
  # Compute thibkness per horizon (A or EBC) and strata (1 or 2)
     # attention: at this stage we create NA if PROF1=NA, but it is corrected after!
      
    # First layer (horizon A) is fixed at 10 cm depth, second layer (horizons EBC) are below 10 cm.
      end_horizon_A <- 10
      
    # Water in First layer (A or TopSoil)
      db.soil[,epaisseur_A_1 := pmin(prof1_cm, end_horizon_A)] # Epaisseur de l'horizon A qui correspondant à PROF1 
      db.soil[,epaisseur_A_2 := pmin(prof2_cm - epaisseur_A_1, end_horizon_A - epaisseur_A_1)] # Epaisseur de l'horizon A qui correspondant à PROF2
    
    # Water in Second layer (EBC or SubSoil)
      db.soil[,epaisseur_EBC_1 := pmax(0,prof1_cm-end_horizon_A)]
      db.soil[,epaisseur_EBC_2 := pmax(0, pmin(prof2_cm - prof1_cm, prof2_cm - end_horizon_A))]


  # final SWHC computation
  
    db.soil[, SWHC_A := (1-affroc_perc/100)*(1-cailloux_perc/100)*(teneur_A_text1*epaisseur_A_1+teneur_A_text2*epaisseur_A_2)]
    db.soil[, SWHC_EBC := (1-affroc_perc/100)*(1-cailloux_perc/100)*(teneur_EBC_text1*epaisseur_EBC_1+teneur_EBC_text2*epaisseur_EBC_2)]
    db.soil[, SWHC := SWHC_A + SWHC_EBC]
    
    # case where TEXT1=H (but potentially TEXT2!=H)
    
    db.soil[TEXT1=="H" , SWHC_A := (1-affroc_perc/100)*(1-cailloux_perc/100)*(teneur_A_text2*epaisseur_A_2)]
    db.soil[TEXT1=="H", SWHC_EBC := (1-affroc_perc/100)*(1-cailloux_perc/100)*(teneur_EBC_text2*epaisseur_EBC_2)]
    db.soil[TEXT1=="H", SWHC := SWHC_A + SWHC_EBC]
    
    # put NA for the case where no OBSPEDO
    db.soil[OBSPEDO %in% c("5","X"), SWHC := NA]

    
  # analyse SWHC = NA
    
    db.soil[is.na(SWHC)]
    db.soil[is.na(SWHC) & !is.element(OBSPEDO,c("5","X"))]
    db.soil[is.na(SWHC) & !is.element(OBSPEDO,c("5","X")) & TEXT2 != "H"]
    db.soil[is.na(SWHC) & !is.element(OBSPEDO,c("5","X")) & TEXT2 != "H" & TEXT2 %in% c(1:9)]
    
  # analyse SWHC when PROF1=NA  
    db.soil[is.na(PROF1) & !OBSPEDO %in% c("5","X"),table(is.na(SWHC),TEXT2)] # all cases are linked to TEXT2 not known or =H
    
  return(db.soil[, c("IDP","YEAR", "SWHC_A", "SWHC_EBC", "SWHC","affroc_perc","cailloux_perc")])
  
}
