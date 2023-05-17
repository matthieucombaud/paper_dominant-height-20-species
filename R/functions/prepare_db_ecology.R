prepare_db_ecology<-function(db.ecology){

  ###########################
  # focus on pedology
  ####################
  
  # add SWHC (soil water holding capacity)
    db.ecology <- merge(db.ecology, compute_swhc(db.soil=db.ecology), by=c("IDP","YEAR"),all.x = T) #careful to merge also on YEAR since there are some remeasures...
    db.ecology[,table(is.na(SWHC))]

  # group soil, humus and creating calc/sili variables

    # soil
    # translate TSOL into simplified soil type (based on the IFN doc "DOCUMENTATION RELATIVE AUX DONNEES ECOLOGIQUES POINTS FORET – DEPUIS LA CAMPAGNE 2017")
    # attention: TSOL is imported as integer, so no "0" before "1","2","3".
    
    {
      db.ecology[,soil_type:="other"] # other is the standard modality, since some values are not classifed. But NA and "" are reclassified as NA below
      db.ecology[TSOL %in% c("85", "82", "83", "89", "59", "87", "81", "11"), soil_type := "A"]
      db.ecology[TSOL %in% c("28", "48", "88", "58", "38"), soil_type := "B"]
      db.ecology[TSOL %in% c("16", "17", "14", "22", "24", "12", "13", "18", "15", "1", "11", "19", "29", "2", "88", "3"), soil_type := "C"]
      db.ecology[TSOL %in% c("31", "21", "23", "25", "32", "22", "26", "27", "28", "29"), soil_type := "D"]
      db.ecology[TSOL %in% c("51", "52", "53", "58", "87", "59"), soil_type := "E"]
      db.ecology[TSOL %in% c("42", "33", "39", "34", "35"), soil_type := "F"]
      db.ecology[TSOL %in% c("61", "62", "63"), soil_type := "G"]
      db.ecology[is.na(TSOL) | TSOL =="" | TSOL==99, soil_type := NA_character_] # we put the case "99" here, since all plots with no pedological fosse (OBSPEDO==5) are classified as 99, and we cannot have info on these soils
    }
    
    # Check no NA (appart for TSOL==99)
    db.ecology[,table(soil_type,TSOL,useNA = "always")] # soil_type = "other" for TSOL=41,43,44,45, because no correspondance in the IFN doc. All these soil are "lessivé". "Sol lessivé" appears as a specific category in the methodological IFN doc for 2006-2010) but not in the "Doc-DB_ecologie" : maybe we should add this category? Other category in the same case: calcic soils. But pb if we use this "soil lessivé" category : what about soil 48 (classified as B in "Doc-DB_ecologie") and 42 (classified as F in "Doc-DB_ecologie"). No correction implemented here.

  #identify calcareous bedrocks
    db.ecology[,ROCHE.Calc:=((ROCHE >=300 & ROCHE <500) | ROCHE == 604)]
    
    # add humus classification based on IFN "Doc-DB_ecologie_PF_2017-2019"
    {
      db.ecology[,humus_type:="other"] # other is the standard modality, since some values are not classifed. But NA and "" are reclassified as NA below
      db.ecology[HUMUS %in%c(30,31,42),humus_type:="mull_1"]
      db.ecology[HUMUS %in%c(40,50),humus_type:="mull_2"]
      db.ecology[HUMUS %in%c(10,21),humus_type:="mor"]
      db.ecology[HUMUS %in%c(20,22),humus_type:="moder"]
      db.ecology[HUMUS %in%c(15,25,45,55),humus_type:="carbo"]
      db.ecology[HUMUS %in%c(18,28,48,80,81),humus_type:="hydro"]
      db.ecology[is.na(HUMUS) | HUMUS=="" | HUMUS ==99, humus_type := NA_character_]
    }
    
    db.ecology[,table(humus_type,HUMUS,useNA = "always")] # 1849 for which humus_type=="other"

  #############################  
  # focus on exposition
  ##########################  
    
    db.ecology[,expo_NS := cos(EXPO*(2*pi/400))] #1 --> North, -1 --> South #conversion from grades to radians
    db.ecology[,expo_EW:=sin(EXPO*(2*pi/400))] #1 --> East, -1 --> West #conversion from grades to radians
    
    #if no expo info and no slope (as we can predict from topo), then no specific orientation (to avoid loosing points because of NA in expo), so we affect the "mean" orientation, which is 0
    db.ecology$expo_NS[is.na(db.ecology$expo_NS) & db.ecology$TOPO %in% c(0, 1, 2, 6,7,8,9)] <- 0 
    db.ecology$expo_EW[is.na(db.ecology$expo_EW) & db.ecology$TOPO %in% c(0, 1, 2, 6,7,8,9)] <- 0
    
    # check NA after correction
    # we have NA mostly when OBSTOPO==4 ("Relevé effectué sur un ensemble topographique hétérogène avec impossibilité de coder l'exposition et la pente.").
    db.ecology[is.na(expo_NS),table(OBSTOPO,TOPO,useNA = "always")]
    db.ecology[is.na(expo_EW),table(OBSTOPO,TOPO,useNA = "always")] 
    
#############################  
# focus on exposition and aquatic elements
##########################  
    
  db.ecology[, aquatic_element := numeric()] # aquatic_element is 0 if no water in the 25 m radius, 1 if there is some water in the 25m radius
  db.ecology[YEAR <= 2016, aquatic_element := OBSRIV ] # OBSRIV variable for 2005-2016
  db.ecology[YEAR > 2016 & !is.na(DISTRIV), aquatic_element := DISTRIV %in% c(1,2) ] # DISTRIV variable for 2017-2020

  db.ecology[,table(aquatic_element,YEAR,useNA="always")] # attention : strong fluctuation across time...


  return(db.ecology)
  
}
