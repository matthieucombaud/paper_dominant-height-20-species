prepare_ifn_cwb_sgdd <- function(db_temperature_precipitation,db_radiation){
  
  db_temperature_precipitation_wide <- dcast(data = db_temperature_precipitation, stand + month + year ~ variable, value.var = "value")
  
  # merge db temperature, precipitation, radiation
  db_t_p_r <- merge(db_temperature_precipitation_wide, db_radiation, by=c("stand","month")) # a row = stand*year*month
  
  # add number of days (because the formula require per day radiations)
  db_t_p_r[,nb.days := 31]
  db_t_p_r[month %in% c(4,6,9,11), nb.days:=30]
  db_t_p_r[month%in%c(2), nb.days := ifelse(year%%4==0&(!year%%100==0|year%%400==0),29,28)] # bissextile years
  
  # Turc formula assuming relative humidity > 50% (Lebourgeois & Piedallu, 2005, Appréhender le niveau de sécheresse dans le cadre des études stationnelles et de la gestion forestière?, Revue foretière française, https://www.researchgate.net/publication/239526048_Comment_apprehender_le_niveau_de_secheresse_dans_le_cadre_des_etudes_stationnelles_et_de_la_gestion_forestiere_Notion_d'indices_bioclimatiques_methode_d'estimation_de_l'evapotranspiration_potentielle)
  db_t_p_r[,etp := nb.days * 0.013 * (radiation * 10^6 / 4.184 / 100^2 / nb.days + 50)*( temperature / (temperature+15) )]
    # etp in mm/month
    # convert radiations in MJ/m2/month in radiations in cal/cm2/day
    # 1 J = 0.239 cal (https://www.thermal-engineering.org/fr/quest-ce-que-joule-unite-j-unite-denergie-definition/?utm_content=cmp-true )
    # 1 cal = 4.1884 J
  db_t_p_r[temperature < 0,etp:=0] # manual correction for negative temperature
  
  # compute monthly climatic water balance
  db_t_p_r[, cwb := precipitation - etp]
  
  # compute *monthly* sgdd
  db_t_p_r[,sgdd := nb.days*pmax(temperature-5.5,0)]
  
  return(db_t_p_r[,.(stand,year,month,precipitation,Tmean=temperature,cwb,sgdd)])
  
}
