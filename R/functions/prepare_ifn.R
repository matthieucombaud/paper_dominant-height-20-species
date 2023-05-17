prepare_ifn<-function(sp,db_stand,stand_number,non_climatic_variables,safran.map,crs_IFN,db_temperature_long,db_precipitation_long,lapse_rate_temperature,lapse_rate_precipitation,limit.period.test,duration,last_month,db_co2,first_year){
  
  # subselect stands per species
  if(all(sp!="all")){
    db<-db_stand[Sp1.Gtot%in%sp]
  }else{
    db<-db_stand
  }
  
  # filter stand in the perimeter of safran data
  out_of_safran_perimeter<-is.na(get_safran_cell(db=db,safran.map=safran.map,crs=crs_IFN))
  db<-db[!out_of_safran_perimeter]
  print(paste0(sum(out_of_safran_perimeter)," stands excluded because outside of Safran perimeter"))
  
  
  # filter stands for which we have complete data concerning the non_climatic variables
  keep_col_filter_uncomplete_data<-intersect(names(db),non_climatic_variables) # necessary because not all the non climatic variables have been added to the db yet (radiation & co2)
  uncomplete_data<-!complete.cases(db[,..keep_col_filter_uncomplete_data])
  db<-db[!uncomplete_data]
  print(paste0(sum(uncomplete_data)," stands excluded because uncomplete non-climatic data (for the selected variables)"))
  
  # subselect stands to get a certain stand number
  if(stand_number!="all"){ # case we study a subsample
    set.seed(123)
    stands_selected<-sample(x=db$stand,size=stand_number,replace=F)
    db<-db[stand%in%stands_selected]
  }
  
  # add temperature & precipitation
  
  db_temperature_precipitation<-prepare_ifn_temperature_precipitation(db=db,
                                                                      safran.map=safran.map,
                                                                      crs_IFN=crs_IFN,
                                                                      db_temperature_long=db_temperature_long,
                                                                      db_precipitation_long=db_precipitation_long,
                                                                      lapse_rate_temperature=lapse_rate_temperature,
                                                                      lapse_rate_precipitation=lapse_rate_precipitation,
                                                                      limit.period.test=limit.period.test,
                                                                      first_year=first_year
                                                                      )
    
  # get radiation db
  db_radiation<-prepare_ifn_radiation(db=db,cloud=T) # monthly_radiations in MJ/m2/month
  db_radiation_annual<-db_radiation[,.(radiation_annual=sum(radiation)),by=stand]

  # get db with temperature and climatic water balance (cwb) (cwb=P-ETP)
  db_climate<-prepare_ifn_cwb_sgdd(db_temperature_precipitation=db_temperature_precipitation,db_radiation=db_radiation)

  # format climate: combine month (for Tmean : mean over several months, for cwb : cumul over several months)
  db_climate<-aggregate_climate_global(db=db_climate,last_month=last_month,duration=duration) # last month of the climatic year
  
  # prepare IFN complete database
  db_ifn_all_variables<-prepare_ifn_stand_climate(db_stand=db,db_climate=db_climate,db_co2=db_co2,db_radiation_annual=db_radiation_annual)

  return(db_ifn_all_variables)
  
}
