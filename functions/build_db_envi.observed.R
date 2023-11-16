# Description
# From climatic, radiation and NFI data base, build a db with all environmental database

# Arguments
# db_ifn.envi: db with IFN environmental data for each stand
# db_climate: db with climatic data for each stand
# db_radiation_annual: db with annual radiation data for each stand

# Value
# a subvector of stand 

build_db_envi.observed <- function(
  db_ifn,
  db_climate,
  db_radiation_annual
){
  
  db <- merge(db_ifn, db_climate, by="stand") # introduces the "climatic_year column"
  db <- merge(db, db_radiation_annual, by="stand")
  
  return(db)
  
}
