prepare_data_homogeneous_climatic_areas<-function(file){
  
  load(file)
  
  data_homogeneous_climatic_areas<-isbagrid
  
  db_homogeneous_climatic_areas<-data.table(cell=c(1:length(data_homogeneous_climatic_areas$symp)),
                                            symp=data_homogeneous_climatic_areas$symp,
                                            alt=data_homogeneous_climatic_areas$alt,
                                            in_France=data_homogeneous_climatic_areas$in.France)
  
  return(db_homogeneous_climatic_areas)
  
}
