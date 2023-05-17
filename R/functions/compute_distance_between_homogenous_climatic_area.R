compute_distance_between_homogenous_climatic_area<-function(file){
  
  load(file)
  
  data_homogeneous_climatic_areas<-isbagrid
  
  sf_homogeneous_climatic_areas<-st_as_sf(data_homogeneous_climatic_areas$coordsall)
  sf_homogeneous_climatic_areas$symp<-data_homogeneous_climatic_areas$symp
  sf_homogeneous_climatic_areas$cell<-1:length(data_homogeneous_climatic_areas$symp)
  
  homogeneous_climatic_areas<-unique(sf_homogeneous_climatic_areas$symp)
  homogeneous_climatic_areas<-homogeneous_climatic_areas[order(homogeneous_climatic_areas)]
  
  distance<-matrix(nrow=length(homogeneous_climatic_areas),ncol=length(homogeneous_climatic_areas))
  rownames(distance)<-homogeneous_climatic_areas
  colnames(distance)<-homogeneous_climatic_areas
  
  for(i in homogeneous_climatic_areas){
    for(j in intersect(c(i+1:max(homogeneous_climatic_areas)),homogeneous_climatic_areas)){
      
      print(paste0(i," , ",j))
     
      symp_i<-sf_homogeneous_climatic_areas[sf_homogeneous_climatic_areas$symp==i,]
      symp_j<-sf_homogeneous_climatic_areas[sf_homogeneous_climatic_areas$symp==j,]
      
      distance[rownames(distance)==i,colnames(distance)==j]<-min(st_distance(x=symp_i,y=symp_j))
      
    }
  }
  
  for(i in homogeneous_climatic_areas){ # to ensure symetry
    for(j in intersect(c(1:i-1),homogeneous_climatic_areas)){
      
      distance[rownames(distance)==i,colnames(distance)==j]<-distance[rownames(distance)==j,colnames(distance)==i]
      
    }
  }
  
  return(distance)  
  
}