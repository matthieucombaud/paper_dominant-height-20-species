get_metadata<-function(metadata,name,exact.name=T){ #explore metadata. Exact.name=T means that you want the variable for which the "code" is eaxctly the name you provide. Else you look for a pattern in the "code" field
  
  output <-vector(mode="list",length = 3)
  names(output)<-c("variable","units","modalities")
  
  if(exact.name==F){
    output[["variable"]]<-metadata$variable[grep(pattern=name,x=code)]
  }else if(exact.name==T){
    output[["variable"]]<-metadata$variable[name==code]
  }
  
  output[["units"]]<-metadata$units[donnee%in%output[["variable"]]$code,]
  output[["modalities"]]<-metadata$modalities[unite%in%output[["units"]]$unite,.(unite,code,libelle,definition)]
  
  return(output)
  
}