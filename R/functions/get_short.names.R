get_short.names <- function(
  db_species.name, # datatable with species names
  species_list # vector of species under consideration
){
  
  # restrict to species under scrutiny (to limit duplication of short names)
  db_species.name <- db_species.name[code %in% species_list]
  
  # short names
  
    # get the short names
    db_species.name[, name.short := paste0(
      str_sub(str_split(name, " ", simplify = T)[,1],1,2),
      ".",
      str_sub(str_split(name, " ", simplify = T)[,2],1,2)
    )
    ]
    
    # check the number of times the short name is used
    db_species.name[, uses := .N, by = name.short]
    
    db_species.name[uses > 1, name.short := paste0(
      str_sub(str_split(name, " ", simplify = T)[,1],1,2),
      ".",
      str_sub(str_split(name, " ", simplify = T)[,2],1,2),
      ".",
      str_sub(str_split(name, " ", simplify = T)[,4],1,2)
    )]
    
    db_species.name[, uses := .N, by = name.short]
    
    if(unique(db_species.name$uses) != 1){ print ("problem")}
  
  # abbreviated names
    
    # get abbreviated name
    
    db_species.name[, name.abbreviated := paste0(
      str_sub(str_split(name, " ", simplify = T)[,1],1,1),
      ". ",
      str_split(name, " ", simplify = T)[,2]
    )
    ]
    
    db_species.name[, uses.abbreviated := .N, by = name.abbreviated]
    
    db_species.name[uses.abbreviated > 1, name.abbreviated := paste0(
      str_sub(str_split(name, " ", simplify = T)[,1],1,1),
      ". ",
      str_split(name, " ", simplify = T)[,2],
      " ",
      str_split(name, " ", simplify = T)[,3],
      " ",
      str_split(name, " ", simplify = T)[,4]
      
    )]
    
    db_species.name[, uses.abbreviated := .N, by = name.abbreviated]
    
    if(unique(db_species.name$uses.abbreviated) != 1){ print ("problem")}
  
    # names simplified
    db_species.name[name == "Larix decidua subsp. decidua", name := "Larix decidua"]
    db_species.name[name == "Picea abies subsp. abies", name := "Picea abies"]
    db_species.name[name == "Pinus mugo subsp. uncinata", name := "Pinus mugo"]
    db_species.name[name == "Pinus pinaster subsp. pinaster", name := "Pinus pinaster"]
    db_species.name[name == "Quercus robur var. robur", name := "Quercus robur"]
    db_species.name[name == "Quercus petraea subsp. petraea", name := "Quercus petraea"]

    
    # order
    db_species.name <- db_species.name[order(name)]
    
  return(db_species.name)
  
  
}



