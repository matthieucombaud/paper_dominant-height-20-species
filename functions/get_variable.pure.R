# Description
# Get selected variable for SDH mdels in pure stand, from a list of objects used for calibration, for each species
# 
# Argument
# list_parameter.SDH.model: list of db of selected variables, per species

# Value
# list of variables used for calibration of SDH model in pure stand, for each species

get_variable.pure <- function(
  list.param.pure
){

  list_variable <- lapply(unique(names(list.param.pure)), function(species.sel){

    return(unique(list.param.pure[[species.sel]][param_class %in% c("alpha", "gamma"), param]))

  })
  names(list_variable) <- unique(names(list.param.pure))
  
  return(list_variable)
  
}
