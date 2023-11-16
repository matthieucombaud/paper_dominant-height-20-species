get_SI_approximate <- function(
  db_DH_dyn_pur, 
  age.ref
){
  
  return(db_DH_dyn_pur[age == age.ref, .(stand, species, SI = DH_p)])
  
}