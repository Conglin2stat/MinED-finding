
get_boundary <- function(p_null, p_alter){
  
  ratio1 <- p_null/p_alter
  ratio2 <- (1-p_null)/(1-p_alter)
  
  boundary <- log(ratio2)/log(ratio2/ratio1)
  
 return(boundary)
}

