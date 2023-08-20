get_boundary <- function(target, p.saf, p.tox){

  lambda1  = log((1-p.saf)/(1-target))/log(target*(1-p.saf)/(p.saf*(1-target)))
  lambda2  = log((1-target)/(1-p.tox))/log(p.tox*(1-target)/(target*(1-p.tox)))
  
  list(lambda_e = lambda1,lambda_d = lambda2)
}