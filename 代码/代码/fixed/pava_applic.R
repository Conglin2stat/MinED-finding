
admefftox <- function(n,y,z,ct,ce,phi_t,phi_e)
{
  dtried = max(which(n != 0))
  rest = rep(0, length(n))
  phi_tu = phi_t 
  phi_el = phi_e
  rest[1:dtried] = 1-pbeta(phi_tu,1+y[1:dtried],1+n[1:dtried]-y[1:dtried])
  rese = 1-pbeta(phi_el,1+z,1+n-z)
  rest=pava(rest, w=n)
  if(any(rest <= ct)&any(rese >= ce)){
    drange = min(which(rese >= ce)):max(which(rest <= ct)) ##最小有效以及最大未过毒性的
    if(min(which(rese >= ce)) > max(which(rest <= ct))){
      cat('\n','drange[1]>drange[2]','\n')
      cat('error~error~error~error~error~error~error','\n')
    }
  }else{
    cat('warning~warning~warning~warning~warning~warning~warning','\n')
    drange = 0
  }
  return(drange)
}


