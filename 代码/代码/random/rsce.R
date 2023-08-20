################### Generate random scenarios ##########################

rsce<-function(phi, psi, J){
  sizeD = J
  D = 1:sizeD
  alpha = phi
  med <- sample(D,1,prob=rep(1/length(D),sizeD))
  mtd <- sample(med:sizeD,1,replace = TRUE)
  if(mtd < sizeD){temp <- sort(runif(length(D)-mtd,alpha,1));bornesup <- temp[length(D)-mtd]}
  if(mtd == sizeD){bornesup <- alpha +(1-alpha)*rbeta(1,0.5,1)}
  test <- 0
  while(test == 0){
  PT <- sort(runif(sizeD,0,bornesup))
  temp <- which.min(abs(PT - alpha))
  if(temp == mtd&min(abs(PT-alpha)) < 0.2*phi){test <- 1}
  }
  # the shape of response curve
  # 1: ingcreasing
  # 2: plateau
  # 3: firstly increaing and then decreasing
  #shape = sample(c(1,2,3),1)
  shape = sample(c(1,2,3),1,prob = c(0.33,0.33,0.33))
  
      if(med < sizeD){
        if(shape == 1){
          E = c(sort(runif(med-1, 0, psi)), psi, sort(runif(sizeD-med,psi,1)))
        }else{
          if(shape == 2){
            E = c(sort(runif(med-1, 0, psi)), psi, rep(psi,sizeD-med))
          }else{
            E = c(sort(runif(med-1, 0, psi)), psi, sort(runif(sizeD-med, 0, psi),decreasing = T))
          }
        }
      }else{
        E = c(sort(runif(med-1, 0, psi)), psi)
      }
    
  return(list(PT=PT, E=E, 
              idtarget=med,
              idmtd= mtd))
}




