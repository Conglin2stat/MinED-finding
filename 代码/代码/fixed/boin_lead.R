boinlead <- function(d, drange, dopt, dtried, q_hat, target_e, target_d)
{
    if(q_hat < target_e){
      if(d < dopt){
        d = min(d+1, drange[2])
      }else{# d >= dopt
        if(d > dopt){
          d = max(d-1, drange[1])
        }else{#d=dopt
          if(dopt < dtried){ # d = dopt and dopt < dtried
            d = min(d+1, drange[2])
          }else{
            d = d
          }# otherwise retain at the same dose level j
        }
      }
    }else{
      if(q_hat > target_d){
        d = max(d-1, drange[1])
        # otherwise retain at the same dose level j
      }else{
        d = d
      }
    }
  return(d)
}