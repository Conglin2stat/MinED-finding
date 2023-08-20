peaklead <- function(d, drange, dopt, dtried, q_hat, target_e, target_d){
    if(d < dopt)
	{
      if(q_hat < target_e){
        d= min(d+1, drange[2]) 
      }else{
        if(q_hat > target_d){
          d = max(drange[1], d-1)
        }else{
          d = d
        }
      }
    }else
	{
      if(d == dopt)
	  {
        if(q_hat < target_e)
		{
          if(dopt < dtried)
		  { ##qhat < lamada1 and j0 < J
            d = min(d+1, drange[2])
          }else
		  {
            d = d 
          }
        }else
		{
          d = max(drange[1], d-1)
        }
      }else
	  {
        d = max(drange[1], d-1)
      }
    }
  return(d)
}