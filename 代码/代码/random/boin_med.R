####################################################
# An Extension version of the Keyboard method     ##
####################################################
# phi_t = 0.3
# phi_e = 0.3
# prior:beta(1, 1)
# n: number of patients treated at each dose level 
# y: ...................experienced dlt at each dose level
# z: ...................have responses at each dose level d
################################################################
#maxKey(n,y,phi,eps)
# d0 is the  starting dose level

BoinMED <- function(ttox,teff,phi_t,phi_e,ct,ce,d0 = 1,cohortsize=3,ncohort1,ncohort2,ntrial,earlystop,method)
{
  ncohort = ncohort1 + ncohort2
  ndose = length(ttox)
  N = matrix(rep(0,ndose*ntrial), ncol=ndose)
  YTOX = matrix(rep(0,ndose*ntrial), ncol=ndose) 
  YEFF = matrix(rep(0,ndose*ntrial), ncol=ndose)
  dselect = rep(0, ntrial)
  for(itrial in 1:ntrial){
    cat('trial proceeding:')
    cat(' ',itrial,'\n')
    flag = "green"
    y = rep(0,ndose)
    z = rep(0,ndose)
    n = rep(0,ndose)
	prop_tox_est = rep(0,ndose)
	prop_eff_est = rep(0,ndose)
    ### safety monitoring ####
    ## dose-finding procedure
    d = d0
    for (i in 1:ncohort)
	{
      if(i <= ncohort1)
	  {
	    y[d]=y[d]+rbinom(1, cohortsize, ttox[d])
        z[d]=z[d]+rbinom(1, cohortsize, teff[d])
        n[d]=n[d] + cohortsize
		prop_tox_est[d] = (y[d]+0.1)/(n[d]+0.1)
		prop_eff_est[d] = (z[d]+0.1)/(n[d]+0.1)
		
        if(1 - pbeta(phi_t,1+y[1],1+n[1]-y[1])>0.95&n[1]>6&earlystop){
          dselect[itrial] = 99
          flag="red"
          break
		  }
		  
		lambda_e_tox = get_boundary(target = phi_t, p.saf = 0.6*phi_t, p.tox=1.4*phi_t)$lambda_e
		lambda_d_tox = get_boundary(target = phi_t, p.saf = 0.6*phi_t, p.tox=1.4*phi_t)$lambda_d
		
		if(prop_tox_est[d] <= lambda_e_tox){d = min(d+1, ndose)}
        if(prop_tox_est[d] >= lambda_d_tox){d = max(d-1, 1)}
		
        if(i == ncohort1)
		{
          drange = range(admefftox(n,y,z,ct,ce,phi_t,phi_e))# determine the safety dose set
          if(drange[1]==0){
            dselect[itrial] = 99
            flag="red"
            break
          }else
		  {
            c_d = drange[1]:drange[2]
            c_maxid = NULL
            for(id in 1:length(c_d))
			{
			  c_maxid[id] = (z[id]+0.1)/(n[id]+0.1)
              target_e = get_boundary(target = phi_e, p.saf = 0.6*phi_e, p.tox=1.4*phi_e)$lambda_e			  
            }
            dis = (c_maxid - target_e)
            if(any(dis>=0))
			{
              d = which(dis>=0)[1]
            }else
			{
              d = which.min(abs(dis))[1]
            }
            #d = drange[1] # as the initial dose for stage 2
          }
          } 
      }else{ # i> ncohort1
        if(d<1|d>ndose){
          cat('~error~error~error~error~error~error~error~error~','\n')
        }
        y[d] = y[d] + rbinom(1,cohortsize,ttox[d])
        z[d] = z[d] + rbinom(1,cohortsize,teff[d])
        n[d] = n[d] + cohortsize
		prop_tox_est[d] = (y[d]+0.1)/(n[d]+0.1)
		
        drange = range(admefftox(n,y,z,ct,ce,phi_t,phi_e))
        if(1 - pbeta(phi_t,1+y[1],1+n[1]-y[1])>0.95&n[1]>6&earlystop){
          dselect[itrial] = 99
          flag = "red"
          break
        }
		
        q_hat = z[d]*1.0/n[d]
        target_e = get_boundary(target = phi_e, p.saf = 0.6*phi_e, p.tox=1.4*phi_e)$lambda_e
		target_d = get_boundary(target = phi_e, p.saf = 0.6*phi_e, p.tox=1.4*phi_e)$lambda_d
		
        if(drange[1]!=0){
            dtried = max(which(n > 0))
            zfit = (z + 0.05)/(n + 0.1)
			zfit.var = (z + 0.05) * (n - z + 0.05)/((n + 0.1)^2 * (n + 0.1 + 1))
			
            unifit = ufit(zfit, x=1:ndose, w=1/zfit.var, type="b")
            dopt = unifit$mode
            d = switch(method,
              boinlead = boinlead(d, drange, dopt, dtried, q_hat, target_e, target_d),
              peaklead = peaklead(d, drange, dopt, dtried, q_hat, target_e, target_d)
            )
        }else{ # the admissible set is empty
          dselect[itrial] = 99 
          flag="red"
          break
        }
      }
    }
    cat("flag",flag,'\n')
    if(flag=="green")
	{
#####################################################################
  # final proposal-one ----v2
          c_maxid = NULL
          dtried = max(which(n > 0))
          for(id in 1:dtried){
            c_maxid[id] = (z[id]+0.1)/(n[id]+0.1)
          }
          dis = (c_maxid - target_e)
          if(any(dis>=0))
		  {
            dselect[itrial] = which(dis>=0)[1]
            }else
			{
              dselect[itrial] = which.min(abs(dis))[1]
            }
    }
    N[itrial,]=n
    YTOX[itrial,]=y
    YEFF[itrial,]=z
    cat('MED:',dselect[itrial],'\n')
    cat('--------------------------------','\n')
  }
  selpercent=rep(0, ndose)
  ## Summarize results 
  for(i in 1:ndose) { selpercent[i]=sum(dselect==i)/ntrial*100 }
  oc<- matrix(NA,ncol = ndose+2, nrow = 6)
  oc[1,1:ndose] = ttox
  oc[2,1:ndose] = teff
  oc[3,1:ndose] = selpercent
  oc[4,1:(ndose+1)] = c(apply(N,2,mean),sum(apply(N,2,mean)))
  oc[5,1:(ndose+1)] = c(apply(YTOX,2,mean),sum(apply(YTOX,2,mean)))
  oc[6,1:(ndose+1)] = c(apply(YEFF,2,mean),sum(apply(YEFF,2,mean)))
  colnames(oc)=c(paste("Dose",1:ndose,sep=""),"Number of Patients"," ")
  row.names(oc) =c("True DLT rate","True Efficacy rate","Sec %",
                   "#Pts treated","#Pts response to tox","#Pts response to eff")
  return(oc)
}
