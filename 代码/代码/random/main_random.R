rm(list=ls())
gc()
setwd('C:/Users/DELL/Desktop/论文/代码/shixian_random')

source('rsce.R')
source('boin_med.R')
source('get_boundary.R')
source('pava_applic.R')
source('boin_lead.R')
source('peak_lead.R')
source('prCI.R')

library("Iso")
######################################################

## initial settings 
phi_e = 0.3
#phi_e = 0.6
phi_t = 0.3
ct = 0.95
ce = 0.05
d0 = 1
ncohort1 = 10
c_ncohort2 = seq(10, 50, by=20)
nsample = length(c_ncohort2)
cohortsize = 3
nsce = 50000
ndose = 5
method = "peaklead"
#method = "boinlead"

N = YTOX = YEFF = array(0,c(nsample,ndose,nsce))
N_poor  = N_overtoxic = matrix(0,nsample,nsce)
recordSuccess = matrix(0,nsample,nsce)
tmat = emat = matrix(0,nsce,ndose)
idmat = idmtd = rep(0,nsce)

for(isc in 1:nsce)
{
  ###generate random scenarios #############
  temp = rsce(phi_t, phi_e, ndose)
  
  ttox = tmat[isc,] = temp$PT
  teff = emat[isc,] = temp$E
  
  idmat[isc] = temp$idtarget
  idmtd[isc] = temp$idmtd
  
  for(isam in 1:nsample){
    ncohort = ncohort1 + c_ncohort2[isam] 
    flag = "green"
    y=rep(0, ndose)
    z=rep(0, ndose)
    n=rep(0, ndose)
	prop_tox_est = rep(0,ndose)
	prop_eff_est = rep(0,ndose)
    ### safety monitoring ####
    ## dose-finding procedure
    d = d0
    earlystop = F
    for (i in 1:ncohort)
	{
      if(i <= ncohort1)
	  {
	    y[d]=y[d]+rbinom(1, cohortsize, ttox[d])
        z[d]=z[d]+rbinom(1, cohortsize, teff[d])
        n[d]=n[d] + cohortsize
		prop_tox_est[d] = (y[d]+0.05)/(n[d]+0.1)
		prop_eff_est[d] = (z[d]+0.1)/(n[d]+0.1)
		
        if(1 - pbeta(phi_t,1+y[1],1+n[1]-y[1])>0.95&n[1]>6&earlystop){
          dselect = 99
          flag="red"
          break
		  }
		  
		lambda_e_tox = get_boundary(target = phi_t, p.saf = 0.7*phi_t, p.tox=1.3*phi_t)$lambda_e
		lambda_d_tox = get_boundary(target = phi_t, p.saf = 0.7*phi_t, p.tox=1.3*phi_t)$lambda_d
		
		if(prop_tox_est[d] <= lambda_e_tox){d = min(d+1, ndose)}
        if(prop_tox_est[d] >= lambda_d_tox){d = max(d-1, 1)}
		
        if(i == ncohort1)
		{
          drange = range(admefftox(n,y,z,ct,ce,phi_t,phi_e))# determine the safety dose set
          if(drange[1]==0){
            dselect = 99
            flag="red"
            break
          }else
		  {
            c_d = drange[1]:drange[2]
            c_maxid = NULL
            for(id in 1:length(c_d))
			{
			  c_maxid[id] = (z[id]+0.05)/(n[id]+0.1)
              target_e = get_boundary(target = phi_e, p.saf = 0.6*phi_e, p.tox=1.4*phi_e)$lambda_e			  
            }
            dis = (c_maxid - target_e)
            if(any(dis >= 0))
			{
              d = which(dis >= 0)[1]
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
        y[d] = y[d] + rbinom(1, cohortsize, ttox[d])
        z[d] = z[d] + rbinom(1, cohortsize, teff[d])
        n[d] = n[d] + cohortsize
		prop_tox_est[d] = (y[d]+0.05)/(n[d]+0.1)
		
        drange = range(admefftox(n,y,z,ct,ce,phi_t,phi_e))
        if(1 - pbeta(phi_t,1+y[1],1+n[1]-y[1])>0.95&n[1]>6&earlystop){
          dselect = 99
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
          dselect = 99 
          flag="red"
          break
        }
      }
    }
    cat("flag",flag,'\n')
    if(flag == "green")
	{
      c_maxid = NULL
      dtried = max(which(n > 0))
      for(id in 1:dtried){
        c_maxid[id] = (z[id]+0.05)/(n[id]+0.1)
      }
      dis = (c_maxid - target_e)
      if(any(dis >= 0)){
        dselect = which(dis >= 0)[1]
      }else{
        dselect = which.min(abs(dis))[1]
      }
    }
    N[isam,,isc] = n
    YTOX[isam,,isc] = y
    YEFF[isam,,isc] = z
    recordSuccess[isam,isc] = as.numeric(dselect == idmat[isc])
    if(idmat[isc] == 1){
      N_poor[isam,isc] = 0
    }else{
      N_poor[isam,isc] = sum(N[isam,1:(idmat[isc]-1),isc]) 
    }
    if(idmtd[isc] < ndose){
      N_overtoxic[isam,isc] = sum(N[isam,(idmtd[isc]+1):ndose,isc]) 
    }
  }
}
N_poor  = matrix(0, nsample, nsce)
N_overtoxic = matrix(0, nsample, nsce)
N_med = matrix(0, nsample, nsce)
for(isc in 1:50000){
  for(isam in 1:3){
    N_med[isam,isc] = N[isam,idmat[isc],isc]
    if(idmat[isc]==1){
      N_poor[isam,isc] = 0
    }else{
      N_poor[isam,isc] = sum(N[isam,1:(idmat[isc]-1),isc]) 
    }
    if(idmtd[isc] < ndose){
      N_overtoxic[isam,isc] = sum(N[isam,(idmtd[isc]+1):ndose,isc]) 
    }
  }
}

result = rbind(
  round(apply(recordSuccess,1,mean)*100,1),
  round(apply(N_med,1,mean),1),
  round(apply(N_poor,1,mean)/c(60,120,180)*100,1),
  round(apply(N_overtoxic,1,mean)/c(60,120,180)*100,1))
  
save(N, YTOX, YEFF, idmat, idmtd, recordSuccess,
     N_poor,N_overtoxic,N_med,tmat,emat,
     file = "result_peak_03_gait.RData")
