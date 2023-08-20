#########load the needed R functions and packages#####
rm(list=ls())
gc()
setwd('C:/Users/DELL/Desktop/论文/代码/shixian')

source('boin_med.R')
source('get_boundary.R')
source('pava_applic.R')
source('boin_lead.R')
source('peak_lead.R')

library("Iso")

######################################################

## initial settings 
phi_t = 0.3
phi_e = 0.6
source('scenarios.R')

##############
ct = 0.95
ce = -1
d0 = 1
ncohort1 = 10
ncohort2 = 10
ntrial = 100
pcs = NULL
result = list()
set.seed(1)
for(isce in 1:24){
    cat('proceeding the', isce, 'th scenario', '\n' )
    ttox = ptrue[[isce]][1,] 
    teff = ptrue[[isce]][2,]
    result[[isce]] = BoinMED(ttox,teff,phi_t,phi_e,ct,ce,d0 = 1,cohortsize=3,ncohort1,ncohort2,ntrial,earlystop = FALSE,method = "peaklead")
    pcs[isce] = result[[isce]][3,c_targetid[isce]] 
}
plot(pcs,type="b",col='darkorange')
#save(result,file = 'result.peak.RData')
#write.csv(pcs,file = 'pcs.peak.csv')
