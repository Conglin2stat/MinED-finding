source("scenarios.R")

N_rep <- 60
res_scenario_all <- list()

for(m in 1:24){
  
  cat("scenario", m, ":", sep = " ")
  res_scenario <- list()
  
  ptrue_temp <- ptrue[[m]] 
  J_max <- ncol(ptrue_temp)
  
  prob_true <- tibble(
    level = 1:J_max,
    q_true = ptrue_temp[1, ],
    p_true = ptrue_temp[2, ]
    )

  #  2 ----------------------------------------------------------------------
  
  rm(next_dose)
  source("next_dose_retire2.R")
  res_scenario[[2]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)

  #  3 ----------------------------------------------------------------------
  rm(next_dose)
  source("next_dose_retire3.R")
  res_scenario[[3]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)

  #  4 ----------------------------------------------------------------------
  rm(next_dose)
  source("next_dose_retire4.R")
  res_scenario[[4]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)

  # 5 -----------------------------------------------------------------------
  
  rm(next_dose)
  source("next_dose_retire5.R")
  res_scenario[[5]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)
  

  # 6 -----------------------------------------------------------------------
  
  rm(next_dose)
  source("next_dose_retire6.R")
  res_scenario[[6]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)
  

  # 7 -----------------------------------------------------------------------
  
  rm(next_dose)
  source("next_dose_retire7.R")
  res_scenario[[7]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                     N.max = 60, target_tox = 0.3, target_eff = 0.3)
  

  # mu ----------------------------------------------------------------------
  
  rm(next_dose)
  source("next_dose_mu.R")
  res_scenario[["mu"]] <- get_OC(num_trial = N_rep, prob_true = prob_true,
                      N.max = 60, target_tox = 0.3, target_eff = 0.3)


  res_scenario_all[[m]] <- res_scenario 
}
  