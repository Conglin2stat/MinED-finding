
# prefix "q_" stands for the toxicity probability (for true/obs/est)
# prefix "p_" stands for the efficacy probability (for true/obs/est)

# prob_true = tibble(
#   level = 1:5,
#   q_true = c(0.01, 0.05, 0.10, 0.15, 0.30),
#   p_true = c(0.15, 0.3, 0.45, 0.25,0.15))

# source("next_dose.R")

trial_simulate <- function(prob_true, start_level = 1, cohort.size = 3, N.max,
                           target_tox, upper_ratio_tox = 1.3,
                           target_eff, upper_ratio_eff = 1.3, lower_ratio_eff = 0.7, 
                           eff_cut = 0.95, saf_cut = 0.95
                           ){
  
  num_level <- nrow(prob_true)
  trial_set <- 
    prob_true %>% 
    left_join(
      tibble(
        level = prob_true$level,
        start_level = start_level,
        N.max = N.max,
        cohort_size = cohort.size,
        n = rep(0, num_level),
        num_saf = rep(0, num_level),
        num_eff = rep(0, num_level),
        safe_flag = T
      )
    )
    
# calculate the decision boundaries for toxicity and efficacy
  phi_0 <- target_tox
  phi_1 <- target_tox*upper_ratio_tox
  phi_U <- get_boundary(phi_0, phi_1)
  
  psi_0 <- target_eff 
  psi_1 <- target_eff*lower_ratio_eff
  psi_L <- get_boundary(psi_0, psi_1)
  
  psi_2 <- target_eff*upper_ratio_tox
  psi_U <- get_boundary(psi_0, psi_2)
  
  Obs_cum <- trial_set %>% 
    mutate(RespRate_fit = NA,
           current_level = 1,
           next_level = 1)
  
  num_cohort <- ceiling(N.max/cohort.size)
  for(i in 1:num_cohort){
    # observation update by using random generation
    Obs_update <- Obs_cum %>% 
      group_by(level) %>% 
      mutate(current_level = next_level,
             next_level = NA,
             n = ifelse(level == current_level, 
                        n + cohort_size, n),
             num_saf = ifelse(level == current_level, 
                              num_saf + rbinom(n = 1, size = cohort_size, prob = q_true), num_saf),
             num_eff = ifelse(level == current_level, 
                              num_eff + rbinom(n = 1, size = cohort_size,  prob = p_true), num_eff)
      ) %>% 
      ungroup()
    
    # descide the next dose level
    Obs_cum <- next_dose(Obs_update, phi_0 = phi_0, phi_U = phi_U, psi_0 = psi_0,
                psi_U = psi_U, psi_L = psi_L, eff_cut = eff_cut, saf_cut = saf_cut)$dataset.out
    
    cat("cohort ", i, ": ", sep = "")
    
    earlystop.saf <- Obs_cum$earlystop_saf %>% unique()
    earlystop.eff <- Obs_cum$earlystop_eff %>% unique()
    
    if (earlystop.saf){
      message("\n The trial is early terminated as the lowest dose is overly toxic")
      break
    }
    
    if (earlystop.eff){
      message("\n The trial is early terminated and we can conclude that 
              no dose level in this trial meets the criteria for MinED")
      break
    }
    
    next.level <- Obs_cum$next_level %>% unique()
    cat("next level is", next.level, "\n")
  }
  

  # EarlyStop_flag <- ifelse( i < num_cohort, "Y", "N")
  export <- Obs_cum %>% 
    mutate(phi_0 = phi_0,
           phi_U = phi_U,
           psi_0 = psi_0,
           psi_L = psi_L,
           psi_U = psi_U,
           Stop_at_cohort = i
           )
  return(dataset.out = export)
}

# Example -----------------------------------------------------------------

# Obs_final <- trial_simulate(prob_true, cohort.size = 3, N.max = 60,
#                target_tox = 0.3, target_eff = 0.28)
