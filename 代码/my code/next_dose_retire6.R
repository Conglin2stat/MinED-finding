source("double_sided_IsoReg.R")
source("get_boundary.R")
library(tidyverse)

next_dose <- function(Obs_cum, phi_0, phi_U, psi_0, psi_L, psi_U,
                      eff_cut, saf_cut){
  
  J <- max(Obs_cum$level)
  
  Obs_tried  <- Obs_cum %>% 
    filter(n != 0) %>% 
    arrange(level) %>% 
    mutate(q_obs = num_saf/n,
           p_obs = num_eff/n,
           seq = 1:n()
    )
  j_h <- max(Obs_tried$level) # the highest level that has been tried
  
  # estimate the maxima of the dose-response curve based on the collected data
  IsoReg_eff_res<- IsoReg_2.sided(y = Obs_tried$p_obs, w = Obs_tried$n)
  Obs_opt <- Obs_tried %>% 
    filter(seq == IsoReg_eff_res$maxima) %>% 
    mutate(pp_belowPsi_0 = pbeta(psi_0, 1 + num_eff, n - num_eff + 1))
  p_est_max = IsoReg_eff_res$maximum
  j.star <- Obs_opt$level
  pp_belowPsi_0_atMaxRR <- Obs_opt$pp_belowPsi_0
  RespRate_fit_set <- tibble(
    level = Obs_tried$level,
    RespRate_fit = IsoReg_eff_res$fit
  ) 
  export <- Obs_cum %>% 
    select(- RespRate_fit) %>% 
    left_join(RespRate_fit_set)
  
  Obs_current <- Obs_tried %>% 
    filter(level == current_level) %>% 
    mutate(pp_abovePhi0 = pbeta(phi_0 + 0.05, 1 + num_saf, 
                                n - num_saf + 1, lower.tail = F))
  
  # n_current <- Obs_current$n
  current.level <- Obs_current$current_level %>% unique()
  p_current <- Obs_current$p_obs
  q_current <- Obs_current$q_obs
  pp_abovePhi0_current <- Obs_current$pp_abovePhi0
  
  # get the flag to indicate if the level of (j_h + 1) is safe
  if(j_h < J){
    safe_flag_plusOne <- Obs_cum %>% 
      filter(level == j_h + 1) %>% 
      .$safe_flag
  }else{
    safe_flag_plusOne <- F
  }
  earlystop.eff <- F
  earlystop.saf <- F
  
  #############################
  #decide the next dose level #
  #############################
  if(q_current >= phi_U | pp_abovePhi0_current > saf_cut){
    next.level <- current.level -1
    if(next.level == 0) earlystop.saf <- T
    # stop("The trial is early terminated as the lowest dose is
    #                          overly toxic")
    export <- export %>% 
      mutate(safe_flag = ifelse(level >= current.level, F, safe_flag))
  }else{
    # q_current < phi_U
    if(pp_belowPsi_0_atMaxRR >= eff_cut | p_est_max <= psi_0){
      if(j_h == J | safe_flag_plusOne == F){
        if(pp_belowPsi_0_atMaxRR >= eff_cut){
          next.level <- NA
          earlystop.eff <- T
          # stop("The trial is early terminated and we can conclude that 
          #      no dose level in this  meets the criteria for MinED")
        }else{
          # p_est_max <= psi_0
          next.level <- j.star
        }
      }else{
        #j_h != J & safe_flag_plusOne == T
        next.level <- j_h + 1
      }
    }else{
      #pp_belowPsi_0_atMaxRR < eff_cut & # p_est_max > psi_0
      
      if(current.level > j.star){
        next.level <- current.level - 1
      }else if (current.level < j.star){
        next.level <- case_when(
          p_current >= psi_U & 
            current.level != 1 ~ current.level - 1,
          p_current <= psi_L &
            current.level != J ~ current.level + 1,
          TRUE ~ current.level
        )
      }else{
        # current.level == j.star
        next.level <- case_when(
          p_current <= psi_L & 
            j.star == j_h & j_h < J ~ current.level + 1,
          p_current >= psi_U & 
            current.level != 1 ~ current.level - 1,
          TRUE ~ current.level
        )
      }
    }
  }
  
  export <- export %>% 
    mutate(next_level = next.level, 
           j_star = j.star,
           earlystop_saf = earlystop.saf,
           earlystop_eff = earlystop.eff 
    )
  
  return(list(dataset.out = export, next_level = next.level))
}

# Obs_cum <- tibble(
#   level = 1:4,
#   n = c(0, 3, 4, 6),
#   num_saf = c(0, 1, 2, 0),
#   num_eff = c(0, 1, 2, 2),
#   safe_flag = T,
#   current_level = 3
# )
# 
# phi_0 <- 0.4
# phi_1 <- 1.4*phi_0
# phi_U <- get_boundary(phi_0, phi_1)
# 
# psi_0 <- 0.28
# psi_1 <- 0.6*psi_0
# psi_L <- get_boundary(psi_0, psi_1)
# psi_2 <- 1.4*psi_0
# psi_U <- get_boundary(psi_0, psi_2)

# next_dose(Obs_cum, phi_U = phi_U, psi_0 = psi_0,
#           psi_U = psi_U, psi_L = psi_L)
# 
# 
