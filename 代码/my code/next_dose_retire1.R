source("double_sided_IsoReg.R")
source("get_boundary.R")
library(tidyverse)

next_dose <- function(Obs_cum, phi_U, psi_0, psi_L, psi_U,
                      eff_cut){
  
  J <- max(Obs_cum$level)
  
  Obs_tried  <- Obs_cum %>% 
    filter(n != 0) %>% 
    arrange(level) %>% 
    mutate(q_obs = num_saf/n,
           p_obs = num_eff/n,
           seq = 1:n()
    )
  j_h <- max(Obs_tried$level) # the highest level that has been tried
  
  IsoReg_eff_res<- IsoReg_2.sided(y = Obs_tried$p_obs, w = Obs_tried$n)
  j_star <- Obs_tried %>% 
    filter(seq == IsoReg_eff_res$maxima) %>% 
    .$level
  p_est_max = IsoReg_eff_res$maximum
  RespRate_fit <- tibble(
    level = Obs_tried$level,
    RR_fit = IsoReg_eff_res$fit
  )
  export <- Obs_cum %>% 
    left_join(RespRate_fit)
  
  Obs_current <- Obs_tried %>% 
    filter(level == current_level)
  # n_current <- Obs_current$n
  current.level <- Obs_current$current_level %>% unique()
  p_current <- Obs_current$p_obs
  q_current <- Obs_current$q_obs
  
  # get the flag to indicate if the next one dose level is safe
  if(current.level < J){
    safe_flag_plusOne <- Obs_cum %>% 
      filter(level == current.level + 1) %>% 
      .$safe_flag
  }
  
  #############################
  #decide the next dose level #
  #############################
  if(q_current >= phi_U){
    next.level <- current.level -1
    if(next.level == 0) stop("The trial is early terminated as the lowest dose is
                             overly toxic")
    export <- export %>% 
      mutate(safe_flag = ifelse(level >= current.level, F, safe_flag))
  }else{
    # q_current < phi_U
    if(p_est_max < psi_0){
      if(j_h == J | safe_flag_plusOne == F){
        stop("The trial is early terminated and we can conclude that 
             no dose level in this  meets the criteria for MinED")
      }else{
        #j_h != J & safe_flag_plusOne == T
        next.level <- current.level + 1
      }
    }else{
      #p_est_max >=psi_0
      if(current.level >= j_star){
        next.level <- ifelse(current.level == 1, 1, 
                             current.level - 1
        )
      }else{
        # current.level < j_star
        next.level <- case_when(
          p_current >= psi_U & 
            current.level != 0 ~ current.level - 1,
          p_current <= psi_L &
            current.level != J~ current.level + 1,
          TRUE ~ current.level
        )
      }
    }
  }
  
  export <- export %>% 
    mutate(next_level = next.level)
  
  return(list(dataset.out = export, next_level = next.level))
}

Obs_cum <- tibble(
  level = 1:4,
  n = c(0, 3, 4, 6),
  num_saf = c(0, 1, 2, 0),
  num_eff = c(0, 1, 2, 2),
  safe_flag = T,
  current_level = 3
)

phi_0 <- 0.4
phi_1 <- 1.4*phi_0
phi_U <- get_boundary(phi_0, phi_1)

psi_0 <- 0.2
psi_1 <- 0.7*psi_0
psi_L <- get_boundary(psi_0, psi_1)
psi_2 <- 1.3*psi_0
psi_U <- get_boundary(psi_0, psi_2)

next_dose(Obs_cum, phi_U = phi_U, psi_0 = psi_0,
          psi_U = psi_U, psi_L = psi_L)
# 
# 
