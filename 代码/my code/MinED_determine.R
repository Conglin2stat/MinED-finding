
get_level <- function(level, target, fit, saf_flag){
  dist_min <- tibble(level, target, fit) %>% 
    mutate(distance_abs = abs(fit - target),
           sign = ifelse(fit >= target, "positive", "negative")
    ) %>% 
    slice_min(order_by = distance_abs) 
  
  dist_min_pos <- dist_min %>% 
    filter(sign == "positive")%>% 
    slice_min(order_by = level) %>% 
    .$level
  
  dist_min_neg <- dist_min %>% 
    filter(sign == "negative") %>% 
    slice_max(order_by = level) %>% 
    .$level
  
  
  len_pos <- length(dist_min_pos)
  len_neg <- length(dist_min_neg)
  
  # target_level <- case_when(
  #   len_pos == 1 & len_neg != 1 ~ dist_min_pos,
  #   len_pos != 1 & len_neg == 1 ~ dist_min_neg,
  #   len_pos == 1 & len_neg == 1 & saf_flag ~ dist_min_neg,
  #   len_pos == 1 & len_neg == 1 & !saf_flag ~ dist_min_pos
  # )
  # 
  
  if(len_pos == 1){
    if(len_neg == 1){
      if(saf_flag){
        target_level <- dist_min_neg
      }else{
        # saf_flag is FALSE
        target_level <- dist_min_pos
      }
    }else{
      # len_neg != 1
      target_level <- dist_min_pos
    }
  }else{
    # len_pos != 1
    target_level <- dist_min_neg
  }
  
  return(target_level)
}

# -------------------------------------------------------------------------
# main macro for selecting MinED based on both true and observed prob
# -------------------------------------------------------------------------

final_select <- function(Obs_final){
  
  TargetLevel_true <- Obs_final %>% 
    mutate(MTD_true = get_level(level, phi_0, q_true, saf_flag = T)) %>% 
    filter(level <= MTD_true) %>% 
    mutate(MinED_true = get_level(level, psi_0, p_true, saf_flag = F))
  
  MTD.true <- TargetLevel_true$MTD_true %>% unique()
  MinED.true <- TargetLevel_true$MinED_true %>% unique()
  n.forTrue <- TargetLevel_true %>% 
    filter(level == MinED_true) %>% 
    .$n
  
  stop_saf <- Obs_final$earlystop_saf %>% unique()
  stop_eff <- Obs_final$earlystop_eff %>% unique()
  
  Obs_tried <- Obs_final %>% 
    filter(n != 0) %>% 
    mutate(num_treated = sum(n),
           num_cohort = Stop_at_cohort
           )
  
  if(!stop_saf & !stop_eff){
    TargetLevel_est <- Obs_tried %>% 
      mutate(q_obs = num_saf/n,
             ToxRate_fit = pava(q_obs, w = n),
             j_T = get_level(level, phi_0, ToxRate_fit, saf_flag = T)
      ) %>% 
      filter(level <= j_T) %>% 
      mutate(MinED_est = get_level(level, psi_0, RespRate_fit, saf_flag = F)
      )
    
    j_T = TargetLevel_est$j_T %>% unique()
    MinED.est = TargetLevel_est$MinED_est %>% unique()
  }else{
    j_T <- NA
    MinED.est <- NA
  }

  level.tried <- nrow(Obs_tried)
  num.treated <- Obs_tried$num_treated %>% unique()
  num.cohort <- Obs_tried$num_cohort %>% unique()
  
  
  return(list(MTD.true = MTD.true, MinED.true = MinED.true, j_T = j_T,
              MinED.est = MinED.est, level.tried = level.tried, 
              num.cohort = num.cohort, num.treated = num.treated,
              n.forTrue = n.forTrue, stop.saf = stop_saf, stop.eff = stop_eff))
}


# Example -----------------------------------------------------------------

# result_eachtrial <- final_select(Obs_final) %>%
#   as_tibble() %>%
#   add_column(id_trial = 1, .before = "MTD.true")
