saveRDS(res_scenario_all, "result_forAll_scenarios.rds")

res_scenario_all_2 <- readRDS("result_forAll_scenarios.rds")

N_rep = 60

res_sum <- tibble()

for(i in 1:24){
  
  res_scenario <- res_scenario_all_2[[i]]
  
  prop <- c()
  n_at_true <- c()
  stop_eff_rate <- c()
  stop_saf_rate <- c()
  
  
  for(l in 2:8){
    prop_temp <- res_scenario[[l]]$result_collect %>% 
      count(MinED.true, MinED.est) %>% 
      mutate(prop = n/N_rep) %>% 
      filter(MinED.true == MinED.est) %>% 
      .$prop
    
    prop_temp <- ifelse(length(prop_temp) == 0, 0, prop_temp)
    prop <- c(prop, prop_temp)
    
    stop_eff_temp <- res_scenario[[l]]$result_collect %>% 
      count(stop.eff) %>% 
      mutate(prop = n/N_rep) %>% 
      filter(stop.eff) %>% 
      .$prop
    
    stop_eff_temp <- ifelse(length(stop_eff_temp) == 0, 0, stop_eff_temp)
    stop_eff_rate <- c(stop_eff_rate, stop_eff_temp)
    
    stop_saf_temp <- res_scenario[[l]]$result_collect %>% 
      count(stop.saf) %>% 
      mutate(prop = n/N_rep) %>% 
      filter(stop.saf) %>% 
      .$prop
    stop_saf_temp <- ifelse(length(stop_saf_temp) == 0, 0, stop_saf_temp)
    stop_saf_rate <- c(stop_saf_rate, stop_saf_temp)
    
    n_at_true_temp <- res_scenario[[l]]$result_collect %>% 
      summarise(n_mean = mean(n.forTrue)) %>% 
      .$n_mean
    
    n_at_true <- c(n_at_true, n_at_true_temp)

  }
  
  res_sum_temp <- tibble(
    sceneario = i,
    method = c(2:7, "mu"),
    prop = prop, 
    stop_saf_rate = stop_saf_rate,
    stop_eff_rate = stop_eff_rate,
    n_at_true = n_at_true
  )
  
  res_sum <- rbind(res_sum, res_sum_temp)
}

a <- res_sum %>% 
  group_by(method) %>% 
  summarise(n_mean_true = mean(n_at_true),
            prop_select = mean(prop)
            )
