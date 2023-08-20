
prob_true = tibble(
  level = 1:5,
  q_true = c(0.01, 0.05, 0.10, 0.15, 0.30),
  p_true = c(0.15, 0.3, 0.45, 0.25,0.15))
N.max = 60
start_level = 1
cohort.size = 3
eff_cut = 0.99
saf_cut = 0.95
target_tox = 0.3
upper_ratio_tox = 1.4
target_eff = 0.3
upper_ratio_eff = 1.3
lower_ratio_eff = 0.7

get_boundary(target_eff, upper_ratio_eff*target_eff)
get_boundary(target_eff, lower_ratio_eff*target_eff)

num_trial <- 10

source("trial_sim.R")
source("MinED_determine.R")


get_OC <- function(num_trial, prob_true, N.max, 
       start_level = 1, cohort.size = 3,
       eff_cut = 0.99, saf_cut = 0.95,
       target_tox, upper_ratio_tox = 1.3,
       target_eff, upper_ratio_eff = 1.3, lower_ratio_eff = 0.7
){
  
  result_collect <- NULL
  
  Obs_gather <- list()
  
  for(k in 1:num_trial){
    cat("Trial", k, ":", "\n", sep = " ")
    Obs_temp <- trial_simulate(prob_true = prob_true, start_level = start_level,
                 cohort.size = cohort.size, N.max = N.max,
                 target_tox = target_tox, upper_ratio_tox = upper_ratio_tox,
                 target_eff = target_eff, upper_ratio_eff = upper_ratio_eff, 
                 lower_ratio_eff = lower_ratio_eff, 
                 eff_cut = eff_cut, saf_cut = saf_cut)
    
    Obs_gather[[k]] <- Obs_temp
      result_temp <- final_select(Obs_temp) %>%
        as_tibble() %>%
        add_column(id_trial = k, .before = "MTD.true")
      
      result_collect <- rbind(result_collect, result_temp)
  }
  
  export <- result_collect %>% 
    mutate(
      cohort.size = cohort.size,
      eff_cut = eff_cut,
      saf_cut = saf_cut,
      target_tox = target_tox,
      target_eff = target_eff,
      upper_ratio_tox = upper_ratio_tox,
      upper_ratio_eff = upper_ratio_eff,
      lower_ratio_eff = lower_ratio_eff,
)
  return(list(Obs_gather = Obs_gather, result_collect = result_collect))
}

source("trial_sim.R")
source("MinED_determine.R")




prob_true = tibble(
  level = 1:5,
  q_true = c(0.01, 0.05, 0.10, 0.15, 0.30),
  p_true = c(0.15, 0.3, 0.45, 0.25,0.15))

N_rep <- 100
#  2 ----------------------------------------------------------------------

rm(next_dose)
source("next_dose_retire2.R")
OC_res_2 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)
OC_res_2$result_collect %>% count(MinED.est)

#  3 ----------------------------------------------------------------------
rm(next_dose)
source("next_dose_retire3.R")
OC_res_3 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)
OC_res_3$result_collect %>% count(MinED.est)

#  4 ----------------------------------------------------------------------
rm(next_dose)
source("next_dose_retire4.R")
OC_res_4 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)
OC_res_4$result_collect %>% count(MinED.est)

# 5 -----------------------------------------------------------------------

rm(next_dose)
source("next_dose_retire5.R")
OC_res_5 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)

OC_res_5$result_collect %>% count(MinED.est)

# 6 -----------------------------------------------------------------------

rm(next_dose)
source("next_dose_retire6.R")
OC_res_6 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)

OC_res_6$result_collect %>% count(MinED.est)

# 7 -----------------------------------------------------------------------

rm(next_dose)
source("next_dose_retire7.R")
OC_res_7 <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)

OC_res_7$result_collect %>% count(MinED.est)

# mu ----------------------------------------------------------------------

rm(next_dose)
source("next_dose_mu.R")
OC_res_mu <- get_OC(num_trial = N_rep, prob_true = prob_true,
                   N.max = 60, target_tox = 0.3, target_eff = 0.3)

OC_res_mu$result_collect %>% count(MinED.est)
