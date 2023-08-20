library(tidyverse)
library(Iso)

IsoReg_2.sided <- function(y, w){
  
  y_length <- length(y)
  id <-  1:y_length
  
  fit_first <- pava(y, w)
  fit_matrix <- fit_first
  for (j in 1:y_length) {
    y1 <- y[id <= j]
    w1 <- w[id <= j]
    y1_fit <- pava(y1, w1)
    y2 <- y[id > j]
    w2 <- w[id > j]
    y2_fit <- pava(y2, w2, decreasing = T)
    fit_temp <- c(y1_fit, y2_fit)
    fit_matrix <-  cbind(fit_matrix, fit_temp)
  }
  
  colnames(fit_matrix) <- 0:y_length
  summary <- tibble(
    id = 0:y_length,
    SSE = crossprod(fit_matrix - y) %>% apply(2, sum),
    maximum = fit_matrix %>% apply(2, max),
    maxima = fit_matrix %>% apply(2, which.max)
  )
  
  optimal <- summary %>% 
    slice_min(order_by = SSE) %>% 
    slice_max(order_by = maximum) %>% 
    arrange(maxima) %>% # when the max have tie, take the one with smallest id.
    slice_head(n=1)
  
  maximum <- optimal$maximum %>% as.numeric()
  maxima <- optimal$maxima %>% as.numeric()
  
  id_opt <- optimal$id
  fitted_opt <- fit_matrix[, colnames(fit_matrix) == id_opt] 
  
  return(list(fit = fitted_opt, weight = w, maximum = maximum, maxima = maxima))
}

# y <- c(1:10, rep(10, 5), 9:1) + rnorm(24)
# w <- c(rep(1,10), rep(2, 5), rep(3, 5), rep(1, 4))
# 
# IsoReg_2.sided(y, w)
# plot(y)
# points(PAVA_double.sided(y)$fit, col = "red")
# lines(PAVA_double.sided(y)$fit)
