ptrue <- list()

#  all doses are safety doses #
# increasing
ptrue[[1]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.3, 0.45, 0.5, 0.65,0.75)) # increasing med: d1

ptrue[[2]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.15, 0.3, 0.45, 0.5,0.65)) # increasing med: d2

ptrue[[3]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.1, 0.15, 0.3, 0.45,0.5)) # increasing med: d3

ptrue[[4]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.01, 0.1, 0.15, 0.3,0.45)) # increasing med: d4

ptrue[[5]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.01, 0.05, 0.1, 0.15,0.30)) # increasing med: d5
# umbrella
ptrue[[6]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.3, 0.45, 0.5, 0.25,0.15)) # umbrella shape med: d1

ptrue[[7]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.15, 0.3, 0.45, 0.25,0.15)) # umbrella shape med: d2

ptrue[[8]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.1, 0.15, 0.3, 0.15,0.05)) # umbrella shape med: d3

ptrue[[9]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                   c(0.05, 0.1, 0.15, 0.3, 0.15)) # umbrella shape med: d4

ptrue[[10]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                    c(0.01, 0.09, 0.1, 0.15,0.30)) # increasing med: d5
# plateau
ptrue[[11]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                    c(0.01, 0.15, 0.30, 0.30,0.30)) # increasing med: d3

ptrue[[12]] = rbind(c(0.01, 0.05, 0.10, 0.15, 0.30),
                    c(0.15, 0.30, 0.30, 0.30,0.30)) # increasing med: d3
############################################################################
# the last one dose is over toxic
# increasing
ptrue[[13]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.3, 0.45, 0.5, 0.65,0.75)) # increasing med: d1

ptrue[[14]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.15, 0.3, 0.45, 0.5,0.65)) # increasing med: d2

ptrue[[15]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.1, 0.15, 0.3, 0.45,0.5)) # increasing med: d3

ptrue[[16]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.01, 0.1, 0.15, 0.3,0.45)) # increasing med: d4

# umbrella
ptrue[[17]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.3, 0.45, 0.5, 0.25,0.15)) # umbrella shape med: d1

ptrue[[18]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.15, 0.3, 0.45, 0.25,0.15)) # umbrella shape med: d2

ptrue[[19]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.1, 0.15, 0.3, 0.15,0.05)) # umbrella shape med: d3

ptrue[[20]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.05, 0.1, 0.15, 0.3, 0.15)) # umbrella shape med: d4
# plateau
ptrue[[21]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.01, 0.15, 0.30, 0.30, 0.30)) # increasing med: d3

ptrue[[22]] = rbind(c(0.01, 0.05, 0.15, 0.30, 0.45),
                    c(0.15, 0.30, 0.30, 0.30, 0.30)) # increasing med: d2
# 
ptrue[[23]] = rbind(c(0.1, 0.30, 0.45, 0.50, 0.55),
                    c(0.15, 0.30, 0.30, 0.30, 0.30)) # increasing med: d2

ptrue[[24]] = rbind(c(0.30, 0.40, 0.45, 0.50, 0.55),
                    c(0.30, 0.45, 0.50, 0.30, 0.20)) # increasing med: d1
