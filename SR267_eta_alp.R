# Program for eta and alpha for X_bar chart with SR267 scheme

rm(list=ls())
ARL = c()
ARL0 = 370.4;  # Nominal value of IC ARL
del = 0;       # shift size
n = 5;         # Subgroup size


f1 = function(eta) {      # eta is optimal design parameters
  f = function(alp) {     # f is ARL as function of alpha (bisection method)
    
    # p1, p2 etc. are probability of charting charting statistics fall in region 1, 2 etc.
    p1 = 1 - pnorm(qnorm(1 - alp / 2, 0, 1) - del * sqrt(n), 0, 1)
    p2 = pnorm(qnorm(1 - alp / 2, 0, 1) - del * sqrt(n), 0, 1) - pnorm(qnorm(1 - alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1)
    p3 = pnorm(qnorm(1 - alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1) - pnorm(-del * sqrt(n), 0, 1)
    p4 = pnorm(-del * sqrt(n), 0, 1) - pnorm(qnorm(alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1)
    p5 = pnorm(qnorm(alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1) - pnorm(qnorm(alp / 2, 0, 1) - del * sqrt(n), 0, 1)
    p6 = pnorm(qnorm(alp / 2, 0, 1) - del * sqrt(n), 0, 1)
    ARL = -((p1 + p1 * p2 + 1) * (p6 + p5 * p6 + 1)) / (p2 + p3 + p4 + p5 + p1 * p3 + p1 * p4 + p1 * p5 + p1 * p6 + p2 * p6 + p3 * p6 + p4 * p6 + p1 * p2 * p3 + p1 * p2 * p4 + p1 * p2 * p5 + p1 * p2 * p6 + p1 * p3 * p6 + p1 * p4 * p6 + p1 * p5 * p6 + p2 * p5 * p6 + p3 * p5 * p6 + p4 * p5 * p6 + p1 * p2 * p3 * p6 + p1 * p2 * p4 * p6 + p1 * p2 * p5 * p6 + p1 * p3 * p5 * p6 + p1 * p4 * p5 * p6 + p1 * p2 * p3 * p5 * p6 + p1 * p2 * p4 * p5 * p6 - 1)
    return(ARL - ARL0)
  }
  
  alp1 = 0.01
  alp2 = 0.1
  
  while(abs(alp1 - alp2) > 1e-10) {
    alp3 = (alp1 + alp2) / 2
    if (f(alp3) < 0) {
      alp2 = alp3
    } else {
      alp1 = alp3
    }
  }
  
  alp = alp3
  
  del = 1   # value of delta for optimal OOC ARL
  
  p1 = 1 - pnorm(qnorm(1 - alp / 2, 0, 1) - del * sqrt(n), 0, 1)
  p2 = pnorm(qnorm(1 - alp / 2, 0, 1) - del * sqrt(n), 0, 1) - pnorm(qnorm(1 - alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1)
  p3 = pnorm(qnorm(1 - alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1) - pnorm(-del * sqrt(n), 0, 1)
  p4 = pnorm(-del * sqrt(n), 0, 1) - pnorm(qnorm(alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1)
  p5 = pnorm(qnorm(alp / 2, 0, 1) * eta - del * sqrt(n), 0, 1) - pnorm(qnorm(alp / 2, 0, 1) - del * sqrt(n), 0, 1)
  p6 = pnorm(qnorm(alp / 2, 0, 1) - del * sqrt(n), 0, 1)
  
  # ARL function
  ARL = -((p1 + p1 * p2 + 1) * (p6 + p5 * p6 + 1)) / (p2 + p3 + p4 + p5 + p1 * p3 + p1 * p4 + p1 * p5 + p1 * p6 + p2 * p6 + p3 * p6 + p4 * p6 + p1 * p2 * p3 + p1 * p2 * p4 + p1 * p2 * p5 + p1 * p2 * p6 + p1 * p3 * p6 + p1 * p4 * p6 + p1 * p5 * p6 + p2 * p5 * p6 + p3 * p5 * p6 + p4 * p5 * p6 + p1 * p2 * p3 * p6 + p1 * p2 * p4 * p6 + p1 * p2 * p5 * p6 + p1 * p3 * p5 * p6 + p1 * p4 * p5 * p6 + p1 * p2 * p3 * p5 * p6 + p1 * p2 * p4 * p5 * p6 - 1)
  
  return(list(ARL = ARL, alp = alp))
}

eta = seq(1, 0, length.out = 1000)
ARL[1] = 370.4  # Starting value for ARL

for (i in 2:length(eta)) {
  cat("Current iteration:", i, "\n")
  
  # Evaluate f1 at current eta
  result = f1(eta[i])
  ARL[i] = result$ARL
  alp = result$alp
  
  # Print current values
  cat("eta:", eta[i], "ARL:", ARL[i], "alp:", alp, "\n")
  
  # Break condition
  if (ARL[i] > ARL[i - 1]) {
    # Print the last iteration's result before the break
    cat("Value of eta and alp:\n")
    cat("eta:", eta[i], "alp:", alp, "\n")
    break
  }
}

