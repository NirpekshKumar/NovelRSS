# Program for design parameters (eta, L) and EQL value of X_bar chart with SR267 scheme

rm(list=ls())
n=5;          # subgroup size
del_max=3;del_min=0;   #limits of integration

# eta and alp are design parameters for optimal chart at specified delta
eta=0.5575576;
alp=0.05948372;
L=qnorm(1-alp/2);

# f1 is ARL as function of del (i.e. delta)
f1=function(del) {
  # p1, p2 etc. are probability of charting charting statistics fall in region 1, 2 etc.
  p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
  p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)*eta-del*sqrt(n),0,1)
  p3=pnorm(qnorm(1-alp/2,0,1)*eta-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
  p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)*eta-del*sqrt(n),0,1)
  p5=pnorm(qnorm(alp/2,0,1)*eta-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  (1/(del_max-del_min))*((del^2)*(-((p1 + p1*p2 + 1)*(p6 + p5*p6 + 1))/(p2 + p3 + p4 + p5 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p1*p2*p3 + p1*p2*p4 + p1*p2*p5 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 + p2*p5*p6 + p3*p5*p6 + p4*p5*p6 + p1*p2*p3*p6 + p1*p2*p4*p6 + p1*p2*p5*p6 + p1*p3*p5*p6 + p1*p4*p5*p6 + p1*p2*p3*p5*p6 + p1*p2*p4*p5*p6 - 1)
  ))
}

# integration of f1 w.r.t. del
EQL=integrate(f1,del_min,del_max);

#Print the value of EQL, L and eta
cat("The EQL, L and eta value are:", EQL$value, L, eta, "\n")