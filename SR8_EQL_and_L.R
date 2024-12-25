# Program for design parameter (L) and EQL value of X_bar chart with SR8 scheme

rm(list=ls())
ARL0=370.4;  #nominal value of IC ARL
del=0;       # shift size delta
n=5;         # subgroup size
del_max=3;del_min=0  # limits of integration

# f is ARL as function of alpha  (bisection method)
f=function(alp) {
  
  # p1, p2 etc. probabilities of charting statistics lies in different region on chart
  p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
  p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)
  p3=pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
  p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)
  p5=pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  
  # ARL function
  ARL=(-((p1^2 + p1 + 1)*(p6^2 + p6 + 1))/(p2 + p3 + p4 + p5 + p1*p2 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p5*p6 + p1^2*p2 + p1^2*p3 + p1^2*p4 + p1^2*p5 + p1*p6^2 + p1^2*p6 + p2*p6^2 + p3*p6^2 + p4*p6^2 + p5*p6^2 + p1^2*p6^2 + p1*p2*p6^2 + p1^2*p2*p6 + p1*p3*p6^2 + p1^2*p3*p6 + p1*p4*p6^2 + p1^2*p4*p6 + p1*p5*p6^2 + p1^2*p5*p6 + p1^2*p2*p6^2 + p1^2*p3*p6^2 + p1^2*p4*p6^2 + p1^2*p5*p6^2 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 - 1)
  )
  
  c(ARL-ARL0)
}
alp1=0.01;f(alp1)
alp2=0.5;f(alp2)

while(abs(alp1-alp2)>0.00000000001)
{
  alp3=(alp1+alp2)/2 ; f(alp3)
  if(f(alp3)<0)
    alp2=alp3
  else
    alp1=alp3
}

alp=alp3
alp
L=qnorm(1-alp/2)

# f1 is ARL as function of del (i.e. delta)
f1=function(del) {
  p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
  p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)
  p3=pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
  p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)
  p5=pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  (1/(del_max-del_min))*((del^2)*(-((p1^2 + p1 + 1)*(p6^2 + p6 + 1))/(p2 + p3 + p4 + p5 + p1*p2 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p5*p6 + p1^2*p2 + p1^2*p3 + p1^2*p4 + p1^2*p5 + p1*p6^2 + p1^2*p6 + p2*p6^2 + p3*p6^2 + p4*p6^2 + p5*p6^2 + p1^2*p6^2 + p1*p2*p6^2 + p1^2*p2*p6 + p1*p3*p6^2 + p1^2*p3*p6 + p1*p4*p6^2 + p1^2*p4*p6 + p1*p5*p6^2 + p1^2*p5*p6 + p1^2*p2*p6^2 + p1^2*p3*p6^2 + p1^2*p4*p6^2 + p1^2*p5*p6^2 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 - 1)
  ))
}

# integration of f1 w.r.t. del
EQL=integrate(f1,del_min,del_max);

#Print the value of EQL and L
cat("The EQL and L value are:", EQL$value, L, "\n")