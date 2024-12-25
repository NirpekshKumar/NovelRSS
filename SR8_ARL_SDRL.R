# Program for ARL and SDRL value of X_bar chart with SR8 scheme

rm(list=ls())
ARL0=370.4; # nominal value of IC ARL
del=0;      # shift sizes delat
n=5;        # subgroup size

# f is ARL as function of alp (bisection method)
f=function(alp) {
  p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
  p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)
  p3=pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
  p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)
  p5=pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
  p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
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

alp; L=qnorm(1-alp/2);  # design parametrs

# Different values of shift size delta  
del=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3);

# probabilities of charting statistics fall in different regions
p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)
p3=pnorm(qnorm(1-alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)
p5=pnorm(qnorm(alp/2,0,1)/2-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)

# ARL function
ARL=(-((p1^2 + p1 + 1)*(p6^2 + p6 + 1))/(p2 + p3 + p4 + p5 + p1*p2 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p5*p6 + p1^2*p2 + p1^2*p3 + p1^2*p4 + p1^2*p5 + p1*p6^2 + p1^2*p6 + p2*p6^2 + p3*p6^2 + p4*p6^2 + p5*p6^2 + p1^2*p6^2 + p1*p2*p6^2 + p1^2*p2*p6 + p1*p3*p6^2 + p1^2*p3*p6 + p1*p4*p6^2 + p1^2*p4*p6 + p1*p5*p6^2 + p1^2*p5*p6 + p1^2*p2*p6^2 + p1^2*p3*p6^2 + p1^2*p4*p6^2 + p1^2*p5*p6^2 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 - 1)
)

# ARL^2 function
ARL2=((2*(p1 + p2 + p3 + p4 + p5 + p6 + 2*p1*p2 + 2*p1*p3 + 2*p1*p4 + 2*p1*p5 + 4*p1*p6 + 2*p2*p6 + 2*p3*p6 + 2*p4*p6 + 2*p5*p6 + 3*p1^2*p2 + 3*p1^2*p3 + 2*p1^3*p2 + 3*p1^2*p4 + 2*p1^3*p3 + p1^4*p2 + 3*p1^2*p5 + 2*p1^3*p4 + p1^4*p3 + 7*p1*p6^2 + 7*p1^2*p6 + 2*p1^3*p5 + p1^4*p4 + 2*p1*p6^3 + 3*p2*p6^2 + 2*p1^3*p6 + p1^4*p5 + p1*p6^4 + 2*p2*p6^3 + 3*p3*p6^2 + p1^4*p6 + p2*p6^4 + 2*p3*p6^3 + 3*p4*p6^2 + p3*p6^4 + 2*p4*p6^3 + 3*p5*p6^2 + p4*p6^4 + 2*p5*p6^3 + p5*p6^4 + 2*p1^2 + 2*p6^2 + 12*p1^2*p6^2 + 4*p1^2*p6^3 + 4*p1^3*p6^2 + 2*p1^2*p6^4 + 2*p1^4*p6^2 + 6*p1*p2*p6^2 + 6*p1^2*p2*p6 + 4*p1*p2*p6^3 + 6*p1*p3*p6^2 + 6*p1^2*p3*p6 + 4*p1^3*p2*p6 + 2*p1*p2*p6^4 + 4*p1*p3*p6^3 + 6*p1*p4*p6^2 + 6*p1^2*p4*p6 + 4*p1^3*p3*p6 + 2*p1^4*p2*p6 + 2*p1*p3*p6^4 + 4*p1*p4*p6^3 + 6*p1*p5*p6^2 + 6*p1^2*p5*p6 + 4*p1^3*p4*p6 + 2*p1^4*p3*p6 + 2*p1*p4*p6^4 + 4*p1*p5*p6^3 + 4*p1^3*p5*p6 + 2*p1^4*p4*p6 + 2*p1*p5*p6^4 + 2*p1^4*p5*p6 + 9*p1^2*p2*p6^2 + 6*p1^2*p2*p6^3 + 9*p1^2*p3*p6^2 + 6*p1^3*p2*p6^2 + 3*p1^2*p2*p6^4 + 6*p1^2*p3*p6^3 + 9*p1^2*p4*p6^2 + 4*p1^3*p2*p6^3 + 6*p1^3*p3*p6^2 + 3*p1^4*p2*p6^2 + 3*p1^2*p3*p6^4 + 6*p1^2*p4*p6^3 + 9*p1^2*p5*p6^2 + 2*p1^3*p2*p6^4 + 4*p1^3*p3*p6^3 + 6*p1^3*p4*p6^2 + 2*p1^4*p2*p6^3 + 3*p1^4*p3*p6^2 + 3*p1^2*p4*p6^4 + 6*p1^2*p5*p6^3 + 2*p1^3*p3*p6^4 + 4*p1^3*p4*p6^3 + 6*p1^3*p5*p6^2 + p1^4*p2*p6^4 + 2*p1^4*p3*p6^3 + 3*p1^4*p4*p6^2 + 3*p1^2*p5*p6^4 + 2*p1^3*p4*p6^4 + 4*p1^3*p5*p6^3 + p1^4*p3*p6^4 + 2*p1^4*p4*p6^3 + 3*p1^4*p5*p6^2 + 2*p1^3*p5*p6^4 + p1^4*p4*p6^4 + 2*p1^4*p5*p6^3 + p1^4*p5*p6^4 + 4*p1*p2*p6 + 4*p1*p3*p6 + 4*p1*p4*p6 + 4*p1*p5*p6))/(p2 + p3 + p4 + p5 + p1*p2 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p5*p6 + p1^2*p2 + p1^2*p3 + p1^2*p4 + p1^2*p5 + p1*p6^2 + p1^2*p6 + p2*p6^2 + p3*p6^2 + p4*p6^2 + p5*p6^2 + p1^2*p6^2 + p1*p2*p6^2 + p1^2*p2*p6 + p1*p3*p6^2 + p1^2*p3*p6 + p1*p4*p6^2 + p1^2*p4*p6 + p1*p5*p6^2 + p1^2*p5*p6 + p1^2*p2*p6^2 + p1^2*p3*p6^2 + p1^2*p4*p6^2 + p1^2*p5*p6^2 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 - 1)^2
)

SD=sqrt(ARL2+ARL-(ARL^2))  # standard deviation
UCL=qnorm(1-alp/2,0,1)/sqrt(n);CL=0;LCL=qnorm(alp/2,0,1)/sqrt(n) #control limits

D=c(ARL, SD)
t=length(del)
mat=matrix(D,t,2);

#Print ARL and SD values
mat  
write.table(mat,"A.csv",sep=",")
