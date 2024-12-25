# Program for ARL and SDRL value of X_bar chart with SR26 scheme

rm(list=ls())
n=5;     #subgroup size
eta=0.3203203;  #design parameter
alp=0.06721324; L=qnorm(1-alp/2); #design parameters

#Different values of shifts (delta)
del=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3);

# p1, p2 etc. are probability of charting charting statistics fall in region 1, 2 etc.
p1=1-pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)
p2=pnorm(qnorm(1-alp/2,0,1)-del*sqrt(n),0,1)-pnorm(qnorm(1-alp/2,0,1)*eta-del*sqrt(n),0,1)
p3=pnorm(qnorm(1-alp/2,0,1)*eta-del*sqrt(n),0,1)-pnorm(-del*sqrt(n),0,1)
p4=pnorm(-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)*eta-del*sqrt(n),0,1)
p5=pnorm(qnorm(alp/2,0,1)*eta-del*sqrt(n),0,1)-pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)
p6=pnorm(qnorm(alp/2,0,1)-del*sqrt(n),0,1)

# ARL function
ARL=(-((p1 + p1*p2 + 1)*(p6 + p5*p6 + 1))/(p2 + p3 + p4 + p5 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p1*p2^2 + p5^2*p6 + p1*p2^2*p6 + p1*p5^2*p6 + p1*p2*p3 + p1*p2*p4 + p1*p2*p5 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 + p2*p5*p6 + p3*p5*p6 + p4*p5*p6 + p1*p2*p3*p6 + p1*p2*p4*p6 + p1*p2*p5*p6 + p1*p3*p5*p6 + p1*p4*p5*p6 + p1*p2*p5^2*p6 + p1*p2^2*p5*p6 + p1*p2*p3*p5*p6 + p1*p2*p4*p5*p6 - 1)
)

#ARL^2 function
ARL2=((2*(p1 + p2 + p3 + p4 + p5 + p6 + 2*p1*p2 + 2*p1*p3 + 2*p1*p4 + 2*p1*p5 + 4*p1*p6 + 2*p2*p6 + 2*p3*p6 + 2*p4*p6 + 2*p5*p6 + 2*p1*p2^2 + p1^2*p3 + p1^2*p4 + p1^2*p5 + p1*p6^2 + p1^2*p6 + p2*p6^2 + p3*p6^2 + p4*p6^2 + 2*p5^2*p6 + 2*p1^2*p2^2 + p1^2*p2^3 + 2*p5^2*p6^2 + p5^3*p6^2 + 2*p1^2*p2*p3 + 2*p1^2*p2*p4 + 2*p1^2*p2*p5 + 2*p1*p2*p6^2 + 4*p1*p2^2*p6 + 2*p1^2*p2*p6 + 2*p1*p3*p6^2 + 2*p1^2*p3*p6 
          + 2*p1*p4*p6^2 + 2*p1^2*p4*p6 + 2*p1*p5*p6^2 + 4*p1*p5^2*p6 + 2*p1^2*p5*p6 + 2*p2*p5*p6^2 + 2*p3*p5*p6^2 + 2*p4*p5*p6^2 + p1^2*p2^2*p3 + p1^2*p2^2*p4 + p1^2*p2^2*p5 + 2*p1*p2^2*p6^2 + 5*p1^2*p2^2*p6 + p1^2*p3*p6^2 + 2*p1^2*p2^3*p6 + p1^2*p4*p6^2 + 5*p1*p5^2*p6^2 + 2*p1^2*p5^2*p6 + 2*p1*p5^3*p6^2 + p2*p5^2*p6^2 + p3*p5^2*p6^2 + p4*p5^2*p6^2 + 2*p1*p2*p3 + 2*p1*p2*p4 + 2*p1*p2*p5 + 6*p1*p2*p6 
          + 4*p1*p3*p6 + 4*p1*p4*p6 + 6*p1*p5*p6 + 2*p2*p5*p6 + 2*p3*p5*p6 + 2*p4*p5*p6 + 2*p1^2*p2^2*p6^2 + p1^2*p2^3*p6^2 + 2*p1^2*p5^2*p6^2 + p1^2*p5^3*p6^2 + 4*p1*p2*p3*p6 + 4*p1*p2*p4*p6 + 8*p1*p2*p5*p6 + 4*p1*p3*p5*p6 + 4*p1*p4*p5*p6 + p1^2*p2^2*p3*p6^2 + p1^2*p2^2*p4*p6^2 + 2*p1*p2^2*p5^2*p6^2 + 4*p1^2*p2*p5^2*p6^2 + 4*p1^2*p2^2*p5*p6^2 + 2*p1^2*p2^2*p5^2*p6 + 2*p1^2*p2*p5^3*p6^2 + p1^2*p3*p5^2*p6^2 
          + 2*p1^2*p2^3*p5*p6^2 + p1^2*p4*p5^2*p6^2 + 2*p1*p2*p3*p6^2 + 4*p1^2*p2*p3*p6 + 2*p1*p2*p4*p6^2 + 4*p1^2*p2*p4*p6 + 4*p1*p2*p5*p6^2 + 4*p1*p2*p5^2*p6 + 4*p1*p2^2*p5*p6 + 4*p1^2*p2*p5*p6 + 4*p1*p3*p5*p6^2 + 2*p1^2*p3*p5*p6 + 4*p1*p4*p5*p6^2 + 2*p1^2*p4*p5*p6 + 4*p1^2*p2^2*p5^2*p6^2 + p1^2*p2^2*p5^3*p6^2 + p1^2*p2^3*p5^2*p6^2 + 2*p1^2*p2*p3*p6^2 + 2*p1^2*p2^2*p3*p6 + 2*p1^2*p2*p4*p6^2 + 2*p1^2*p2^2*p4*p6 
          + 6*p1*p2*p5^2*p6^2 + 4*p1*p2^2*p5*p6^2 + 4*p1^2*p2*p5^2*p6 + 6*p1^2*p2^2*p5*p6 + 2*p1*p2*p5^3*p6^2 + 2*p1*p3*p5^2*p6^2 + 2*p1^2*p3*p5*p6^2 + 2*p1^2*p2^3*p5*p6 + 2*p1*p4*p5^2*p6^2 + 2*p1^2*p4*p5*p6^2 + p1^2*p2^2*p3*p5^2*p6^2 + p1^2*p2^2*p4*p5^2*p6^2 + 2*p1*p2*p3*p5^2*p6^2 + 4*p1^2*p2*p3*p5*p6^2 + 2*p1^2*p2^2*p3*p5*p6 + 2*p1*p2*p4*p5^2*p6^2 + 4*p1^2*p2*p4*p5*p6^2 + 2*p1^2*p2^2*p4*p5*p6 + 4*p1*p2*p3*p5*p6 
          + 4*p1*p2*p4*p5*p6 + 2*p1^2*p2*p3*p5^2*p6^2 + 2*p1^2*p2^2*p3*p5*p6^2 + 2*p1^2*p2*p4*p5^2*p6^2 + 2*p1^2*p2^2*p4*p5*p6^2 + 4*p1*p2*p3*p5*p6^2 + 4*p1^2*p2*p3*p5*p6 + 4*p1*p2*p4*p5*p6^2 + 4*p1^2*p2*p4*p5*p6))/(p2 + p3 + p4 + p5 + p1*p3 + p1*p4 + p1*p5 + p1*p6 + p2*p6 + p3*p6 + p4*p6 + p1*p2^2 + p5^2*p6 + p1*p2^2*p6 + p1*p5^2*p6 + p1*p2*p3 + p1*p2*p4 + p1*p2*p5 + p1*p2*p6 + p1*p3*p6 + p1*p4*p6 + p1*p5*p6 
                                                                                                                                                                                                                        + p2*p5*p6 + p3*p5*p6 + p4*p5*p6 + p1*p2*p3*p6 + p1*p2*p4*p6 + p1*p2*p5*p6 + p1*p3*p5*p6 + p1*p4*p5*p6 + p1*p2*p5^2*p6 + p1*p2^2*p5*p6 + p1*p2*p3*p5*p6 + p1*p2*p4*p5*p6 - 1)^2)
SD=sqrt(ARL2+ARL-(ARL^2)) # standard deviation

D=c(ARL,SD) 
t=length(del)
mat=matrix(D,t,2);mat;L  #print ARL, SD and L values
write.table(mat,"A.csv",sep=",")

