# Program for control chart for case study (white millbase process)

rm(list=ls(all=TRUE))
#library("readxl")
expr1 = expression("UCL"['S'['R'['1']]])
expr2 = expression("LCL"['S'['R'['1']]])
expr3 = expression("UCL"['S'['R'['24']]])
expr4 = expression("LCL"['S'['R'['24']]])
expr5 = expression("UCL"['S'['R'['267']]])
expr6 = expression("LCL"['S'['R'['267']]])
expr7 = expression("UIL"['S'['R'['267']]])
expr8 = expression("LIL"['S'['R'['267']]])
n=1; #subgroup size
c4=1; #constant in control limits

# sample observation
x_bar_fut=c(14.56,13.88,13.98,14.50,14.22,14.36,14.46,14.32,14.27,14.20,14.35,13.84,14.43
  ,14.72,14.75,14.27,14.23,14.60,14.25,14.12,14.13,14.47,14.73,14.02,14.11,14.48
  ,14.36,13.70,14.36,14.66,14.04,13.94,13.82,14.11,13.86,13.62,13.66,13.85,13.67
  ,13.80,13.84,13.98,13.40,13.60,13.80,13.66,13.93,13.45,13.90,13.83,13.64,13.62
  ,13.97,13.80,13.70,13.71,13.67)

x_dbar=14.311; # estimate of mean based on first 30 phase I sample
sig_hd=0.261;  # estimate of standard deviation based on first 30 phase I sample
length(x_bar_fut)

######################################################################
alp=0.04572823;eta=0.4116 # design parameters for delta_T=0.6 for SR267 scheme

# k, k_M and k_IM are the control limit constant for SR1,SR24 and SR267 scheme respectively
k=3;k_M=1.8664;k_IM=1.9969  

# control limits for SR1 scheme
UCL_1=x_dbar+3*sig_hd/(c4*sqrt(n));UCL_1
CL=x_dbar;CL
LCL_1=x_dbar-3*sig_hd/(c4*sqrt(n));LCL_1

# control limts for SR24 scheme
UCL_M=x_dbar+k_M*sig_hd/(c4*sqrt(n));UCL_M
LCL_M=x_dbar-k_M*sig_hd/(c4*sqrt(n));LCL_M

# control limits for SR267 scheme
UCL_IM=x_dbar+k_IM*sig_hd/(c4*sqrt(n));UCL_IM
LCL_IM=x_dbar-k_IM*sig_hd/(c4*sqrt(n));LCL_IM

#inner limits for SR267 scheme
UIL_IM=x_dbar+eta*k_IM*sig_hd/(2*c4*sqrt(n));UIL_IM
LIL_IM=x_dbar-eta*k_IM*sig_hd/(2*c4*sqrt(n));LIL_IM
##################################################################################
d1=57 # d1 is no. of sample observation

sam_fut=seq(1,d1,1) # generate sequence of 1 of length d1
U1=rep(UCL_1,d1)    # repeat the value of UCL_1 of length d1
C=rep(CL,d1)        # repeat the value of CL of length d1
L1=rep(LCL_1,d1)    # repeat the value of LCL_1 of length d1
U_IM=rep(UCL_IM,d1)  # repeat the value of UCL_IM of length d1
UI_IM=rep(UIL_IM,d1)  # repeat the value of UIL_IM of length d1
LI_IM=rep(LIL_IM,d1)  # repeat the value of LIL_IM of length d1
L_IM=rep(LCL_IM,d1)   # repeat the value of LCL_IM of length d1
U_M=rep(UCL_M,d1)     # repeat the value of UCL_M of length d1
L_M=rep(LCL_M,d1)     # repeat the value of LCL_M of length d1

#####################################################################################

# plot the control chart
plot(sam_fut,x_bar_fut,type="o",pch=1,lty=1,xlab="Sample Number",ylab="Batch Weight (X)",xlim=c(1,70),
     ylim=c(13.2,15.2),cex.lab=0.8,cex.axis=0.8,cex=0.8)

#legend of control chart
legend("topright",legend=c(expr1,expr2,expr3,expr4,expr5,expr6,expr7,expr8),
       col=c("black","black","blue","blue","red","red","red","red")
       ,lty=c(1,1,4,4,1,1,2,2),cex=0.5)

# plot different color lines (control limits) on control chart
lines(sam_fut,U1,lty=1,col="black")
lines(sam_fut,L1,lty=1,col="black")
lines(sam_fut,C,lty=1,col="black")
lines(sam_fut,U_IM,lty=1,col="red")
lines(sam_fut,L_IM,lty=1,col="red")
lines(sam_fut,LI_IM,lty=2,col="red")
lines(sam_fut,UI_IM,lty=2,col="red")
lines(sam_fut,U_M,lty=4,col="blue")
lines(sam_fut,L_M,lty=4,col="blue")

