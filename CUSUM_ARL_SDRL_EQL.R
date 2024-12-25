 # Program for ARL, SDRL and EQL values of CUSUM chart

rm(list=ls())
arl_values=c();ARL=c();x_bar=c();SD=c();QL=c()
 
 n = 5;           # subgroup size
 k=0.30;           # reference value, typically shift size / 2
 h=7.0650;         # decision interval)
 mu = 0;          # mean of the process
 sigma = 1;       # standard deviation of the process
 
 H=h*sigma/sqrt(n)
 K=k*sigma/sqrt(n)
 
 # different OOC value of mean mu
 mu1=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3);mu1
 
  for(j in 1:length(mu1))  {
 iterations=100000
  for (i in 1:iterations) {
    C_pos <- 0    # Positive cumulative sum
    C_neg <- 0    # Negative cumulative sum
    t <- 0        # Sample counter
    
    # Simulate until an out-of-control signal
    while (TRUE) {
      #print(arl_values)
      t <- t + 1
     
      # Generate a subgroup mean based on a normal distribution
      x_bar[j] <- mean(rnorm(n, mean = mu1[j], sd = sigma))
      
      # Update the positive and negative CUSUMs
      C_pos <- max(0, C_pos + (x_bar[j] - mu) - K)
      C_neg <- min(0, C_neg + (x_bar[j] - mu) + K)
      
      # Check for out-of-control condition
      if (C_pos > H || abs(C_neg) > H) {
        break
      }
    }
    
    # Store the number of samples until the signal i.e. run length
    arl_values[i] <- t
  }
 ARL[j]=mean(arl_values)    #average run length
 SD[j]=mean(arl_values^2)   # average of square of run length
 QL[j]=ARL[j]*(mu1[j]^2)        # product of ARL and square of mu1 (delta)
  }
 
SDRL=sqrt(SD-ARL^2)  # standard deviation

#Calculation of EQL values
del_min=0    #  lower limits of integration
del_max=3  #    upper limits of integration
n1=15   #  no. of interval
h1 <- (del_max - del_min) / n1  # length of interval


# integral by Trapezoidal rule
integral <- (h1 / 2) * (QL[1]+2*(QL[2]+QL[3]+QL[4]+QL[5]+QL[6]+QL[7]+QL[8]+
QL[9]+QL[10]+QL[11]+QL[12]+QL[13]+QL[14]+QL[15])+QL[16])
integral
EQL=integral/(del_max-del_min);

#Print ARL and SDRL values
res=c(ARL,SDRL)
results=matrix(res,length(mu1),2);results

#Print the value of EQL value
cat("The EQL value is:", EQL, "\n")

write.table(res,"A.csv",sep=",")



  