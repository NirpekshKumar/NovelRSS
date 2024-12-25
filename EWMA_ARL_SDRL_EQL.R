# Program for ARL, SDRL and EQL of EWMA chart

rm(list=ls())
run_length=c();arl_values=c();subgroup=c();ARL=c();SD=c();X_bar=c(); QL=c()

# Parameters
lambda_ewma <- 0.05     # EWMA smoothing parameter
L <- 2.4901             # Control limit multiplier
mu0 <- 0               # In-control process mean
sigma <- 1             # In-control process standard deviation
n <- 5                 # Subgroup size

mu1=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3);mu1

iterations=100000       # Number of iteration

for(j in 1:length(mu1))  {

# Adjust the standard deviation for subgroup size
sigma_xbar <- sigma / sqrt(n)

# EWMA control limits with subgroup size adjustment
UCL <- mu0 + L * sigma_xbar * sqrt(lambda_ewma / (2 - lambda_ewma))
LCL <- mu0 - L * sigma_xbar * sqrt(lambda_ewma / (2 - lambda_ewma))

# Function to simulate run length
for (i in 1:iterations) {
  Z_t <- mu0  # Initialize EWMA statistic
  t <- 0
  while (TRUE) {
    t <- t + 1
    # Generate subgroup and calculate sample mean
    
    X_bar[j] <- mean(rnorm(n, mean = mu1[j], sd = sigma))
    
    # Update EWMA statistic
    Z_t <- lambda_ewma * X_bar[j] + (1 - lambda_ewma) * Z_t
    
    # Check if EWMA statistic falls outside control limits
    if (Z_t > UCL || Z_t < LCL) {
      break
    }
  }
  run_length[i]=t
}

# Calculate ARL
ARL[j] <- mean(run_length)     #average run length
SD[j]=mean(run_length^2)       # average of square of run length
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

write.table(results,"A.csv",sep=",")

