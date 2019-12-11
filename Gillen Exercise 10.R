
rn <- 0.1
rn_with_drug <- -0.1
rm <- 0.1
rm_with_drug <- 0.5*rm
K <- 1000000
N0 <- 99
M0 <- 1
timesteps <- 500

N <- c()
M <- c()
M[1] <- M0
N[1] <- N0

for (t in 1:(timesteps-1)){
  if( t < 150 ){
    M[t + 1] <- M[t] + rm*M[t]*(1 - (N[t] + M[t])/K)
    N[t + 1] <- N[t] + rn*N[t]*(1 - (N[t] + M[t])/K)
  } else {
    M[t + 1] <- M[t] + rm_with_drug*M[t]*(1 - (N[t] + M[t])/K)
    N[t + 1] <- N[t] + rn_with_drug*N[t]*(1 - (N[t] + M[t])/K)
  }
}

plot(1:timesteps, N, col = "black", xlab = "Time", ylab = "Population", type = "line")
lines(1:timesteps, M, col = "red")
