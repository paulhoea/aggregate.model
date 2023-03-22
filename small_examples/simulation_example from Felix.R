##############################################
############## Simulation Example
############# Demonstrating that single equations in a loop are identical to a SVAR with known restrictions
####################################

########### DGP (do not use for estimation)

set.seed(12345)
time <- 100
epsx <- rnorm(time, 0, 1)
epsy <- rnorm(time, 0, 1)
epsz <- rnorm(time, 0, 1)



x <- matrix(NA, time, 1)
y <- matrix(NA, time, 1)
z <- matrix(NA, time, 1)

x[1] <- epsx[1]
y[1] <- epsy[1]
z[1] <- epsz[1]

### any coefficient that is changed here also needs to be manually changed in the A and B matrices below (sorry it's not automated!):
for (i in 2:time){
  x[i] <-                                                     epsx[i]
  y[i] <- 0.5*x[i] +           + 0.5*y[i-1]                 + epsy[i]
  z[i] <- 0.8*x[i] + 0.3*y[i]  + 0.4*y[i-1] + 0.3*z[i-1]    + epsz[i]
}

############################################

#####equivalence to using a matrix approach in DGP:
# (needs to match the simulation DGP loop above)
A <- matrix(c(1, 0, 0,
              -0.5, 1, 0,
              -0.8, -0.3, 1), 3, 3, byrow = T)

B <- matrix(c(0, 0, 0,
              0, 0.5, 0,
              0, 0.4, 0.3), 3, 3, byrow = T)

Ainv <- solve(A)
AinvB <- solve(A)%*%B

#for first set of observations
xyz <- AinvB%*%c(x[1], y[1], z[1]) + Ainv %*% c(epsx[2], epsy[2], epsz[2])

#check they're the same:
cbind(xyz, c(x[2], y[2], z[2]))


#################################################
######## Estimation Part: Estimate the Models (single equation)
#################################################

library(lgarch)
library(gets)

L1.x <- glag(x)
L1.y <- glag(y)
L1.z <- glag(z)

### models assuming that we know the correct relevant variables (i.e. we impose restrictions in VAR setting)
# Could specify this that if A matrix includes regressor (i.e. has non-zero value), then contemporaneous should be included in model


#### set estimation sample (estimate on first 50 observations, then simulate for the remainder)
time_est <- c(1:50)

## Model for X
mx <- arx(x[time_est], mc=TRUE, mxreg=NULL)

#
# kk <- mxy[time_est,]
# kk <- cbind(1,kk)
# kk <- kk[complete.cases(kk),]
#
# ll <- y[time_est]
# ll <- ll[-1]
#
# solve(t(kk) %*% kk) %*% t(kk) %*% ll

## Model for Y
mxy <- cbind(x, L1.y)
colnames(mxy) <- c("x", "L1.y")
my <- arx(y[time_est], ar=NULL, mxreg=mxy[time_est,])

## Model for Z
mxz <- cbind(x, y, L1.y, L1.z)
colnames(mxz) <- c("x", "y", "L1.y", "L1.z")
mz <- arx(z[time_est,], ar=NULL, mxreg=mxz[time_est,])

#######################################################
### Extract coefficients and build simulation part

#### Building Contemporaneous Coefficient Matrix
### could perhaps automate this - if contemporaneous is included, then include a non-zero value in this matrix.

Ahat <- matrix(0, 3, 3)
row.names(Ahat) <- c("x", "y", "z")
colnames(Ahat) <- c("x", "y", "z")

Ahat[row.names(Ahat)=="x",colnames(Ahat)=="x"] <- 1 #x row

Ahat[row.names(Ahat)=="y",colnames(Ahat)=="x"] <- (-1)*coefficients(my)[names(coefficients(my))=="x"] #y row, x coefficient
Ahat[row.names(Ahat)=="y",colnames(Ahat)=="y"] <- 1 #y row

Ahat[row.names(Ahat)=="z",colnames(Ahat)=="x"] <- (-1)*coefficients(mz)[names(coefficients(mz))=="x"] #z row, x coefficient
Ahat[row.names(Ahat)=="z",colnames(Ahat)=="y"] <- (-1)*coefficients(mz)[names(coefficients(mz))=="y"] #z row, y coefficient
Ahat[row.names(Ahat)=="z",colnames(Ahat)=="z"] <- 1 #z row

#compare to DGP:
Ahat #estimated
A #DGP matrix


### Building Lagged Coefficient Matrix
Bhat <- matrix(0, 3, 3)
row.names(Bhat) <- c("x", "y", "z")
colnames(Bhat) <- c("L1.x", "L1.y", "L1.z")

Bhat[row.names(Bhat)=="y",colnames(Bhat)=="L1.y"] <- coefficients(my)[names(coefficients(my))=="L1.y"] #y row

Bhat[row.names(Bhat)=="z",colnames(Bhat)=="L1.y"] <- coefficients(mz)[names(coefficients(mz))=="L1.y"] #z row, y lag coef
Bhat[row.names(Bhat)=="z",colnames(Bhat)=="L1.z"] <- coefficients(mz)[names(coefficients(mz))=="L1.z"] #z row, z lag coef

#compare to DGP
Bhat
B

#Intercept/deterministic terms (here there is just a constant)

MChat <- c(coefficients(mx)["mconst"], coefficients(my)["mconst"], coefficients(mz)["mconst"])


######### Simulate this in two ways: using the matrices as well as a simple loop for single equations
### Equation is: A Y = BY_{t-1} + MC + eps
### solve: Y = A^-1 BY_{t-1} + A^-1 MC + A^-1 eps

Ahatinv <- solve(Ahat)
AhatinvBhat <- Ahatinv %*% Bhat
AhatinvMChat <-  Ahatinv %*% MChat


##### Simulate some errors using the estimated covariance matrix
sim_err_x <- rnorm(time, 0, mx$sigma2)
sim_err_y <- rnorm(time, 0,  my$sigma2)
sim_err_z <- rnorm(time, 0,  mz$sigma2)

sim_err_v <- cbind(sim_err_x, sim_err_y, sim_err_z)

sim_xyz <- matrix(NA, time, 3) #simulation output from matrices
colnames(sim_xyz) <- c("x.sim", "y.sim", "z.sim")

sim_xyz_loop <- matrix(NA, time, 3) #simulation output from loop (for comparison)
colnames(sim_xyz_loop) <- c("x.sim", "y.sim", "z.sim")


time_sim_start <- 51 #simulation sample
sim_xyz[(time_sim_start-1),] <- cbind(x,y,z)[time_sim_start-1,]
sim_xyz_loop[(time_sim_start-1),] <-  cbind(x,y,z)[time_sim_start-1,]


### could probably also compute this in a non-loop-y way!
for (i in time_sim_start:time){

  ### Matrix approach
  sim_xyz[i,] <- AhatinvBhat %*% sim_xyz[i-1,] + AhatinvMChat + Ahatinv%*%sim_err_v[i,]

  ### Equivalent to single equation loop
  sim_xyz_loop[i,"x.sim"] <- MChat[1] + sim_err_v[i,1]
  sim_xyz_loop[i,"y.sim"] <- (-1)*Ahat[2,"x"] * sim_xyz_loop[i,"x.sim"] + Bhat[2,"L1.y"]*sim_xyz_loop[i-1,"y.sim"] +   MChat[2] + sim_err_v[i,2]
  sim_xyz_loop[i,"z.sim"] <- (-1)*Ahat[3,"x"] * sim_xyz_loop[i,"x.sim"] +   (-1)*Ahat[3,"y"] * sim_xyz_loop[i,"y.sim"]  +  Bhat[3,"L1.y"]*sim_xyz_loop[i-1,"y.sim"] +  Bhat[3,"L1.z"]*sim_xyz_loop[i-1,"z.sim"] +  MChat[3] + sim_err_v[i,3]


}


# Show Equivalence
par(mfrow=c(3,1))

plot(x, type="l")
lines(sim_xyz[,"x.sim"], col="red", lwd=2) #matrix
points(sim_xyz_loop[,"x.sim"], col="red", lwd=2) #single equation loop
abline(v=max(time_est), lty=2)

plot(y, type="l")
lines(sim_xyz[,"y.sim"], col="blue", lwd=2)
points(sim_xyz_loop[,"y.sim"], col="blue", lwd=2)
abline(v=max(time_est), lty=2)

plot(z, type="l")
lines(sim_xyz[,"z.sim"], col="purple", lwd=2)
points(sim_xyz_loop[,"z.sim"], col="purple", lwd=2)
abline(v=max(time_est), lty=2)

#they're the same!
cbind(sim_xyz[,1], sim_xyz_loop[,1], sim_xyz[,2], sim_xyz_loop[,2])













