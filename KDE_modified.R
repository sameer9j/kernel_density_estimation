#Including relevant libraries
library(dplyr)
library(scatterplot3d)
library(mvtnorm)

#Setting a seed value
set.seed(1000)

#Generating two normal distributions of var 1, centered at -2 and 2 respectively

#Creating a function to calculate the probability value at each specified x, distribution assume mean = 0, standard deviation =1
d_norm <- function(x)
{
  return(1/sqrt(2*pi)*exp(-0.5*x^2))
}

#Generating 1000 random variables each with SD =1, and mean = 2, and -2 respectively
v1 = rnorm(1000,2,1)
v2 = rnorm(1000,-2,1)

#Creating relevant variables
kf= as.data.frame(matrix(NA,length(v1)*2,1))
kf = as.data.frame(c(v1,v2))
names(kf) = "all"

h = 0.6
kv = NA
ivl = .1
a=0
sq_n = seq(-8,8,ivl)

#Creating a univariate KDE estimator function, h value is an input to this function
kde_est = function(h)
{
  for(i in sq_n){
    a=a+1
    kf$val = as.matrix((i - kf$all)/h)
    kf$dnorm = as.matrix(d_norm(kf$val))
    kv[a] = apply(kf,2,sum)[3]/(nrow(kf)*h)
  }
  return(kv)
}

#Plotting distribution for different values of h
plot(sq_n, kde_est(0.1), type="l", lwd=1, ylab = "Density",xlab = "Sample.x", col = "blue")
lines(sq_n, kde_est(0.5), type="l", lwd=2, ylab = "Density",xlab = "Sample.x", col="red")
lines(sq_n, kde_est(1), type="l", lwd=1, ylab = "Density",xlab = "Sample.x", col = "dark blue")
lines(sq_n, kde_est(2.5), type="l", lwd=1, ylab = "Density",xlab = "Sample.x", col = "green")
lines(sq_n, kde_est(10), type="l", lwd=1.5, ylab = "Density",xlab = "Sample.x", col = "pink")

#Generating a bivariate distribution
bv1 = rmvnorm(100,mean= c(2,2))
bv2 = rmvnorm(100,mean= c(-2,-2))

#Creating relevant variables
kfb= as.data.frame(matrix(NA,nrow(bv1)*2,2))
kfb= as.data.frame(rbind(bv1,bv2))
names(kfb) = c("x","y")

h = 1
ivl = 1
a=0
b=0 

#Generating the sequence for x and y axis
sq_n = seq(-8,8,ivl)
kv = as.data.frame(matrix(NA,length(sq_n)*length(sq_n),3))
names(kv) = c("x", "y","dnorm")

#Creating a bivariate KDE estimator function, h value is an input to this function
kde_est_bv = function(h)
{
  for(i in sq_n){
    for(j in sq_n)
    {
      b=b+1
      kfb$val_x = as.matrix((i - kfb$x)/h)
      kfb$dnorm_x = as.matrix(d_norm(kfb$val_x))
      
      kfb$val_y = as.matrix((j - kfb$y)/h)
      kfb$dnorm_y = as.matrix(d_norm(kfb$val_y))
      
      kv$x[b] = i
      kv$y[b] = j
      
      #As 0 covariance between the two variables, the bivariate distribution is equal to multiplication of univariate distributions
      kv$dnorm[b] = (apply(kfb,2,sum)[4]*apply(kfb,2,sum)[6])/(nrow(kfb)*h)
    }
  }
  return(kv)
}

#Plotting distribution for different values of h
kvn = as.data.frame(kde_est_bv(0.5))
scatterplot3d(x= kvn$x, y = kvn$y, z= kvn$dnorm, xlab = "x axis", ylab = "y axis", zlab = "Density", angle=80,pch = 16, color="steelblue",grid = FALSE, box = FALSE)

kvn = as.data.frame(kde_est_bv(1))
scatterplot3d(x= kvn$x, y = kvn$y, z= kvn$dnorm, xlab = "x axis", ylab = "y axis", zlab = "Density", angle=80,pch = 16, color="steelblue",grid = FALSE, box = FALSE)

kvn = as.data.frame(kde_est_bv(2))
scatterplot3d(x= kvn$x, y = kvn$y, z= kvn$dnorm, angle=80,pch = 16, color="red",grid = FALSE, box = FALSE)

kvn = as.data.frame(kde_est_bv(5))
scatterplot3d(x= kvn$x, y = kvn$y, z= kvn$dnorm, angle=80,pch = 16, color="green",grid = FALSE, box = FALSE)