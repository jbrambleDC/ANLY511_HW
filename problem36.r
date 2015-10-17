take_max <- function(x){
  for (i in 1:length(x)){
    x[i] <- max(x[i],1)
  }
  return(x)
}

mybivariate <- function(n,mx,my,sigx,sigy,rho){
  Z1 <- rnorm(n)
  Z2 <- rnorm(n)
  X <- sigx*Z1 + mx
  X <- take_max(X)
  Y <- rho*sigy*Z1 + sqrt(1-rho^2)*sigy*Z2
  z <- matrix(c(X,Y),ncol=2)
  plot(z[,1],z[,2],asp=1)
  grid(col=1)
  return(z)
}
res <- mybivariate(100,0,0,1,1,.5)
print(res)
mean(res)
sd(res)
qqnorm(res)
hist(res)
