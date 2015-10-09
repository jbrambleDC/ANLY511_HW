rand_walk <- function(){
  x <- 0
  count <- 0
  while(x < 20){
    p <- runif(1,0,1)
    if(p <= .5){
      x <- x + 1
    }
    else{
      x <- x - 1
    }
    count <- count + 1
  }
  return(count)
}

sim_walk <- function(){
  t=vector('numeric')
  for (i in 1:100){
    t <- c(t,rand_walk())
  }
  return(t)
}

res <- sim_walk()
mean(res)
median(res)
sd(res)
max(res)
min(res)
quantile(res)
boxplot(res)
