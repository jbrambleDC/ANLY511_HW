sim_arrival = function(n){
  vec <- vector()
  x <- runif(n,0,60) - runif(n,0,60)
  for (i in 1:n){
    vec[i] <- max(x[i],0)
  }
  return(vec)
}

sim_result <- sim_arrival(1000)
print(mean(sim_result))
res <- ecdf(sim_result)



