pgbinom <- function(q, n, alpha, beta){
  
  res <- rep(NA, length(q))

  for (j in 1:length(q)){

    if (q[j] <= 0){res[j] <- 0}
    if (q[j] >= n){res[j] <- 1}
    if (is.na(res[j])){
      t1 <- rep(NA, q[j])
      t2 <- t1
    
      for (x in 0:q[j]){
        t1[x + 1] <- choose(n, x)
        t2[x + 1] <- integrate(f = toIntpgbinom, lower = 0, upper = 1, x = x, n = n, alpha = alpha, beta = beta)$value
      }

      res[j] <- sum(t1 * t2, na.rm = TRUE)
    }
  }  
  return(res)
}