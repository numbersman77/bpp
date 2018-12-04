qg <- function(p, alpha, beta, tol = 10^-20){
  
  res <- rep(NA, length(p))
  for (i in 1:length(p)){
    res[i] <- uniroot(toRootqg, lower = 0, upper = 1, tol = tol,  p = p[i], alpha = alpha, beta = beta)$root
  }
  return(res)
  
}