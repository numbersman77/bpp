pnormtrunc <- function(x, mean, sd, a, b){
  
  ind1 <- (x < a)
  ind2 <- ((x >= a) & (x <= b))
  ind3 <- (x > b)
  
  # definition from wikipedia :-(
  nom <- (pnorm((x[ind2] - mean) / sd) - pnorm((a - mean) / sd))
  den <- (pnorm((b - mean) / sd) - pnorm((a - mean) / sd))

  res <- rep(NA, length(x))
  res[ind1] <- 0
  res[ind2] <- nom / den
  res[ind3] <- 1
  
  return(res)
}