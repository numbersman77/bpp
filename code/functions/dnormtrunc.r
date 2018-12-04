dnormtrunc <- function(x, mean, sd, a, b){
  
  ind1 <- ((x >= a) & (x <= b))
  
  # definition from wikipedia :-(
  nom <- dnorm((x - mean) / sd)  / sd
  den <- pnorm((b - mean) / sd) - pnorm((a - mean) / sd)
  
  res <- rep(NA, length(x))
  res[ind1] <- nom[ind1] / den
  
  return(res)
}