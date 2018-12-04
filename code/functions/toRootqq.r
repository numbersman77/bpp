toRootqg <- function(x, p, alpha, beta){
  
  res <- pg(x, alpha, beta) - p
  return(res)
  
}