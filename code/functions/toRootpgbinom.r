toRootpgbinom <- function(x, b, n, beta, gamma){
  
  res <- gamma - pgbinom(q = b, n = n, alpha = x, beta = beta)
  return(res)
}