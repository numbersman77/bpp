toIntpgbinom <- function(p, x, n, alpha, beta){
  
  res <- p ^ x * (1 - p) ^ (n - x) * dg(x = p, alpha = alpha, beta = beta)
  return(res)
}