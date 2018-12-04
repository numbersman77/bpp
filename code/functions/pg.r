pg <- function(x, alpha, beta, num.int = FALSE){
  
  if (num.int == TRUE){
    res <- rep(0, length(x))
    for (i in 1:length(x)){
      if (x[i] > 0){res[i] <- integrate(dg, lower = 0, upper = x[i], alpha = alpha, beta = beta)$value}
    }
  }
  
  if (num.int == FALSE){
    nom <- beta - alpha * qnorm(x)  
    res <- 1 - pnorm(nom)
  }
  
  return(res)
}