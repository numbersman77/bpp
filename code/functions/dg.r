dg <- function(x, alpha, beta){
     
     z <- qnorm(x)
     
     nom <- alpha * dnorm(beta - alpha * z)
     den <- dnorm(z)
     
     res <- nom / den
     
     return(res)
}