toInt1 <- function(x, thetasuc, sigmafin, theta0, sigma0, a, b){
     
     f1 <- pnorm((thetasuc - x) / sigmafin)
     f2 <- dnormtrunc(x, mean = theta0, sd = sigma0, a = a, b = b)
          
     res <- f1 * f2
     
     return(res)
}