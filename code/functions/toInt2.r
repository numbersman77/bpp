toInt2 <- function(x, a, b, thetasuc, sigmafin){
     
     f1 <- pnorm((thetasuc - x) / sigmafin)
     f2 <- dunif(x, min = a, max = b)
     
     res <- f1 * f2
     
     return(res)
}