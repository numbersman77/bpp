toInt3 <- function(x, mu, width, height, thetasuc, sigmafin){
     
     f1 <- pnorm((thetasuc - x) / sigmafin)
     f2 <- dUniformNormalTails(x, mu, width, height)
     
     res <- f1 * f2
     
     return(res)
}