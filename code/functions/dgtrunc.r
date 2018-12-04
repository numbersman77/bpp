dgtrunc <- function(x, a, b, theta0, sigma0, thetasuc, sigmafin){
     
     alpha <- sigmafin / sigma0
     beta <- (thetasuc - theta0) / sigma0
     const <- (pnorm((b - theta0) / sigma0) - pnorm((a - theta0) / sigma0)) ^ (-1)    
     abar <- pnorm((thetasuc - b) / sigmafin)
     bbar <- pnorm((thetasuc - a) / sigmafin)
          
     ind <- ((x >= abar) & (x <= bbar))
     gtrunc <- rep(NA, length(x))     
     gtrunc[ind] <- const * dg(x[ind], alpha, beta)
     
     res <- list("gtrunc" = gtrunc, "abar" = abar, "bbar" = bbar, "const" = const)
     
     return(res)
}