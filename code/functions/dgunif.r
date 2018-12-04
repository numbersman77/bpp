dgunif <- function(x, a, b, thetasuc, sigmafin){
     
     abar <- pnorm((thetasuc - b) / sigmafin)
     bbar <- pnorm((thetasuc - a) / sigmafin)
    
     z <- qnorm(x)
     
     ind <- ((x >= abar) & (x <= bbar))
     gunif <- rep(0, length(x))     
     gunif[ind] <- sigmafin / ((b - a) * dnorm(z[ind]))
     
     res <- list("gunif" = gunif, "abar" = abar, "bbar" = bbar)
     
     return(res)
}