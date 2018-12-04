dgUniformNormalTails <- function(x, mu, width, height, thetasuc, sigmafin){
     
  area <- height * width
  sig <- (1 - area) / (height * sqrt(2 * pi))
  
  a <- mu - width / 2
  b <- mu + width / 2
  abar <- pnorm((thetasuc - b) / sigmafin)
  bbar <- pnorm((thetasuc - a) / sigmafin)
    
     z <- qnorm(x)
     
     ind1 <- (x < abar)
     ind2 <- ((x >= abar) & (x <= bbar))
     ind3 <- (x > bbar)
     gUniformNormalTails <- rep(NA, length(x))     
     gUniformNormalTails[ind1] <- sigmafin / dnorm(z[ind1]) * dnorm(thetasuc - sigmafin * z[ind1], mean = b, sd = sig) * (1 - area)
     gUniformNormalTails[ind2] <- sigmafin / dnorm(z[ind2]) * height
     gUniformNormalTails[ind3] <- sigmafin / dnorm(z[ind3]) * dnorm(thetasuc - sigmafin * z[ind3], mean = a, sd = sig) * (1 - area)
     
     res <- list("gUniformNormalTails" = gUniformNormalTails, "abar" = abar, "bbar" = bbar)
     
     return(res)
}