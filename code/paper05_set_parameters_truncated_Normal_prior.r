n0.plot <- c(10, 100, 500)
sigma0.plot <- sqrt(4 / n0.plot)


# DDCPs for Normal prior
ddcp.normal <- pnorm((log(mdd) - theta0) / sqrt(sigmafin ^ 2 + sigma0.plot ^ 2))

# set parameters for truncated Normal prior
a <- log(0.5)
b <- log(1)

alpha2 <- sigmafin / sigma0.plot
beta2 <- (thetasuc - theta0) / sigma0.plot

c0 <- 0.8
bound2 <- qnorm(c0) * (alpha2 ^ 2 - 1) / alpha2

# DDCP for truncated Normal via numerical integration
ddcp.trunc.normal <- rep(NA, length(sigma0.plot))

for (i in 1:length(ddcp.trunc.normal)){
     ddcp.trunc.normal[i] <- integrate(toInt1, lower = a, upper = b, thetasuc = thetasuc, sigmafin = sigmafin, 
               theta0 = theta0, sigma0 = sigma0.plot[i], a = a, b = b)$value
}
