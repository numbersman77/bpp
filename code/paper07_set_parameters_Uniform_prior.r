# fix a. Choose b such that the uniform prior is centered at theta0 on the log-scale
a.plot <- log(c(0.5, 0.55, 0.6))
b.plot <- 2 * theta0 - a.plot

# DDCP for Uniform via numerical integration
ddcp.unif <- rep(NA, length(a.plot))

for (i in 1:length(ddcp.unif)){
  ddcp.unif[i] <- integrate(toInt2, lower = a.plot[i], upper = b.plot[i], 
                    a = a.plot[i], b = b.plot[i], thetasuc = thetasuc, sigmafin = sigmafin)$value
}