# fix mu. Choose width such that the prior is centered at theta0 on the log-scale
width.plot <- 2 * (theta0 - log(c(0.5, 0.55, 0.6)))
height.plot <- 0.8 / width.plot

# DDCP for Uniform via numerical integration
ddcp.pessimistic <- rep(NA, length(width.plot))

for (i in 1:length(ddcp.pessimistic)){
  ddcp.pessimistic[i] <- integrate(toInt3, lower = -Inf, upper = Inf, 
                    mu = theta0, width = width.plot[i], height = height.plot[i], 
                    thetasuc = thetasuc, sigmafin = sigmafin)$value
}