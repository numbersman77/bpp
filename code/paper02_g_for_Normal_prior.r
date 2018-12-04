par(las = 1)
hist(power_from_prior, main = "",
     xlab = expression("Value of power T("*theta[suc]*"|"*theta*")"), 
     ylab = "density", freq = FALSE, ylim = c(0, 10))
title(expression("T("*theta[suc]*"|"*theta*") for "*theta*" sampled"), line = 2.1)
title(expression(" from Normal prior"), line = 1)
lines(y, u, col = 1, lwd = 2)

# mean of power values
ddcp.h <- mean(power_from_prior)

points(ddcp.h, 0, pch = 19, cex = 1.5)

int <- c(0.2, 0.9)
pexa <- pg(int[2], alpha = alpha0, beta = beta0) - pg(int[1], alpha = alpha0, beta = beta0)


