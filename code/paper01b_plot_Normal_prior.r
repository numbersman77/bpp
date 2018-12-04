plot(0, 0, type = "n", xlim = c(-1, 0.3), ylim = c(0, 1.5), xlab = "", ylab = "density", main = "")
title("Normal prior", line = 1)

abline(h = 0, lty = 3)
xlabs <- seq(0, 5, by = 0.1)
axis(1, at = log(xlabs), labels = xlabs, line = 4.5)
mtext("true hazard ratio", side = 1, line = 6.5)
mtext(expression(theta*" = log(true hazard ratio)"), side = 1, line = 2.2)

thetas <- seq(-7, 5, by = 0.01)
lines(thetas, dnorm(thetas, mean = log(lambda0), sd = sqrt(4 / n0)), col = 1, lwd = 2)
