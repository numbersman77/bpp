theta <- seq(-5, 5, by = 0.01)

# PoS
pos <- pnorm((thetasuc - theta) / sqrt(sigmafin ^ 2 + sigma0 ^ 2))

# power
pow <- pnorm((thetasuc - theta) / sigmafin)

par(las = 1)
plot(0, 0, type = "l", xlab = expression(theta[0]*" = log(prior mean)"), ylab = "Power and PoS", xlim = c(-1, 0.5), ylim = c(0, 1))
mtext(text = expression("Power and PoS as a function of "*theta[0]*" for a Normal prior"), side = 3, line = 0.5)
legend("topright", c("Power", "PoS"), col = cols[c(1, 7)], lty = 1, lwd = 2, bty = "n")
abline(h = c(0, 0.5), lty = 2, col = grey(0.5))
abline(v = thetasuc, lty = 2, col = grey(0.5))
lines(theta, pow, col = cols[1], lwd = 2)
lines(theta, pos, col = cols[7], lwd = 2)

text(thetasuc + 0.01, 0.85, expression(theta[suc]), adj = 0)
