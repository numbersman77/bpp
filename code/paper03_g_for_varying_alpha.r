alphas.plot <- c(0.1, seq(0.5, 2, by = 0.5))
beta.plot <- 0
par(las = 1)
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = expression("Power value densities for varying "*tau), xlab = "y", ylab = "density")
mtext(bquote(paste(psi, " fixed at ", .(beta.plot))), side = 3, line = 0.25)
abline(h = 0, lty = 3)

for (i in 1:length(alphas.plot)){
     u <- alphas.plot[i] * exp(-0.5 * (beta.plot - alphas.plot[i] * qnorm(y)) ^ 2 + 0.5 * qnorm(y) ^ 2)
     lines(y, u, col = cols[i], lwd = 2, lty = ltys2[i])
}

legend(0.2, 4, alphas.plot, col = cols[1:length(alphas.plot)], lwd = 2, lty = ltys2[1:length(alphas.plot)], bty = "n", title = expression(tau*" = "))
