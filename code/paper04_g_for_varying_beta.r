alpha.plot <- 1
betas.plot <- seq(-1, 1, by = 0.5)
par(las = 1)
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = expression("Power value densities for varying "*psi), xlab = "y", ylab = "density")
mtext(bquote(paste(tau, " fixed at ", .(alpha.plot))), side = 3, line = 0.25)
abline(h = 0, lty = 3)

for (i in 1:length(betas.plot)){
     u <- alpha.plot * exp(-0.5 * (betas.plot[i] - alpha.plot * qnorm(y)) ^ 2 + 0.5 * qnorm(y) ^ 2)
     lines(y, u, col = cols[i], lwd = 2, lty = ltys2[i])
}

legend(0.2, 4, betas.plot, col = cols[1:length(betas.plot)], lwd = 2, lty = ltys2[1:length(alphas.plot)], bty = "n", title = expression(psi*" = "))

