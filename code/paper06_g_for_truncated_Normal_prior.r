par(las = 1, mfcol = c(2, 2))
x <- seq(-1, 0.2, by = 0.01)
ltys2 <- c(2, 3, 4)




# ---------------------------
# prior densities
# ---------------------------
par(mar = c(9, 4.5, 3, 1))
baseplot()
for (i in 1:length(sigma0.plot)){     
  d1 <- dnorm(x, mean = theta0, sd = sigma0.plot[i])
  lines(x, d1, col = cols2[i], lty = ltys2[i], lwd = 2)
}  

baseplot(tit = "Truncated Normal prior density functions")
for (i in 1:length(sigma0.plot)){     
  d1 <- dnormtrunc(x, mean = theta0, sd = sigma0.plot[i], a = a, b = b)
  d1[is.na(d1)] <- 0
  lines(x, d1, col = cols2[i], lty = ltys2[i], lwd = 2)
}  

#legend(-1, 4, paste("d0 = ", n0.plot, " / alpha = ", disp(alpha2, 2), " / beta = ", disp(beta2, 2), " / \n[a, b] = [log(", exp(a), "); log(", exp(b), ")]", sep = ""), 
#       col = cols2[i] + 1:length(sigma0.plot), lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1.5)
#legend(-1, 5, paste(n0.plot, " / ", disp(alpha2, 2), " / ", disp(beta2, 2), " / [log(", exp(a), "); log(", exp(b), ")]", sep = ""), 
#       col = cols2[i], lwd = 2, lty = ltys2, bty = "n", y.intersp = 1, title = leg.tit1)
legend(-1.06, 5, paste(n0.plot, sep = ""), 
       col = cols2[i], lwd = 2, lty = ltys2, bty = "n", y.intersp = 1, title = leg.tit1)


# ---------------------------
# g and gbar
# ---------------------------
y <- seq(0, 1, by = 0.005)
thetasuc <- log(mdd)

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = expression("Power value densities for varying "*n[0]), xlab = "y", ylab = "density")
for (i in 1:length(sigma0.plot)){     
  u <- dg(y, alpha = alpha2[i], beta = beta2[i])
  lines(y, u, col = cols2[i], lwd = 2, lty = ltys2[i])
}

legend(0.2, 4, paste(n0.plot, " / ", disp(ddcp.normal, 3), sep = ""), 
       col = cols2[i], lwd = 2, lty = ltys2, bty = "n", y.intersp = 1, title = expression("d"[0]*" / PoS:"))

abar.plot <- rep(NA, length(sigma0.plot))
bbar.plot <- abar.plot

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = expression("Power value densities for varying "*n[0]), xlab = "y", ylab = "density")
for (i in 1:length(sigma0.plot)){     
  u <- dgtrunc(y, a = a, b = b, theta0 = theta0, sigma0 = sigma0.plot[i], thetasuc = thetasuc, sigmafin = sigmafin)
  lines(y, u$gtrunc, col = cols2[i], lwd = 2, lty = ltys2[i])
  
  u <- dgtrunc(y, a = a, b = b, theta0 = theta0, sigma0 = sigma0.plot[i], thetasuc = thetasuc, sigmafin = sigmafin)
  
  abar.plot[i] <- u$abar
  bbar.plot[i] <- u$bbar     
}


#legend(-0.05, 4, paste("n0 = ", n0.plot, " / alpha = ", disp(alpha2, 2), " / beta = ", disp(beta2, 2), " / \n[abar, bbar] = [", disp(abar.plot, 2), "; ", disp(bbar.plot, 2), "] / PoS = ", disp(ddcp.trunc.normal, 3), sep = ""), 
#       col = cols2[i] + 1:length(sigma0.plot), lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1.5)
#legend(-0.05, 4, paste(n0.plot, " / ", disp(alpha2, 2), " / ", disp(beta2, 2), " / [", disp(abar.plot, 2), "; ", disp(bbar.plot, 2), "] / ", disp(ddcp.trunc.normal, 3), sep = ""), 
#       col = cols2, lwd = 2, lty = ltys2, bty = "n", y.intersp = 1, title = leg.tit2)
legend(0, 4, paste("[", disp(abar.plot, 2), "; ", disp(bbar.plot, 2), "] / ", disp(ddcp.trunc.normal, 3), sep = ""), 
       col = cols2[i], lwd = 2, lty = ltys2, bty = "n", y.intersp = 1, title = leg.tit2)




