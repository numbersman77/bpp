x <- seq(-1, 0.2, by = 0.01)
ltys2 <- c(2, 3, 4)



# ---------------------------
# prior densities
# ---------------------------
par(mar = c(9, 4.5, 3, 1))

baseplot(ymax = 4, tit = "Uniform prior density functions")
for (i in 1:length(a.plot)){     
  d1 <- dunif(x, min = a.plot[i], max = b.plot[i])
  #lines(x, d1, col = cols2[i], lty = ltys2[i], lwd = 2)
  jiggle <- 0.05
  segments(min(x), jiggle * (i - 1), a.plot[i], jiggle * (i - 1), col = cols2[i], lty = ltys2[i], lwd = 2)
  segments(a.plot[i], max(d1), b.plot[i], max(d1), col = cols2[i], lty = ltys2[i], lwd = 2)
  segments(b.plot[i], jiggle * (i - 1), max(x), jiggle * (i - 1), col = cols2[i], lty = ltys2[i], lwd = 2)
}  

legend(-1, 4.2, paste("[log(", disp(exp(a.plot), 3), "); log(", disp(exp(b.plot), 3), ")]", sep = ""), 
       col = cols2, lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1, title = "[a, b]:")



# ---------------------------
# g and gbar
# ---------------------------
y <- seq(0.005, 1 - 0.005, by = 0.005)
thetasuc <- log(mdd)

abar.plot <- rep(NA, length(sigma0.plot))
bbar.plot <- abar.plot

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = "Power value densities for Uniform prior", xlab = "y", ylab = "density")
for (i in 1:length(a.plot)){     
  u <- dgunif(y, a = a.plot[i], b = b.plot[i], thetasuc = thetasuc, sigmafin = sigmafin)
  #lines(y, u$gunif, col = cols2[i], lwd = 2)
  
  abar.plot[i] <- u$abar
  bbar.plot[i] <- u$bbar     

  jiggle <- 0.05
  
  segments(min(y), jiggle * (i - 1), u$abar, jiggle * (i - 1), col = cols2[i], lty = ltys2[i], lwd = 2)
  ind <- (y >= u$abar)
  lines(y[ind], u$gunif[ind], col = cols2[i], lwd = 2, lty = ltys2[i])
}

legend(0.1, 4, paste("[", disp(abar.plot, 2), "; ", disp(bbar.plot, 2), "] / ", disp(ddcp.unif, 3), sep = ""), 
       col = cols2[i], lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1, title = leg.tit2)

#
