x <- seq(-1, 0.2, by = 0.01)
ltys2 <- c(2, 3, 4)



# ---------------------------
# prior densities
# ---------------------------
par(mar = c(9, 4.5, 3, 1))

baseplot(ymax = 4, tit = "pessimistic prior density functions")
for (i in 1:length(width.plot)){     
  d1 <- dUniformNormalTails(x, mu = theta0, width = width.plot[i], height = height.plot[i])
  lines(x, d1, col = cols2[i], lty = ltys2[i], lwd = 2)
}  

legend(-1, 4.2, paste("w = ", disp(width.plot, 3), " / h = ", disp(height.plot, 3), sep = ""), 
       col = cols2, lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1)



# ---------------------------
# g and gbar
# ---------------------------
y <- seq(0.005, 1 - 0.005, by = 0.005)
thetasuc <- log(mdd)

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 4), main = "Power value densities for pessimistic prior", xlab = "y", ylab = "density")
for (i in 1:length(width.plot)){     
  u <- dgUniformNormalTails(y, mu = theta0, width = width.plot[i], height = height.plot[i], thetasuc = thetasuc, sigmafin = sigmafin)
  lines(y, u$gUniformNormalTails, col = cols2[i], lwd = 2, lty = ltys2[i])
  
  abar.plot[i] <- u$abar
  bbar.plot[i] <- u$bbar     
}

legend(0.1, 4, paste("[", disp(abar.plot, 2), "; ", disp(bbar.plot, 2), "] / ", disp(ddcp.pessimistic, 3), sep = ""), 
       col = cols2, lwd = 2, lty = ltys2[i], bty = "n", y.intersp = 1, title = leg.tit2)














#
