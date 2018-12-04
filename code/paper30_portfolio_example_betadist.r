# illustrate potential impact of bathtub shape on portfolio
n.studies <- 25
cost <- 10
win <- 100

# fixed probability for point mass
a1 <- 0.5

# parameters of beta distribution (bathtub)
a2 <- 0.3
b2 <- 0.3

# parameters of beta distribution (bell)
a3 <- 2
b3 <- 2

# plot densities under consideration for p
if (1 == 0){
  par(mar = c(4.5, 4.5, 1, 1), las = 1)
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 2), xlab = "x", ylab = "density")
  x <- seq(0, 1, by = 0.01)
  lines(x, dbeta(x, a2, b2), col = 2)
  lines(x, dbeta(x, a3, b3), col = 2)
}

# compute CDF
M <- 10 ^ 4
res <- matrix(NA, ncol = 3, nrow = M)
for (i in 1:M){
  
  # probability for a successful trial
  p1 <- a1
  p2 <- rbeta(1, shape1 = a2, shape2 = b2)
  p3 <- rbeta(1, shape1 = a3, shape2 = b3)
  
  # generate studies
  suc1 <- rbinom(n.studies, size = 1, prob = p1) 
  suc2 <- rbinom(n.studies, size = 1, prob = p2) 
  suc3 <- rbinom(n.studies, size = 1, prob = p3) 

  g1 <- sum(win * suc1 - cost)
  g2 <- sum(win * suc2 - cost)
  g3 <- sum(win * suc3 - cost)

  res[i, ] <- c(g1, g2, g3)
}

par(las = 1)
plot(0, 0, type = "n", xlim = c(0, max(res)), xlab = "gain", ylab = "probability", ylim = c(0, 1), main = "distribution of gain for various portfolio strategies")
legend("topleft", c(paste("p = ", a1, sep = ""), "bathtub", "bell shaped"), col = 2:4, lty = 1, bty = "n", lwd = 3)
lines(ecdf(res[, 1]), col = 2, verticals = TRUE, pch = NA, lwd = 3)
lines(ecdf(res[, 2]), col = 3, verticals = TRUE, pch = NA, lwd = 3)
lines(ecdf(res[, 3]), col = 4, verticals = TRUE, pch = NA, lwd = 3)
abline(h = p1, lty = 2, col = grey(0.75))
abline(v = n.studies * (p1 * win - cost), lty = 2, col = grey(0.75))

# probability of gain <= 0
apply(res <= 0, 2, mean)

# true CDF for fixed p --> binomial
s <- seq(-500, 2500, by = 1)
r <- (s + cost * n.studies) / win
t1 <- pbinom(q = r, size = n.studies, prob = a1)
lines(s, t1, col = 1)

# true CDF for the beta distribution on p (bathtub) --> beta-binomial
library(VGAM)
t2 <- pbetabinom.ab(q = r, size = n.studies, shape1 = a2, shape2 = b2)
lines(s, t2, col = 1)

# true CDF for the beta distribution on p (bell) --> beta-binomial
t3 <- pbetabinom.ab(q = r, size = n.studies, shape1 = a3, shape2 = b3)
lines(s, t3, col = 1)




