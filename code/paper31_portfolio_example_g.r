# illustrate potential impact of bathtub shape on portfolio
n <- 25
cost <- 10
win <- 100

# fixed probability for point mass
a1 <- 0.5

# parameters of g distribution (bathtub)
a2 <- 0.5
b2 <- 0

# parameters of g distribution (bell)
a3 <- 2
b3 <- 0

# plot densities under consideration for p
if (1 == 0){
  par(mar = c(4.5, 4.5, 1, 1), las = 1)
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 2), xlab = "x", ylab = "density")
  x <- seq(0, 1, by = 0.01)
  lines(x, dg(x, a2, b2), col = 2)
  lines(x, dg(x, a3, b3), col = 2)
}

# compute CDF by simulation
M <- 10 ^ 4
res <- matrix(NA, ncol = 3, nrow = M)
for (i in 1:M){
  
  # probability for a successful trial - every trial has same p
  p1 <- a1
  p2 <- qg(runif(1), alpha = a2, beta = b2)
  p3 <- qg(runif(1), alpha = a3, beta = b3)
  
  if (1 == 0){
    # probability for a successful trial - trial-individual p
    p1 <- rep(a1, n)
    p2 <- qg(runif(n), alpha = a2, beta = b2)
    p3 <- qg(runif(n), alpha = a3, beta = b3)
  }
  
  # generate studies
  suc1 <- rbinom(n, size = 1, prob = p1) 
  suc2 <- rbinom(n, size = 1, prob = p2) 
  suc3 <- rbinom(n, size = 1, prob = p3) 

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
abline(v = n * (p1 * win - cost), lty = 2, col = grey(0.75))

# probability of gain <= 0
apply(res <= 0, 2, mean)

# true CDF for fixed p --> binomial
s <- seq(-500, 2500, by = 1)
r <- (s + cost * n) / win

# make sure r is only integers
r2 <- unique(round(r))

# recompute s from this
s2 <- r2 * win - cost * n

# plot resulting CDFs
t1 <- pbinom(q = r2, size = n, prob = a1)
lines(s2, t1, col = 1, type = "s")

# true CDF for prior g(bathtub) --> g-binomial, i.e. p marginalized out (no closed expression possible)
t2 <- pgbinom(q = r2, n = n, alpha = a2, beta = b2)
lines(s2, t2, col = 1, type = "s")

# true CDF for the beta distribution on p (bell) --> g-binomial
t3 <- pgbinom(q = r, n = n, alpha = a3, beta = b3)
lines(s, t3, col = 1)

# now find alpha such that P(S <= 0) = gamma, where gamma is a probability to experience a loss
# first plot the difference between P(S <= 0) and gamma
gamma <- 0.01
b <- cost * n / win
xs <- seq(0.1, 5, 0.01)
r <- rep(NA, length(xs))
for (i in 1:length(r)){r[i] <- toRootpgbinom(x = xs[i], b = b, n = n, beta = b2, gamma = gamma)}
plot(xs, r, type = "l")
abline(h = 0, col = 2)

alpha0 <- uniroot(f = toRootpgbinom, interval = c(0.1, 10), b = b, n = n, beta = b2, gamma = gamma)$root

# sigma0 = sigmafinal / alpha0
# translated in number of events in a 1:1 randomized trial: d0 = alpha * dfinal
alpha0

# example: dfinal = 380 --> prior needs to be based on 
alpha0 * 380 
# events such that the probability of a loss is <= gamma










