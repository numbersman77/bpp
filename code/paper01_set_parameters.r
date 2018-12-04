# set basic parameters
hr0 <- 0.75
power0 <- 0.8
nfin <- 380     # corresponds to necessary number of events for HR = 0.75, alpha = 0.05, beta = 0.2, and 1:1 rando
sigmafin <- sqrt(4 / nfin)

# from this, compute MDD
alpha <- 0.05
mdd <- exp(-qnorm(1 - alpha / 2) * sqrt(4 / nfin))
thetasuc <- log(mdd)

lambda0 <- 0.7
theta0 <- log(lambda0)

n0 <- 50
sigma0 <- sqrt(4 / n0)

# direct computation
ddcp1 <- pnorm((log(mdd) - theta0) / sqrt(sigmafin ^ 2 + sigma0 ^ 2))

# histogram of power
M <- 10 ^ 6
set.seed(1977)
thetaprior <- rnorm(M, mean = theta0, sd = sigma0)
power_from_prior <- pnorm((log(mdd) - thetaprior) / sigmafin)

# corresponding density
y <- seq(0.005, 0.995, by = 0.005)
alpha0 <- sigmafin / sigma0
beta0 <- (log(mdd) - theta0) / sigma0
z <- qnorm(y)
u <- alpha0 * dnorm(beta0 - alpha0 * z) / dnorm(z)

# corresponding sensitivity interval
gamma0 <- 0.95
ci0 <- qg(p = c((1 - gamma0) / 2, (1 + gamma0) / 2), alpha = alpha0, beta = beta0)  
gamma1 <- 0.8
ci1 <- qg(p = c((1 - gamma1) / 2, (1 + gamma1) / 2), alpha = alpha0, beta = beta0)  

# location of minimium of g
NN <- sigmafin * (log(mdd) - theta0)
ZZ <- sigmafin ^ 2 - sigma0 ^ 2
ym <- pnorm(NN / ZZ)

# legend titles
leg.tit1 <- expression("d"[0]*" = ")
leg.tit2 <- expression("["*bar(a)*", "*bar(b)*"] / PoS: ")

# colors
cols <- 2:10
cols <- grey(c(2:10) / 11)
ltys2 <- 2:10

cols2 <- 2:4
cols2 <- grey(c(2:4) / 5)



