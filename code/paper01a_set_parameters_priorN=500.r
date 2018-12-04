# increase number of prior events
n02 <- 500
sigma02 <- sqrt(4 / n02)

# direct computation of PoS
ddcp12 <- pnorm((log(mdd) - theta0) / sqrt(sigmafin ^ 2 + sigma02 ^ 2))
alpha02 <- sigmafin / sigma02
beta02 <- (log(mdd) - theta0) / sigma02

# corresponding sensitivity intervals
ci02 <- qg(p = c((1 - gamma0) / 2, (1 + gamma0) / 2), alpha = alpha02, beta = beta02)  
ci12 <- qg(p = c((1 - gamma1) / 2, (1 + gamma1) / 2), alpha = alpha02, beta = beta02)  





