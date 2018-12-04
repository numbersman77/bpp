# --------------------------------------------------------------
# load packages
# --------------------------------------------------------------
packs <- c("reporttools", "distr")
for (i in 1:length(packs)){library(packs[i], character.only = TRUE)}

# --------------------------------------------------------------
# define paths
# --------------------------------------------------------------
path         <- paste(getwd(), "/", sep = "")
path.paper   <- paste(path, "paper/", sep = "")
path.results <- paste(path, "results/", sep = "")
path.code    <- paste(path, "code/", sep = "")
path.depot   <- "C:/rufibach/depot/"

# --------------------------------------------------------------
# load functions
# --------------------------------------------------------------
source(paste(path.code, "functions/baseplot.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/dnormtrunc.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/pnormtrunc.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/dg.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/pg.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toRootqq.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/qg.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/dgtrunc.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toInt1.r", sep = ""), echo = FALSE)

source(paste(path.code, "functions/dgunif.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toInt2.r", sep = ""), echo = FALSE)

source(paste(path.code, "functions/dUniformNormalTails.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/dgUniformNormalTails.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toInt3.r", sep = ""), echo = FALSE)

source(paste(path.code, "functions/bisection.r", sep = ""), echo = FALSE)

source(paste(path.code, "functions/pgbinom.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toIntpgbinom.r", sep = ""), echo = FALSE)
source(paste(path.code, "functions/toRootpgbinom.r", sep = ""), echo = FALSE)


