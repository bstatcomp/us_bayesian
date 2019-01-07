setwd(paste0("C:\\Users\\Gregor\\Documents\\shared_files\\MR\\lectures",
             "\\US_Bayesian_FA"))

# libraries and sourcing -------------------------------------------------------
library(rstan)
library(ggplot2)

for (fn in list.files("./R_functions", full.names = T)) {
  cat("Sourcing ", fn, "\n")
  source(fn)
}

for (fn in list.files("./R_models", full.names = T)) {
  cat("Sourcing ", fn, "\n")
  source(fn)
}

# data -------------------------------------------------------------------------
deca_dat <- readRDS("./data/decathlon.rds")
head(deca_dat)

deca_dat[ ,c(1,5,6,10)] <- -deca_dat[ ,c(1,5,6,10)]
deca_dat                <- deca_dat[ ,c(1,3,10,2,4,5,6,7,8,9,11,12,13)]



X         <- deca_dat[ ,1:10]
sX        <- scale(X)
var_names <- colnames(deca_dat)[1:10]
ath_names <- rownames(deca_dat)


# factanal ---------------------------------------------------------------------
my_fa <- factanal(sX, factors = 2, scores = "regression")
load  <- my_fa$loadings
scr   <- my_fa$scores
my_biplot(scr, load)


# rotation
Q1 <- matrix(c(cos(pi / 4), -sin(pi / 4),
               sin(pi / 4), cos(pi / 4)), 
             byrow = TRUE,
             nrow  = 2)
par(mfrow = c(1,2))
my_biplot(scr, load)
my_biplot(scr %*% Q1, load %*% Q1)



# reflection
Q2 <- matrix(c(1, 0,
               0, -1),
             byrow = TRUE,
             nrow  = 2)
par(mfrow = c(1,2))
my_biplot(scr, load)
my_biplot(scr %*% Q2, load %*% Q2)


par(mfrow = c(1,1))
