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


# 2-factor model ---------------------------------------------------------------
tX <- t(sX)
m  <- nrow(tX)
n  <- ncol(tX)
p  <- 2
stan_data <- list(n  = n,
                  m  = m,
                  p  = p,
                  X  = tX)

# Model 1
model_name <- "fa1"
model      <- get_stanmod(model_name)
samp1      <- rstan::sampling(model, 
                              data   = stan_data, 
                              chains = 3, 
                              iter   = 2000, 
                              seed   = 3)
# saveRDS(samp1, file = "./samples/fa1_nfac2_3chain.rds")
# samp1 <- readRDS("./samples/fa1_nfac2_3chain.rds")
stan_trace(samp1)
stan_trace(samp1) + scale_color_manual(values = c("white", "white", "orange"))
stan_ac(samp1)
stan_dens(samp1)
samp1

# Nekaj ni OK, traceplot izgleda, kot da se ustavi na veèih mestih. Prav tako
# opažamo visoko avtokorelacijo, tudi po 20 korakih in veè. Nekatere gostote
# vzorcev imajo veè vrhov.



# Model 2
model_name <- "fa2"
model      <- get_stanmod(model_name)
samp2      <- rstan::sampling(model, 
                              data   = stan_data, 
                              chains = 3, 
                              iter   = 2000,
                              seed   = 1)
# saveRDS(samp2, file = "./samples/fa2_nfac2_3chain.rds")
# samp2 <- readRDS("./samples/fa2_nfac2_3chain.rds")
stan_trace(samp2, pars = "L_tri")
stan_trace(samp2, pars = "L_tri") + 
  scale_color_manual(values = c("white", "white", "orange"))
stan_ac(samp2, pars = "L_tri")
stan_dens(samp2, pars = "L_tri")
samp2

ext2 <- get_fapars(samp2, par_names = c("L_tri", "Factor", "Psi", "Psi_full"))
ld2  <- ext2$L_tri
sc2  <- ext2$Factor
psi2 <- ext2$Psi
rownames(ld2) <- var_names
colnames(sc2) <- ath_names
ld2
my_biplot(t(sc2), ld2)

# Varimax
ld_vmax <- varimax(ld2)
Q       <- ld_vmax$rotmat
sc_vmax <- t(Q) %*% sc2
par(mfrow = c(1,2))
my_biplot(t(sc2), ld2)
my_biplot(t(sc_vmax), ld_vmax$loadings)




# Model prior
model_name             <- "fa_prior"
model                  <- get_stanmod(model_name)
stan_data$Lambda_prior <- ld2
stan_data$Psi_prior    <- psi2
samp3                  <- rstan::sampling(model, 
                                          data   = stan_data, 
                                          chains = 3, 
                                          iter   = 50000, 
                                          seed   = 3)
# saveRDS(samp3, file = "./samples/fa_prior_nfac2_3chain.rds")
# samp3 <- readRDS("./samples/fa_prior_nfac2_3chain.rds")
stan_trace(samp3)
stan_ac(samp3)
stan_dens(samp3)

ext3 <- get_fapars(samp3, par_names = c("Lambda", "Factor", "Psi", "Psi_full"))
ld3  <- ext3$Lambda
sc3  <- ext3$Factor
psi3 <- ext3$Psi
rownames(ld3) <- var_names
colnames(sc3) <- ath_names
ld3
par(mfrow = c(1,1))
my_biplot(t(sc3), ld3, xld = c(-1.55, 1.55), yld = c(-1.55, 1.55))



# 3-factor model ---------------------------------------------------------------
stan_data$p <- 3

model_name <- "fa2"
model      <- get_stanmod(model_name)
samp4      <- rstan::sampling(model, 
                              data   = stan_data, 
                              chains = 3, 
                              iter   = 2000, 
                              seed   = 3)
samp4
# saveRDS(samp4, file = "./samples/fa2_nfac3_3chain.rds")
# samp4 <- readRDS("./samples/fa2_nfac3_3chain.rds")
stan_trace(samp4, pars = "L_tri")
stan_ac(samp4, pars = "L_tri")
stan_dens(samp4, pars = "L_tri")

ext4 <- get_fapars(samp4, par_names = c("L_tri", "Factor", "Psi", "Psi_full"))
ld4  <- ext4$L_tri
sc4  <- ext4$Factor
psi4 <- ext4$Psi
rownames(ld4) <- var_names
colnames(sc4) <- ath_names
ld4
par(mfrow = c(1,2))
my_biplot(t(sc4), ld4)
my_biplot(t(sc4[c(1,3), ]), ld4[ ,c(1,3)])


# primerjava preostalih varianc
psi2
psi4
