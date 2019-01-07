setwd(paste0("C:\\Users\\Gregor\\Documents\\shared_files\\MR\\lectures",
             "\\US_Bayesian_FA"))

library("FactoMineR")
data(decathlon)
head(decathlon)
saveRDS(decathlon, "./data/decathlon.rds")
