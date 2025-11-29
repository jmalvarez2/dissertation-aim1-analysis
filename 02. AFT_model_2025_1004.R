# ------------------------------------------------------------
# Purpose: Impute values below the LOD using Accelerated Failure Time (AFT)
#
# Created by: Juan M. Alvarez
#
# Reference:
#   Lee, M., Saha, A., Sundaram, R. et al. (2024).
#   Accommodating detection limits of multiple exposures in
#   environmental mixture analyses. Environ Health, 23, 48.
#
# Input dataset: aim1_v5.sas7bdat
# Output dataset: aim1_imputed_2025_1004.csv
# ------------------------------------------------------------

library(haven)
library(tidyverse)
library(corrplot)
library(melt)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library (broom)
library (RColorBrewer)
library (viridis)
library (patchwork)
library (purrr)
library (forcats)
library (ggcorrplot)
library (Hmisc)
library (survival)
library (tmvtnorm)
library (truncnorm)
library(writexl)

aim1_v5 <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/SAS Datasets/aim1_v5.sas7bdat", NULL)

# Step 1: Set up variable names
lod.var.names <- c("PFHxS", "PFNA", "PFDE", "MBzP", "MCPP", "MEHHP", "MEHP", "MEOHP", "MEP", "MnBP", "MiBP",
                   "NAP1", "NAP2", "OHFlu2", "OHFlu3", "OHPHE1", "PYR1")
lod.n <- length(lod.var.names)
lod.values <- c(0.1, 0.08, 0.1, 0.3, 0.2, 0.2, 0.5, 0.2, 0.6, 0.4, 0.2, 44, 42, 10, 10, 10, 10)
names(lod.values) <- lod.var.names

covariate.names <- c("AGE", "racethnic", "diabetes_new", "BMI", "education", "Cotinine", "GENDER", "PIR")  # replace with real covariates
aft.data <- aim1_v5


# Step 2: Create indicator variables for detection (1 = detected, 0 = censored)
for (chem in lod.var.names) {
  aft.data[[paste0("ind_", chem)]] <- ifelse(aft.data[[chem]] >= lod.values[chem], 1, 0)
}

# Step 3: Setup matrices
n <- nrow(aft.data)
aft.coef.mat <- matrix(NA, ncol=length(covariate.names)+1, nrow=lod.n)
aft.resi.mat <- matrix(NA, ncol=lod.n, nrow=n)
aft.mean.mat <- matrix(NA, ncol=lod.n, nrow=n)

# Step 4: Fit AFT models
for(k in 1:lod.n){
  zname <- lod.var.names[k]
  indname <- paste0("ind_", zname)
  
  TT <- -log(pmax(aft.data[[zname]], 1e-10))  # Avoid log(0)
  delta <- aft.data[[indname]]
  
  formula_str <- paste0("Surv(exp(TT), delta) ~ ", paste(covariate.names, collapse = " + "))
  aft.reg <- survreg(as.formula(formula_str), data=aft.data, dist="lognormal")
  
  X <- model.matrix(aft.reg)
  mu_hat <- as.vector(X %*% aft.reg$coefficients)
  
  aft.coef.mat[k, ] <- aft.reg$coefficients
  aft.resi.mat[, k] <- TT - mu_hat
  aft.mean.mat[, k] <- mu_hat
}

# Step 5: Covariance matrix of residuals (only from detected values)
delta.mat <- aft.data[, paste0("ind_", lod.var.names)]
aft.resi.mat2 <- ifelse(delta.mat == 1, aft.resi.mat, NA)
sigma <- cov(aft.resi.mat2, use = "pairwise.complete.obs")

# Step 6: Impute missing residuals using truncated multivariate normal
imputed.res.mat <- aft.resi.mat2

for(i in 1:n){
  ind.delta <- as.numeric(delta.mat[i, ])
  ind.res <- aft.resi.mat[i, ]
  obs.d <- which(ind.delta == 1)
  mis.d <- which(ind.delta == 0)
  
  if(length(mis.d) == 0){
    next  # nothing to impute
  } else if(length(obs.d) == 0){
    mu.cond <- rep(0, lod.n)
    cov.cond <- sigma
    tmu.cond <- mtmvnorm(mean = mu.cond, sigma = cov.cond, lower = ind.res[mis.d])$tmean
    imputed.res.mat[i, mis.d] <- tmu.cond
  } else {
    mu.cond <- sigma[mis.d, obs.d, drop=FALSE] %*%
      solve(sigma[obs.d, obs.d, drop=FALSE]) %*%
      ind.res[obs.d]
    cov.cond <- sigma[mis.d, mis.d, drop=FALSE] -
      sigma[mis.d, obs.d, drop=FALSE] %*%
      solve(sigma[obs.d, obs.d, drop=FALSE]) %*%
      sigma[obs.d, mis.d, drop=FALSE]
    
    tmu.cond <- mtmvnorm(mean = as.vector(mu.cond), sigma = cov.cond,
                         lower = ind.res[mis.d])$tmean
    tmu.cond <- ifelse(is.na(tmu.cond), ind.res[mis.d], tmu.cond)
    imputed.res.mat[i, mis.d] <- tmu.cond
  }
}

# Step 7: Back-transform to original scale
imputed.Z <- exp(-(aft.mean.mat + imputed.res.mat))

# Step 8: Replace imputed values into dataset
aft.data[, lod.var.names] <- imputed.Z

# Step 9: Log-transform the imputed variables
for (z in lod.var.names) {
  aft.data[[paste0(z, "_log")]] <- log(aft.data[[z]])
}

write.csv(aft.data, "C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Excel Datasets/aim1_imputed_2025_1004.csv", row.names = TRUE)
write_xlsx(aft.data, "C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Excel Datasets/aim1_imputed_2025_1004.xlsx")




