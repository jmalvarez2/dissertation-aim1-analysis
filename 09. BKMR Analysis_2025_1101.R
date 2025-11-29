# ------------------------------------------------------------
# Purpose: Conduct Bayesian Kernel Machine Regression (BKMR) analyses
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

# Load required library
library(bkmr)
library (patchwork)

# Define outcome
Y <- aim1_OR$CVD_risk

# Define covariates (adjust this list as needed)
covariate.names <- c("AGE","GENDER","racethnic","diabetes_new","education","BMI","Cotinine","PIR", "plogC")

# Define exposures — adjust to match your log-transformed EDC names
exposure_vars <- c("PFHxS_log", "PFNA_log", "PFDE_log", "MBzP_log_adj", "MCPP_log_adj", "MEHHP_log_adj", "MEHP_log_adj", 
                   "MEOHP_log_adj", "MEP_log_adj", "MnBP_log_adj", "MiBP_log_adj", "NAP1_log_adj", "NAP2_log_adj", "OHFlu2_log_adj", "OHFlu3_log_adj",
                   "OHPHE1_log_adj", "PYR1_log_adj")

# Extract covariates and exposures from the data
covar.bkmr <- aim1_OR[, covariate.names]
expos.bkmr <- aim1_OR[, exposure_vars]

covar.bkmr_numeric <- covar.bkmr %>%
  mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
  as.matrix()


# Run BKMR (binary outcome requires family = "binomial")
set.seed(2025)
bkmr_fit <- kmbayes(
  y = Y,
  Z = expos.bkmr,
  X = covar.bkmr_numeric,
  iter = 2000,
  est.h = TRUE,
  family = "binomial",
  varsel = TRUE,            # enable variable selection
  groups = c(1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3),
  verbose = FALSE
)

summary(bkmr_fit)

# Overall mixture effect when ALL exposures increase together
overall_est <- OverallRiskSummaries(
  fit = bkmr_fit,
  qs = seq(0.25, 0.75, by = 0.05),  # evaluate mixture at 25th–75th percentiles
  q.fixed = 0.5,                     # hold other exposures at their medians
  method = "approx"
)

# Plot

overall_est$ci.lb <- overall_est$est - 1.96 * overall_est$sd
overall_est$ci.ub <- overall_est$est + 1.96 * overall_est$sd

library(ggplot2)

p1 <- ggplot(overall_est, aes(x = quantile, y = est)) +

  # dots at each estimate
  geom_point(size = 2, color = "black") +
  # vertical error bars for CIs
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.00, color = "black") +
  # horizontal reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Quantile",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 14)

########################################################################

# Get medians of exposures
meds <- apply(expos.bkmr, 2, median)

# Predictor-response univariate estimates from BKMR
pred.resp.univar <- PredictorResponseUnivar(
  fit = bkmr_fit,
  qs = seq(0.1, 0.9, by = 0.1),   # exposures grid percentiles
  q.fixed = 0.5,                  # fix other exposures at median
  method = "approx"
)

clean_labels <- c(
  "PFHxS_log" = "PFHxS",
  "PFNA_log" = "PFNA",
  "PFDE_log" = "PFDE",
  "MBzP_log_adj" = "MBzP",
  "MCPP_log_adj" = "MCPP",
  "MEHHP_log_adj" = "MEHHP",
  "MEHP_log_adj" = "MEHP",
  "MEOHP_log_adj" = "MEOHP",
  "MEP_log_adj" = "MEP",
  "MnBP_log_adj" = "MnBP",
  "MiBP_log_adj" = "MiBP",
  "NAP1_log_adj" = "1-NAP",
  "NAP2_log_adj" = "2-NAP",
  "OHFlu2_log_adj" = "2-FLUO",
  "OHFlu3_log_adj" = "3-FLUO",
  "OHPHE1_log_adj" = "1-PHEN",
  "PYR1_log_adj" = "1-PYR"
)

# Then in your p2 plot:
p2 <- ggplot(pred.resp.univar, aes(x = z, y = est)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = est - 1.96*se, ymax = est + 1.96*se), 
              alpha = 0.2, fill = "gray50") +
  facet_wrap(~ variable, ncol = 4, scales = "fixed",
             labeller = labeller(variable = clean_labels)) +
  coord_cartesian(xlim = c(-2,4), ylim = c(-0.75, 0.75)) +
  labs(
    x = "Exposure",
    y = "h(Exposure)"
  ) +
  theme_minimal(base_size = 14)



BKMR_results <- p1 / p2 + 
  plot_layout(heights = c(1, 2)) +   # p1 smaller, p2 bigger
  plot_annotation(
    tag_levels = 'A',                
    tag_prefix = "",                  
    tag_suffix = ""                   
  )

BKMR_results

ggsave(
  filename = "C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/BKMR/BKMR_results.pdf", 
  plot = BKMR_results, 
  width = 10,       # increase width
  height = 16,      # increase height
  dpi = 300
)


