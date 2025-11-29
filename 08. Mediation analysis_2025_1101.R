# ------------------------------------------------------------
# Purpose: Conduct mediation analyses
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

# Install required package if not already installed
devtools::install_github("dustinfife/flexplot")

install.packages("mediation")

# Load package
library(mediation)
library(flexplot)

aim1_OR <- subset(aim1_OR, education != 6)

#################################################################
#Uric acid and PFNA

# STEP 1: Fit the mediator model (UAR ~ exposure + covariates)
med_model_ua <- lm(uric_acid ~ PFNA_log + AGE + racethnic + education + diabetes_new + 
                     BMI_new + cot_new + GENDER + PIR, data = aim1_OR)
summary(med_model_ua)

# STEP 2: Fit the outcome model (CVD_risk ~ exposure + mediator + covariates)
out_model_ua <- glm(CVD_risk ~ uric_acid + PFNA_log + AGE + racethnic + education + 
                      diabetes_new + BMI_new + cot_new + GENDER + PIR, data = aim1_OR)
summary(out_model_ua)

# STEP 3: Run mediation analysis
med_out_ua <- mediate(med_model_ua, out_model_ua, treat = "PFNA_log", 
                      mediator = "uric_acid",
                      boot = TRUE, sims = 10000)

# STEP 4: Summarize results
summary(med_out_ua)

mediate_plot(CVD_risk ~ uric_acid + PFNA_log, data = aim1_OR)

# Extract mediation results
med_res <- summary(med_out_ua)

# Create formatted results table
med_table <- data.frame(
  Type = c("Indirect", "Direct", "Total"),
  Effect = c(
    round(med_res$d0, 3),   # ACME (Average Causal Mediation Effect)
    round(med_res$z0, 3),   # ADE (Average Direct Effect)
    round(med_res$tau.coef, 3) # Total Effect
  ),
  SE = c(
    round(med_res$d0.sims %>% sd(), 3),   # SE for ACME from bootstrap sims
    round(med_res$z0.sims %>% sd(), 3),   # SE for ADE
    round(med_res$tau.sims %>% sd(), 3)   # SE for Total
  ),
  LCI = c(
    round(med_res$d0.ci[1], 3),
    round(med_res$z0.ci[1], 3),
    round(med_res$tau.ci[1], 3)
  ),
  UCI = c(
    round(med_res$d0.ci[2], 3),
    round(med_res$z0.ci[2], 3),
    round(med_res$tau.ci[2], 3)
  ),
  PCT = c(
    round(med_res$n0 * 100, 1),   # proportion mediated %
    round((1 - med_res$n0) * 100, 1),
    100
  ),
  `p value` = c(
    signif(med_res$d0.p, 3),
    signif(med_res$z0.p, 3),
    signif(med_res$tau.p, 3)
  )
)

med_table


#################################################################
#Uric acid and PFHxS

# STEP 1: Fit the mediator model (UAR ~ exposure + covariates)
med_model_ua <- lm(uric_acid ~ PFHxS_log + AGE + racethnic + education + diabetes_new + 
                     BMI_new + cot_new + GENDER + PIR, data = aim1_OR)
summary(med_model_ua)

# STEP 2: Fit the outcome model (CVD_risk ~ exposure + mediator + covariates)
out_model_ua <- glm(CVD_risk ~ uric_acid + PFHxS_log + AGE + racethnic + education + 
                      diabetes_new + BMI_new + cot_new + GENDER + PIR,
                    data = aim1_OR)
summary(out_model_ua)

# STEP 3: Run mediation analysis
med_out_ua <- mediate(med_model_ua, out_model_ua, treat = "PFHxS_log", 
                      mediator = "uric_acid",
                      boot = TRUE, sims = 10000)

# STEP 4: Summarize results
summary(med_out_ua)

mediate_plot(CVD_risk ~ uric_acid + PFHxS_log, data = aim1_OR)

# Extract mediation results
med_res <- summary(med_out_ua)

# Create formatted results table
med_table_PFHxS <- data.frame(
  Type = c("Indirect", "Direct", "Total"),
  Effect = c(
    round(med_res$d0, 3),   # ACME (Average Causal Mediation Effect)
    round(med_res$z0, 3),   # ADE (Average Direct Effect)
    round(med_res$tau.coef, 3) # Total Effect
  ),
  SE = c(
    round(med_res$d0.sims %>% sd(), 3),   # SE for ACME from bootstrap sims
    round(med_res$z0.sims %>% sd(), 3),   # SE for ADE
    round(med_res$tau.sims %>% sd(), 3)   # SE for Total
  ),
  LCI = c(
    round(med_res$d0.ci[1], 3),
    round(med_res$z0.ci[1], 3),
    round(med_res$tau.ci[1], 3)
  ),
  UCI = c(
    round(med_res$d0.ci[2], 3),
    round(med_res$z0.ci[2], 3),
    round(med_res$tau.ci[2], 3)
  ),
  PCT = c(
    round(med_res$n0 * 100, 1),   # proportion mediated %
    round((1 - med_res$n0) * 100, 1),
    100
  ),
  `p value` = c(
    signif(med_res$d0.p, 3),
    signif(med_res$z0.p, 3),
    signif(med_res$tau.p, 3)
  )
)

med_table_PFHxS








