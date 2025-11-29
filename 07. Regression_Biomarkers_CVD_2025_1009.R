# ------------------------------------------------------------
# Purpose: Regression analysis of mediators (uric acid, albumin, and UAR) and CVD risk
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

#-----------------------------------------------------------
# LOAD REQUIRED PACKAGES
#-----------------------------------------------------------
library(dplyr)
library(broom)
library(ggplot2)

#-----------------------------------------------------------
# INITIALIZE OUTPUT DATAFRAME
#-----------------------------------------------------------
results_cvd <- data.frame(
  Exposure = character(),
  Model = character(),
  OR = numeric(),
  LCI = numeric(),
  UCI = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

#-----------------------------------------------------------
# FUNCTION: Run logistic regression for continuous exposures
#-----------------------------------------------------------
run_cvd_models_cont <- function(data, exposures, model_label, covars = c()) {
  for (exp_var in exposures) {
    # Construct formula
    f <- as.formula(paste("CVD_risk ~", exp_var,
                          if(length(covars) > 0) paste("+", paste(covars, collapse = "+"))))
    
    # Fit logistic regression
    mod <- glm(f, data = data, family = binomial)
    
    # Extract estimate, 95% CI, p-value
    est <- broom::tidy(mod, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == exp_var) %>%
      transmute(
        Exposure = exp_var,
        Model = model_label,
        OR = estimate,
        LCI = conf.low,
        UCI = conf.high,
        P_value = p.value
      )
    
    # Append to results dataframe
    results_cvd <<- bind_rows(results_cvd, est)
  }
}

#-----------------------------------------------------------
# DEFINE EXPOSURES
#-----------------------------------------------------------
exposures <- c("UAR", "albumin_log", "uric_log")

#-----------------------------------------------------------
# RUN MODELS
#-----------------------------------------------------------

# Model 3: Fully adjusted covariates
run_cvd_models_cont(aim1_OR, exposures, "Model3",
  covars = c("AGE","GENDER","racethnic","diabetes_new",
             "education","BMI","Cotinine","PIR","plogC")
)

#-----------------------------------------------------------
# VIEW RESULTS
#-----------------------------------------------------------
results_cvd <- results_cvd %>%
  arrange(Exposure, Model)

print(results_cvd)