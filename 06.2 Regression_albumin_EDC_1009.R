# ------------------------------------------------------------
# Purpose: Regression analysis of albumin and individual EDCs
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

#-----------------------------------------------------------
# OUTPUT DATAFRAME
#-----------------------------------------------------------
results_albumin <- data.frame(
  Chem = character(),
  Model = character(),
  Type = character(),
  OR = numeric(),
  LCI = numeric(),
  UCI = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

#-----------------------------------------------------------
# FUNCTION: Run regression for continuous exposures only
#-----------------------------------------------------------
run_models_cont_albumin <- function(data, chems, units, model_label, covars = c()) {
  for (i in seq_along(chems)) {
    chem <- chems[i]
    unit <- units[i]
    
    # Continuous model
    f_cont <- as.formula(paste("albumin_log ~", chem,
                               if(length(covars) > 0) paste("+", paste(covars, collapse = "+"))))
    mod_cont <- glm(f_cont, data = data, family = gaussian)
    
    est_cont <- broom::tidy(mod_cont, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == chem) %>%  # only chemical
      transmute(
        Chem = chem,
        Model = model_label,
        Type = "Continuous",
        OR = estimate,
        LCI = conf.low,
        UCI = conf.high,
        P_value = p.value
      )
    
    results_albumin <<- bind_rows(results_albumin, est_cont)
  }
}

#-----------------------------------------------------------
# MODEL 3: Fully adjusted covariates
#-----------------------------------------------------------
run_models_cont_albumin(aim1_OR, pfases, pfas_unit, "Model3", 
                        covars = c("AGE","GENDER","racethnic","diabetes_new","education","BMI","Cotinine","PIR"))
run_models_cont_albumin(aim1_OR, phthals, phthal_unit, "Model3",
                        covars = c("AGE","GENDER","racethnic","diabetes_new","education", "BMI","Cotinine","PIR","plogC"))
run_models_cont_albumin(aim1_OR, pahs, pah_unit, "Model3",
                        covars = c("AGE","GENDER","racethnic","diabetes_new","education","BMI","Cotinine","PIR","plogC"))
