# ------------------------------------------------------------
# Purpose: Conduct crude, minimally adjusted, and fully adjusted
#          logistic regression of CVD risk for 17 EDCs
#
# Created by: Juan M. Alvarez
#
# Input dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

library(dplyr)
library(broom)
library(readr)
library(writexl)

# Load data
aim1_OR <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/SAS Datasets/imputed_aim1_2025_1004.sas7bdat", NULL)

#-----------------------------------------------------------
# SETUP: Define chemical lists
#-----------------------------------------------------------

# PFAS
pfases <- c("PFHxS_log", "PFNA_log", "PFDE_log")
pfas_quarts <- c("PFHxS_log_quart", "PFNA_log_quart", "PFDE_log_quart")
pfas_unit <- c(1, 1, 1)

# Phthalates
phthals <- c("MnBP_log_adj", "MEP_log_adj", "MEHP_log_adj", "MBzP_log_adj",
             "MCPP_log_adj", "MEHHP_log_adj", "MEOHP_log_adj", "MiBP_log_adj")
phthal_quarts <- c("MnBP_log_adj_quart", "MEP_log_adj_quart", "MEHP_log_adj_quart",
                   "MBzP_log_adj_quart", "MCPP_log_adj_quart", "MEHHP_log_adj_quart",
                   "MEOHP_log_adj_quart", "MiBP_log_adj_quart")
phthal_unit <- rep(1, length(phthals))

# PAHs
pahs <- c("NAP1_log_adj", "NAP2_log_adj", "OHFlu2_log_adj", "OHFlu3_log_adj", 
          "OHPHE1_log_adj", "PYR1_log_adj")
pah_quarts <- c("NAP1_log_adj_quart", "NAP2_log_adj_quart", "OHFlu2_log_adj_quart",
                "OHFlu3_log_adj_quart", "OHPHE1_log_adj_quart", "PYR1_log_adj_quart")
pah_unit <- rep(1, length(pahs))

#-----------------------------------------------------------
# OUTPUT DATAFRAME
#-----------------------------------------------------------
results <- data.frame(
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
# FUNCTION: Run logistic regression for continuous and quartiles
#-----------------------------------------------------------
run_models <- function(data, chems, quarts, units, model_label, covars = c()) {
  for (i in seq_along(chems)) {
    chem <- chems[i]
    quart <- quarts[i]
    unit <- units[i]
    
# Continuous model: per IQR increase
chem_iqr <- IQR(data[[chem]], na.rm = TRUE)  # calculate IQR of the chemical
data[[paste0(chem, "_IQR")]] <- data[[chem]] / chem_iqr  # rescale variable

f_cont <- as.formula(
  paste("CVD_risk ~", paste0(chem, "_IQR"),
        if(length(covars) > 0) paste("+", paste(covars, collapse = "+")))
)

mod_cont <- glm(f_cont, data = data, family = binomial)

est_cont <- broom::tidy(mod_cont, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term == paste0(chem, "_IQR")) %>%  # only chemical
  transmute(
    Chem = chem,
    Model = model_label,
    Type = "Continuous (per IQR)",
    OR = estimate,
    LCI = conf.low,
    UCI = conf.high,
    P_value = p.value
  )

results <<- bind_rows(results, est_cont)
    
    # Quartile model
    data[[quart]] <- factor(data[[quart]])
    data[[quart]] <- relevel(data[[quart]], ref = levels(data[[quart]])[1]) # Q1 reference
    
    f_quart <- as.formula(paste("CVD_risk ~", quart,
                                if(length(covars) > 0) paste("+", paste(covars, collapse = "+"))))
    mod_quart <- glm(f_quart, data = data, family = binomial)
    
    est_quart <- broom::tidy(mod_quart, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(grepl(quart, term)) %>%  # keep only quartile terms
      filter(!grepl("0$", term)) %>%  # remove Q1 (reference)
      mutate(
        # Extract only the trailing digit after "_quart" to assign quartile
        qnum = as.integer(sub(paste0("^", quart, "(\\d+)$"), "\\1", term)),
        Type = paste0("Q", qnum, " vs Q1")
      ) %>%
      transmute(
        Chem = chem,
        Model = model_label,
        Type = Type,
        OR = estimate,
        LCI = conf.low,
        UCI = conf.high,
        P_value = p.value
      )
    
    # Append quartile results
    results <<- bind_rows(results, est_quart)
  }
  }
#-----------------------------------------------------------
# MODEL 1: Minimal covariates
#-----------------------------------------------------------
run_models(aim1_OR, pfases, pfas_quarts, pfas_unit, "Model1",covars = c())
run_models(aim1_OR, phthals, phthal_quarts, phthal_unit, "Model1",covars = c("plogC"))
run_models(aim1_OR, pahs, pah_quarts, pah_unit, "Model1",covars = c("plogC"))

#-----------------------------------------------------------
# MODEL 2: Basic covariates (AGE, GENDER, racethnic)
#-----------------------------------------------------------
run_models(aim1_OR, pfases, pfas_quarts, pfas_unit, "Model2", covars = c("AGE","GENDER","racethnic"))
run_models(aim1_OR, phthals, phthal_quarts, phthal_unit, "Model2",covars = c("AGE","GENDER","racethnic","plogC"))
run_models(aim1_OR, pahs, pah_quarts, pah_unit, "Model2",covars = c("AGE","GENDER","racethnic","plogC"))

#-----------------------------------------------------------
# MODEL 3: Fully adjusted covariates
# (AGE, GENDER, racethnic, diabetes_new, BMI, Cotinine, PIR)
#-----------------------------------------------------------
run_models(aim1_OR, pfases, pfas_quarts, pfas_unit, "Model3", 
           covars = c("AGE","GENDER","racethnic","diabetes_new","education","BMI","Cotinine","PIR"))
run_models(aim1_OR, phthals, phthal_quarts, phthal_unit, "Model3",
           covars = c("AGE","GENDER","racethnic","diabetes_new","education", "BMI","Cotinine","PIR","plogC"))
run_models(aim1_OR, pahs, pah_quarts, pah_unit, "Model3",
           covars = c("AGE","GENDER","racethnic","diabetes_new","education","BMI","Cotinine","PIR","plogC"))

#-----------------------------------------------------------
# EXPORT RESULTS TO CSV
#-----------------------------------------------------------
write_xlsx(results,"C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Tables/R Table outputs/EDC_CVD_results_2025_1004.xlsx")
