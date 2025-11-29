# ------------------------------------------------------------
# Purpose: Create Frequency Table 1 along with ORs
#
# Created by: Juan M. Alvarez
#
# Input dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

library(haven)
library(dplyr)
library(tableone)
library(writexl)

# Load data
aim1 <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/SAS Datasets/imputed_aim1_2025_1004.sas7bdat", NULL)

# Convert to data.frame
df <- as.data.frame(aim1)

# Define variables
cat_vars <- c("GENDER", "age_new", "age_strata", "racethnic", "education",
              "PIR_new", "diabetes_new", "BMI_new", "cot_new", "albuminuria", "hyperuric")

cont_vars <- c("AGE", "PIR", "BMI", "Cotinine", "CHOL", "UAR", "albumin_mgdL",
               "uric_acid", "SBP_avg", "DBP_avg", "plogC")

# Convert categorical variables to factors
df[cat_vars] <- lapply(df[cat_vars], function(x) as.factor(as.character(x)))
df$CVD_risk <- as.factor(df$CVD_risk)

# Create TableOne
tab1 <- CreateTableOne(vars = c(cat_vars, cont_vars),
                       strata = "CVD_risk",
                       data = df,
                       test = TRUE,
                       addOverall = TRUE)

# Extract table as a data frame
tab1_mat <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

# Convert matrix to proper data frame and keep row names as a column
tab1_df <- as.data.frame(tab1_mat)
tab1_df <- cbind(Characteristic = rownames(tab1_df), tab1_df)
rownames(tab1_df) <- NULL

# Export to Excel
write_xlsx(tab1_df,
           "C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Tables/R Table outputs/Table1_freq_2025_1004.xlsx")