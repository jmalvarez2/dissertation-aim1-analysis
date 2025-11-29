# ------------------------------------------------------------
# Purpose: To evaluate the distribution of EDC chemicals (PFAS, phthalates, and PAHs)
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_v5.sas7bdat
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

aim1 <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/SAS Datasets/aim1_v5.sas7bdat", NULL)

################################################################################
#QQ Plots

###################Perfluoroalkyl Substances###################

#PFHxS
qqnorm(aim1$PFHxS,
       main = "QQ plot of PFHxS")
qqline(aim1$PFHxS, col = "red")

qqnorm(aim1$PFHxS_log,
       main = "QQ plot of log-transformed PFHxS")
qqline(aim1$PFHxS_log, col = "red")


#PFNA
qqnorm(aim1$PFNA,
       main = "QQ plot of PFNA")
qqline(aim1$PFNA, col = "red")

qqnorm(aim1$PFNA_log,
       main = "QQ plot of log-transformed PFNA")
qqline(aim1$PFNA_log, col = "red")


#PFDE
qqnorm(aim1$PFDE,
       main = "QQ plot of PFDE")
qqline(aim1$PFDE, col = "red")

qqnorm(aim1$PFDE_log,
       main = "QQ plot of log-transformed PFDE")
qqline(aim1$PFDE_log, col = "red")

###################Phthalates###################

#MBzP
qqnorm(aim1$MBzP_adj,
       main = "QQ plot of creatinine-adjusted MBzP")
qqline(aim1$MBzP_adj, col = "black")

qqnorm(aim1$lnMBzP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MBzP")
qqline(aim1$lnMBzP_adj, col = "black")


#MCPP
qqnorm(aim1$MCPP_adj,
       main = "QQ plot of creatinine-adjusted MCPP")
qqline(aim1$MCPP_adj, col = "black")

qqnorm(aim1$lnMCPP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MCPP")
qqline(aim1$lnMCPP_adj, col = "black")


#MEHHP
qqnorm(aim1$MEHHP_adj,
       main = "QQ plot of creatinine-adjusted MEHHP")
qqline(aim1$MEHHP_adj, col = "black")

qqnorm(aim1$lnMEHHP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MEHHP")
qqline(aim1$lnMEHHP_adj, col = "black")


#MEHP
qqnorm(aim1$MEHP_adj,
       main = "QQ plot of creatinine-adjusted MEHP")
qqline(aim1$MEHP_adj, col = "black")

qqnorm(aim1$lnMEHP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MEHP")
qqline(aim1$lnMEHP_adj, col = "black")


#MEOHP
qqnorm(aim1$MEOHP_adj,
       main = "QQ plot of creatinine-adjusted MEOHP")
qqline(aim1$MEOHP_adj, col = "black")

qqnorm(aim1$lnMEOHP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MEOHP")
qqline(aim1$lnMEOHP_adj, col = "black")


#MEP
qqnorm(aim1$MEP_adj,
       main = "QQ plot of creatinine-adjusted MEP")
qqline(aim1$MEP_adj, col = "black")

qqnorm(aim1$lnMEP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MEP")
qqline(aim1$lnMEP_adj, col = "black")


#MnBP
qqnorm(aim1$MnBP_adj,
       main = "QQ plot of creatinine-adjusted MnBP")
qqline(aim1$MnBP_adj, col = "black")

qqnorm(aim1$lnMnBP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MnBP")
qqline(aim1$lnMnBP_adj, col = "black")


#MiBP
qqnorm(aim1$MiBP_adj,
       main = "QQ plot of creatinine-adjusted MiBP")
qqline(aim1$MiBP_adj, col = "black")

qqnorm(aim1$lnMiBP_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted MiBP")
qqline(aim1$lnMiBP_adj, col = "black")

###################Polycylic Aromatic Hydrocarbons###################

#NAP1
qqnorm(aim1$NAP1_adj,
       main = "QQ plot of creatinine-adjusted NAP1")
qqline(aim1$NAP1_adj, col = "blue")

qqnorm(aim1$lnNAP1_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted NAP1")
qqline(aim1$lnNAP1_adj, col = "blue")


#NAP2
qqnorm(aim1$NAP2_adj,
       main = "QQ plot of creatinine-adjusted NAP2")
qqline(aim1$NAP2_adj, col = "blue")

qqnorm(aim1$lnNAP2_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted NAP2")
qqline(aim1$lnNAP2_adj, col = "blue")


#OHFlu2
qqnorm(aim1$OHFlu2_adj,
       main = "QQ plot of creatinine-adjusted OHFlu2")
qqline(aim1$OHFlu2_adj, col = "blue")

qqnorm(aim1$lnOHFlu2_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted OHFlu2")
qqline(aim1$lnOHFlu2_adj, col = "blue")


#OHFlu3
qqnorm(aim1$OHFlu3_adj,
       main = "QQ plot of creatinine-adjusted OHFlu3")
qqline(aim1$OHFlu3_adj, col = "blue")

qqnorm(aim1$lnOHFlu3_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted OHFlu3")
qqline(aim1$lnOHFlu3_adj, col = "blue")


#OHPHE1
qqnorm(aim1$OHPHE1_adj,
       main = "QQ plot of creatinine-adjusted OHPHE1")
qqline(aim1$OHPHE1_adj, col = "blue")

qqnorm(aim1$lnOHPHE1_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted OHPHE1")
qqline(aim1$lnOHPHE1_adj, col = "blue")


#PYR1
qqnorm(aim1$PYR1_adj,
       main = "QQ plot of creatinine-adjusted PYR1")
qqline(aim1$PYR1_adj, col = "blue")

qqnorm(aim1$lnPYR1_adj,
       main = "QQ plot of log-adjusted creatinine-adjusted PYR1")
qqline(aim1$lnPYR1_adj, col = "blue")

################################################################################
#Histograms
#Prepare data for histograms

exp_A1_log <-aim1 [,c("PFHxS_log", "PFDE_log", "PFNA_log",
                                "lnMnBP_adj", "lnMEP_adj", "lnMEHP_adj", "lnMBzP_adj", "lnMCPP_adj", "lnMEHHP_adj", "lnMEOHP_adj", "lnMiBP_adj",
                                "lnNAP1_adj", "lnNAP2_adj", "lnOHFlu3_adj", "lnOHFlu2_adj", "lnOHPHE1_adj", "lnPYR1_adj")]
names(exp_A1_log) <- gsub("^ln|_adj|_log$", "", names(exp_A1_log))
exp_mlt_A1_log <- melt(exp_A1_log)

PFAS_A1_log <-aim1 [,c("PFHxS_log", "PFDE_log", "PFNA_log")]
names(PFAS_A1_log) <- gsub("_log", "", names(PFAS_A1_log))
PFAS_mlt_A1_log <- melt(PFAS_A1_log)

PAE_A1_log <-aim1 [,c("lnMnBP_adj", "lnMEP_adj", "lnMEHP_adj", "lnMBzP_adj", "lnMCPP_adj", "lnMEHHP_adj", "lnMEOHP_adj", "lnMiBP_adj")]
names(PAE_A1_log) <- gsub("^ln|_adj$", "", names(PAE_A1_log))
PAE_mlt_A1_log <- melt(PAE_A1_log)

PAH_A1_log <-aim1[,c("lnNAP1_adj", "lnNAP2_adj", "lnOHFlu3_adj", "lnOHFlu2_adj", "lnOHPHE1_adj", "lnPYR1_adj")]
names(PAH_A1_log) <- gsub("^ln|_adj$", "", names(PAH_A1_log))
PAH_mlt_A1_log <- melt(PAH_A1_log)


ggplot(PFAS_mlt_A1_log, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +  # Adjust binwidth as needed
  facet_wrap(~variable, scales = "free") +  # Create a separate plot for each variable
  theme_minimal() + 
  labs(title = "Log-transformed Per-fluoroalkyl Substances", x = "PFAS (ng/mL)", y = "Frequency")

ggplot(PAE_mlt_A1_log, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +  # Adjust binwidth as needed
  facet_wrap(~variable, scales = "free") +  # Create a separate plot for each variable
  theme_minimal() + 
  labs(title = "Log-transformed, creatinine-adjusted Phthalates", x = "Phthalates (ug/g creatinine)", y = "Frequency")

ggplot(PAH_mlt_A1_log, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +  # Adjust binwidth as needed
  facet_wrap(~variable, scales = "free") +  # Create a separate plot for each variable
  theme_minimal() + 
  labs(title = "Log-transformed, creatinine-adjusted PAHs", x = "PAHs (ug/g creatinine)", y = "Frequency")



###################################################################################################
###################################################################################################


#All chemicals

# Assign your original chemical variable names as column names
colnames(exp_A1_log) <- c(
  "PFHxS_log", "PFDE_log", "PFNA_log",
  "lnMnBP_adj", "lnMEP_adj", "lnMEHP_adj", "lnMBzP_adj", "lnMCPP_adj", "lnMEHHP_adj", "lnMEOHP_adj", "lnMiBP_adj",
  "lnNAP1_adj", "lnNAP2_adj", "lnOHFlu3_adj", "lnOHFlu2_adj", "lnOHPHE1_adj", "lnPYR1_adj"
)

# Clean names by removing 'ln' prefix and '_adj' or '_log' suffix
clean_names <- colnames(exp_A1_log)
clean_names <- sub("^ln", "", clean_names)               # Remove 'ln' at start
clean_names <- sub("(_adj|_log)$", "", clean_names)      # Remove '_adj' or '_log' at end
colnames(exp_A1_log) <- clean_names

# Rename PAHs for clearer label
colnames(exp_A1_log)[colnames(exp_A1_log) == "PYR1"] <- "1-PYR"
colnames(exp_A1_log)[colnames(exp_A1_log) == "NAP1"] <- "1-NAP"
colnames(exp_A1_log)[colnames(exp_A1_log) == "NAP2"] <- "2-NAP"
colnames(exp_A1_log)[colnames(exp_A1_log) == "OHFlu2"] <- "2-FLUO"
colnames(exp_A1_log)[colnames(exp_A1_log) == "OHFlu3"] <- "3-FLUO"
colnames(exp_A1_log)[colnames(exp_A1_log) == "OHPHE1"] <- "1-PHEN"


# Define the chemical groups
PFAS <- c("PFHxS", "PFDE", "PFNA")
Phthalates <- c("MnBP", "MEP", "MEHP", "MBzP", "MCPP", "MEHHP", "MEOHP", "MiBP")
PAHs <- c("1-NAP", "2-NAP", "2-FLUO", "3-FLUO", "1-PHEN", "1-PYR")


# Match colors based on group
label_colors <- sapply(colnames(exp_A1_log), function(name) {
  if (name %in% PFAS) return("blue")
  else if (name %in% Phthalates) return("darkgreen")
  else if (name %in% PAHs) return("darkred")
  else return("black")  # fallback
})


# Compute Spearman correlation and p-values
exp_corr1_log <- cor(exp_A1_log, 
                     method = "spearman")

cor_pvals <- cor.mtest(exp_A1_log, 
                     method = "spearman",exact=FALSE)

# Define color scale for the correlation plot
col_scale <- colorRampPalette(c("white", "lightpink", "red"))(200)

# Plot the upper triangle: ellipses with significance stars
pvals_for_plot <- cor_pvals$p
pvals_for_plot[pvals_for_plot > 0.05] <- 1

corrplot(exp_corr1_log,
         method = "ellipse",
         type = "upper",
         p.mat = pvals_for_plot,
         sig.level = 0.05,
         insig = "label_sig",
         pch = "*",
         pch.cex = 1,
         pch.col = "black",
         diag = TRUE,
         tl.cex = 0.8,
         tl.pos = "t",
         tl.srt = 45,
         tl.col = label_colors,
         col = col_scale,
         cl.cex = 0.8,
         mar = c(7, 2, 2, 2),
         addrect = 3,       # <<-- Draw 3 rectangles
         rect.col = "navy",
         rect.lwd = 2)

# PFAS box
rect(0.5, 14.5, 3.5, 17.5, border = "blue", lwd = 2)

# Phthalates box
rect(3.5, 6.5, 11.5, 14.5, border = "darkgreen", lwd = 2)

# PAHs box (12–17)
rect(11.5, 0.5, 17.5, 6.5, border = "darkred", lwd = 2)


# Add the lower triangle: correlation coefficients as numbers
corrplot(exp_corr1_log,
         add = TRUE,
         method = "number",
         type = "lower",
         diag = FALSE,
         tl.pos = "n",
         number.cex = 0.7,
         number.digits = 2,
         col = col_scale)

#Repeat boxes
rect(0.5, 14.5, 3.5, 17.5, border = "blue", lwd = 2)   # PFAS
rect(3.5, 6.5, 11.5, 14.5, border = "darkgreen", lwd = 2)   # Phthalates
rect(11.5, 0.5, 17.5, 6.5, border = "darkred", lwd = 2)   # PAHs



legend("right",
       legend = c("PFAS", "Phthalates", "PAHs"),
       col = c("blue", "darkgreen", "darkred"),
       pch = 15,
       pt.cex = 1.5,
       bty = "n",
       cex = 0.8)


###################################################################################################
###################################################################################################
# Create Boxplots

aim1 <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/SAS Datasets/aim1.sas7bdat", NULL)

phthalates<- aim1 [,c("lnMnBP_adj", "lnMEP_adj", "lnMEHP_adj", "lnMBzP_adj", "lnMCPP_adj", "lnMEHHP_adj", "lnMEOHP_adj", "lnMiBP_adj", "CVD_risk")]

PFAS <- aim1[, c("PFHxS_log", "PFDE_log", "PFNA_log", "CVD_risk")]

PAHS <- aim1[, c("lnNAP1_adj", "lnNAP2_adj", "lnOHFlu3_adj", "lnOHFlu2_adj", "lnOHPHE1_adj", "lnPYR1_adj", "CVD_risk")]

EDC <- aim1 [, c("PFHxS_log", "PFDE_log", "PFNA_log", "lnMnBP_adj", "lnMEP_adj", "lnMEHP_adj", "lnMBzP_adj", "lnMCPP_adj", "lnMEHHP_adj", "lnMEOHP_adj", "lnMiBP_adj", 
                           "lnNAP1_adj", "lnNAP2_adj", "lnOHFlu3_adj", "lnOHFlu2_adj", "lnOHPHE1_adj", "lnPYR1_adj", 
                          "CVD_risk")]

###################################################################################################
###################################################################################################


# 1. Remove _log suffix from variable names
EDC <- EDC %>%
  rename_with(~ sub("_adj$", "",  # remove "_adj" at end
              sub("_log$", "",  # remove "_log" at end
              sub("^ln", "", .x) # remove "ln" at start
             )))

# 2. Define chemical groupings
pfas <- c("PFHxS", "PFDE", "PFNA")
phthalates <- c("MnBP", "MEP", "MEHP", "MBzP", "MCPP", "MEHHP", "MEOHP", "MiBP")
pahs <- c("NAP1", "NAP2", "OHFlu2", "OHFlu3", "OHPHE1", "PYR1")

# 3. Reshape data to long format and add group info
EDC_long <- EDC %>%
  pivot_longer(cols = -CVD_risk, names_to = "chemical", values_to = "concentration") %>%
  mutate(
    group = case_when(
      chemical %in% pfas ~ "PFAS",
      chemical %in% phthalates ~ "Phthalates",
      chemical %in% pahs ~ "PAHs",
      TRUE ~ "Other"
    ),
    CVD_risk = factor(CVD_risk, levels = c(0, 1), labels = c("No Risk", "Risk"))
  )

# 4. Logistic regression per chemical
or_edc_table <- EDC_long %>%
  group_by(chemical) %>%
  group_modify(~ tidy(glm(CVD_risk ~ concentration, data = ., family = "binomial"))) %>%
  filter(term == "concentration") %>%
  mutate(signif_label = ifelse(p.value < 0.05, "*", ""))

# --- KEY FIX: Add group info to or_edc_table before label_data ---
or_edc_table <- or_edc_table %>%
  left_join(EDC_long %>% select(chemical, group) %>% distinct(), by = "chemical")

# 5. Get label position slightly above the max
label_positions <- EDC_long %>%
  group_by(chemical) %>%
  summarize(y_pos = max(concentration, na.rm = TRUE) * 1.05, .groups = "drop")

# Create label_data now with group included
label_data <- left_join(or_edc_table, label_positions, by = "chemical")

# 6. Calculate y-axis limits per group for fixed scales
y_limits <- EDC_long %>%
  group_by(group) %>%
  summarize(
    ymin = min(concentration, na.rm = TRUE),
    ymax = max(concentration, na.rm = TRUE) * 1.05,  # add some space for labels
    .groups = "drop"
  )

# 7. Order chemicals (for consistent plotting order)
chemical_order <- c(pfas, phthalates, pahs)
EDC_long$chemical <- factor(EDC_long$chemical, levels = chemical_order)
label_data$chemical <- factor(label_data$chemical, levels = chemical_order)

# 8. Define plotting function by group
plot_group <- function(group_name) {
  data_sub <- filter(EDC_long, group == group_name)
  label_sub <- filter(label_data, group == group_name)
  
  ylim <- y_limits %>% filter(group == group_name) %>% select(ymin, ymax)
  ylim_vec <- c(ylim$ymin, ylim$ymax)
  
  ggplot(data_sub, aes(x = CVD_risk, y = concentration, fill = CVD_risk)) +
    geom_boxplot() +
    facet_wrap(~ chemical, scales = "fixed") +   # fixed y-scale per group
    geom_text(data = label_sub,
              aes(x = 1.5, y = y_pos, label = signif_label),
              inherit.aes = FALSE,
              size = 6,
              fontface = "bold") +
    scale_fill_manual(values = c("No Risk" = "steelblue", "Risk" = "firebrick")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),  # remove major gridlines
      panel.grid.minor = element_blank()   # remove minor gridlines
    ) +
    labs(title = paste0(group_name, " Chemical Concentrations by CVD Risk"),
         x = "Cardiovascular Disease Risk",
         y = "Log Concentration (ng/mL)") +
    coord_cartesian(ylim = ylim_vec)
}

# 9. Generate plots for each group
pfas_plot <- plot_group("PFAS")
phthalates_plot <- plot_group("Phthalates")
pahs_plot <- plot_group("PAHs")

# 10. Combine the plots vertically
combined_plot <- pfas_plot / phthalates_plot / pahs_plot + plot_layout(ncol = 1, heights = c(2, 6, 3))

# Display combined plot
print(combined_plot)

ggsave("EDC_plot.pdf", combined_plot, width = 8, height = 16)




