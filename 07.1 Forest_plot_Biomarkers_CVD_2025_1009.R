# ------------------------------------------------------------
# Purpose: Plot regression analysis results of mediators 
#          (uric acid, albumin, and UAR) and CVD risk
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

#==============================================================
# Figure: Association of biomarkers with CVD risk
#==============================================================

library(dplyr)
library(ggplot2)
library(grid)

# Example data (replace with your real results_cvd dataframe)
results_cvd <- data.frame(
  Chem = c("UAR", "albumin_log", "uric_log"),
  Model = c("Model3", "Model3", "Model3"),
  OR = c(1.396514, 1.056727, 2.564857),
  LCI = c(0.9226413, 0.9639998, 1.5054629),
  UCI = c(2.121630, 1.158759, 4.400789),
  P_value = c(0.1155398815, 0.2393198379, 0.0005717643)
)

#==============================================================
# Step 1: Prepare dataframe
#==============================================================

results_cvd <- results_cvd %>%
  mutate(
    Biomarker = case_when(
      Chem == "albumin_log" ~ "Albumin",
      Chem == "uric_log" ~ "Uric Acid",
      Chem == "UAR" ~ "UAR",
      TRUE ~ Chem
    ),
    Biomarker = factor(Biomarker, levels = c("Albumin", "Uric Acid", "UAR")),
    sig = ifelse(P_value < 0.05, "*", ""),
    y_sig = UCI + 0.05 * (max(UCI) - min(LCI))
  )

#==============================================================
# Step 2: Plot
#==============================================================

y_min <- 0.7
y_max <- 5.0
n <- length(levels(results_cvd$Biomarker))

p_cvd <- ggplot(results_cvd, aes(x = Biomarker, y = OR, ymin = LCI, ymax = UCI)) +
  # Reference line at OR = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.6) +
  
  # Error bars and points
  geom_errorbar(width = 0.10, size = 0.5, color = "black") +
  geom_point(size = 3, color = "black") +
  
  # Asterisks for significance
  geom_text(aes(y = y_sig, label = sig), size = 6, color = "black", fontface = "bold") +
  
  # Add manual x-axis line and ticks
  geom_segment(aes(x = 0.5, xend = n + 0.5, y = y_min, yend = y_min),
               color = "black", size = 0.6) +
  geom_segment(x = 1:n, xend = 1:n, y = y_min, yend = y_min - 0.05,
               color = "black", size = 0.6) +
  
  # Add manual y-axis line (up to 4)
  geom_segment(aes(x = 0.5, xend = 0.5, y = y_min, yend = 5),
               color = "black", size = 0.6) +
  
  # Custom axes
  scale_y_continuous(limits = c(y_min, y_max), breaks = seq(1, 5, 1)) +
  scale_x_discrete(labels = c("Albumin", "Uric Acid", "UAR")) +
  
  labs(y = "OR of CVD risk (95% CI)") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                               size = 12, face = "bold", margin = margin(t = -12)),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 35, r = 10, b = 20, l = 10)
  )

# Print plot
x11()
p_cvd


#==============================================================
# Step 3: Display plot
#==============================================================

p_cvd
p_mediator




# Stack plots vertically
figure_regressions <- p_cvd / p_mediator + 
  plot_layout(ncol = 1, heights = c(1, 2)) +  
  plot_annotation(tag_levels = 'A')           

# Display the stacked figure
figure_regressions

# Save PDF
ggsave("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/Regression Analyses/OS_CVD_Model3_2025_1101.pdf",
       plot = p_cvd, width = 4, height = 6, units = "in")


