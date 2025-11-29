# ------------------------------------------------------------
# Purpose: Plot results of regression analyses of mediators (uric acid, albumin, UAR)
#          and individual EDCs
#
# Created by: Juan M. Alvarez
#
# Input dataset: aim1_OR: created from SAS dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(grid)  # for textGrob

# ---- Step 1: Label and combine datasets ----
results_albumin <- results_albumin %>% mutate(Biomarker = "Albumin")
results_uric    <- results_uric %>% mutate(Biomarker = "Uric Acid")
results_UAR     <- results_UAR %>% mutate(Biomarker = "UAR")

results_all <- bind_rows(results_albumin, results_uric, results_UAR)

# ---- Step 2: Keep only PFDE, PFNA, PFHxS ----
results_filtered <- results_all %>%
  filter(Chem %in% c("PFDE_log", "PFNA_log", "PFHxS_log")) %>%
  mutate(
    Chem = factor(Chem, levels = c("PFDE_log", "PFNA_log", "PFHxS_log")),
    Biomarker = factor(Biomarker, levels = c("Albumin", "Uric Acid", "UAR")),
    sig = ifelse(P_value < 0.05, "*", ""),
    y_sig = UCI + 0.03 * (max(UCI) - min(LCI))  # asterisks above whiskers
  )

# ---- Step 3: Combine Biomarker + Chemical for x-axis grouping ----
results_filtered <- results_filtered %>%
  mutate(
    Combo = paste(Biomarker, Chem, sep = "_"),
    Combo = factor(Combo, levels = c(
      "Albumin_PFDE_log", "Albumin_PFNA_log", "Albumin_PFHxS_log",
      "Uric Acid_PFDE_log", "Uric Acid_PFNA_log", "Uric Acid_PFHxS_log",
      "UAR_PFDE_log", "UAR_PFNA_log", "UAR_PFHxS_log"
    ))
  )

# ---- Step 4: Plot ----

fonts <- list(
  sans = "Helvetica",
  mono = "Consolas",
  `Times New Roman` = "DejaVu Serif"
)

n <- length(levels(results_filtered$Combo))  # number of x-axis levels
y_min <- 0.7
y_max <- 1.3

p_mediator <- ggplot(results_filtered, aes(x = Combo, y = OR, ymin = LCI, ymax = UCI)) +
 
  # horizontal dashed line at OR = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.6) +
  
  # whiskers and points
  geom_errorbar(width = 0.10, size = 0.5, color = "black") +
  geom_point(size = 2.5, color = "black") +
  
  # significance asterisks
  geom_text(aes(y = y_sig, label = sig), size = 5, color = "black", fontface = "bold") +
  
  # vertical separators between biomarker groups
  geom_segment(aes(x = 3.5, xend = 3.5, y = y_min, yend = 1.27),
               linetype = "dashed", color = "gray60", size = 0.6) +
  geom_segment(aes(x = 6.5, xend = 6.5, y = y_min, yend = 1.27),
               linetype = "dashed", color = "gray60", size = 0.6) +

  # y-axis line
  geom_segment(aes(x = 0.5, xend = 0.5, y = y_min, yend = 1.27),
               color = "black", size = 0.6) +
  
  # x-axis line connecting ticks
  geom_segment(aes(x = 0.5, xend = n + 0.5, y = y_min, yend = y_min),
               color = "black", size = 0.6) +
  
  # optional tick marks on x-axis
  geom_segment(x = 1:n, xend = 1:n, y = y_min, yend = y_min - 0.01,
               color = "black", size = 0.6) +
  
  # x-axis labels
  scale_x_discrete(labels = rep(c("PFDE", "PFNA", "PFHxS"), 3)) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = c(0.75, 1.00, 1.25)) +
  labs(y = "Odds Ratio (95% CI)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = -12), size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),  # remove default lines
    axis.ticks = element_blank(), # remove default ticks
    plot.margin = margin(t = 35, r = 10, b = 10, l = 10)
  )

# ---- Step 5: Add biomarker labels above plotting area ----
p_mediator +
  annotation_custom(
    grob = textGrob("Albumin", gp = gpar(fontface = "bold", fontsize = 14)),
    xmin = 1, xmax = 3, ymin = 1.32, ymax = 1.30
  ) +
  annotation_custom(
    grob = textGrob("Uric Acid", gp = gpar(fontface = "bold", fontsize = 14)),
    xmin = 4, xmax = 6, ymin = 1.32, ymax = 1.30
  ) +
  annotation_custom(
    grob = textGrob("UAR", gp = gpar(fontface = "bold", fontsize = 14)),
    xmin = 7, xmax = 9, ymin = 1.32, ymax = 1.30
  )

# Print plot
x11()
p_mediator


# Save PDF
ggsave("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/Regression Analyses/EDC_OS_Model3_2025_1101.pdf",
       plot = p_mediator, width = 10, height = 6, units = "in")
