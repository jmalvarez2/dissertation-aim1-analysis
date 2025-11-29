# ------------------------------------------------------------
# Purpose: Run adjusted Weighted Quantile Sum Regression Analysis
#          and create plots
#
# Created by: Juan M. Alvarez
#
# Input dataset: imputed_aim1_2025_1004.sas7bdat
# ------------------------------------------------------------

library(gWQS)
library(dplyr)
library(ggplot2)
library(haven)
library(stringr)
library(patchwork)

aim1_OR <- read_sas("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/SAS Datasets/imputed_aim1_2025_1004.sas7bdat", NULL)

group_list <- list(
  pfas_vars <- c("PFHxS_log", "PFNA_log", "PFDE_log"),
  
  phthalate_vars <- c("MnBP_log_adj", "MEP_log_adj", "MEHP_log_adj", "MBzP_log_adj",
                      "MCPP_log_adj", "MEHHP_log_adj", "MEOHP_log_adj", "MiBP_log_adj"),
  
  pah_vars <- c("NAP1_log_adj", "NAP2_log_adj", "OHFlu2_log_adj", "OHFlu3_log_adj", 
                "OHPHE1_log_adj", "PYR1_log_adj")
)

chem_groups <- c("PFAS", "Phthalates", "PAHs", "PFAS_PAHs")


# Grouped exposure variables
group_list <- list(
  PFAS = c("PFHxS_log", "PFNA_log", "PFDE_log"),
  Phthalates = c("MBzP_log_adj", "MCPP_log_adj", "MEHHP_log_adj", "MEHP_log_adj", 
                 "MEOHP_log_adj", "MEP_log_adj", "MnBP_log_adj", "MiBP_log_adj"),
  PAHs = c("NAP1_log_adj", "NAP2_log_adj", "OHFlu2_log_adj", "OHFlu3_log_adj", 
           "OHPHE1_log_adj", "PYR1_log_adj"),
  PFAS_PAHs = c("PFHxS_log", "PFNA_log", "PFDE_log", "NAP1_log_adj", "NAP2_log_adj", 
                "OHFlu2_log_adj", "OHFlu3_log_adj", "OHPHE1_log_adj", "PYR1_log_adj"),
  HMW_PAE = c("MBzP_log_adj", "MCPP_log_adj", "MEHHP_log_adj", "MEHP_log_adj", 
              "MEOHP_log_adj"),
  LMW_PAE = c("MEP_log_adj", "MnBP_log_adj", "MiBP_log_adj"),
  DEHP = c("MEHHP_log_adj", "MEHP_log_adj", "MEOHP_log_adj"),
  Prim_PAE = c("MBzP_log_adj", "MEHP_log_adj", "MEP_log_adj", "MnBP_log_adj", "MiBP_log_adj"),
  Sec_PAE = c("MCPP_log_adj", "MEHHP_log_adj", "MEOHP_log_adj"),
  Alt_PAH = c("PFHxS_log", "PFNA_log", "PFDE_log", "NAP1_log_adj", "NAP2_log_adj", 
              "OHPHE1_log_adj", "PYR1_log_adj"),
  Non_alt_PAH = c("OHFlu2_log_adj", "OHFlu3_log_adj"),
  Cancer_PAH = c("NAP1_log_adj", "NAP2_log_adj"),
  Noncancer_PAH = c("OHFlu2_log_adj", "OHFlu3_log_adj", "OHPHE1_log_adj", "PYR1_log_adj")
)


# Function to fit WQS for one group
run_group_wqs <- function(vars, data) {
  gwqs(
    formula = CVD_risk ~ wqs + AGE + racethnic + diabetes_new +
      BMI + education + cot_new + GENDER + PIR + creatinine_log,
    mix_name = vars,
    data = data,
    q = 4,
    validation = 0.6,
    b = 1000,
    b1_pos = TRUE,
    family = "binomial",
    seed = 123
  )
}

# Run for each group
wqs_all <- run_group_wqs(unlist(group_list), aim1_OR)
wqs_pfas <- run_group_wqs(group_list$PFAS, aim1_OR)
wqs_phth <- run_group_wqs(group_list$Phthalates, aim1_OR)
wqs_pah  <- run_group_wqs(group_list$PAHs, aim1_OR)

extract_est <- function(model, group_name) {
  coef_table <- summary(model$fit)$coefficients
  beta <- coef_table["wqs", "Estimate"]
  se   <- coef_table["wqs", "Std. Error"]
  lci  <- beta - 1.96*se
  uci  <- beta + 1.96*se
  data.frame(Group = group_name, beta, lci, uci)
}

group_results <- rbind(
  extract_est(wqs_all, "All EDCs"),
  extract_est(wqs_pfas, "PFAS"),
  extract_est(wqs_phth, "Phthalates"),
  extract_est(wqs_pah,  "PAHs")
)


# Collect results
group_results <- group_results %>%
  mutate(
    OR = exp(beta),
    LCI = exp(lci),
    UCI = exp(uci),
    Group = factor(Group, levels = c("PAHs", "Phthalates", "PFAS", "All EDCs"))
  )

group_results


# Forest plot
forest_plot <- ggplot(group_results, aes(x = OR, y = Group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.0) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  theme_minimal() +
  xlab("OR (95% CI)") +
  ylab("EDC Mixtures") +
  scale_x_continuous(limits = c(0.6, 1.8))

x11()   # Opens a new plotting window on Windows
print(forest_plot)


# Extract weights for barplot


# Convert final_weights to data.frame and add Group column

weights_pfas <- data.frame(wqs_pfas$final_weights, Group = "PFAS")
weights_phth <- data.frame(wqs_phth$final_weights, Group = "Phthalates")
weights_pah  <- data.frame(wqs_pah$final_weights,  Group = "PAHs")

# Combine
weights_df <- rbind(weights_pfas, weights_phth, weights_pah)

# Ensure column names exist
colnames(weights_df)[1:2] <- c("mix_name", "weight")  # first column: chemical, second: weight


# Clean chemical names and enforce desired order

chem_order <- c(
  "PFHxS", "PFNA", "PFDE", "MBzP", "MCPP", "MEHHP", "MEHP",
  "MEOHP", "MEP", "MnBP", "MiBP", "NAP1", "NAP2", "OHFlu2",
  "OHFlu3", "OHPHE1", "PYR1"
)

weights_df <- weights_df %>%
  mutate(
    chem_clean = mix_name %>%
      str_remove("_log_adj") %>%
      str_remove("_log") %>%
      str_remove("_adj") %>%
      str_replace_all("_", "")
  )

# Make factor to enforce desired order
weights_df$chem_clean <- factor(weights_df$chem_clean, levels = chem_order)


# Compute group means for reference lines
group_means <- aggregate(weight ~ Group, data = weights_df, mean)

# Compute x positions for each group's segment
group_positions <- weights_df %>%
  group_by(Group) %>%
  summarize(x_min = min(as.numeric(chem_clean)),
            x_max = max(as.numeric(chem_clean)))

weights_df$Group <- factor(weights_df$Group, levels = c("PFAS", "Phthalates", "PAHs"))

# Vertical barplot with group-specific mean lines

weight_plot <- ggplot(weights_df, aes(x = chem_clean, y = weight, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_segment(data = merge(group_means, group_positions, by = "Group"),
               aes(x = x_min - 0.4, xend = x_max + 0.4, y = weight, yend = weight),
               color = "tomato1", linewidth = 1) +
  theme_minimal() +
  ylab("Estimated weights") +
  xlab("") +
  scale_fill_manual(values = c("steelblue2", "lightgoldenrod3", "palegreen4"),
  name = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

label_map <- c(
  "NAP1" = "1-NAP", "NAP2" = "2-NAP",
  "OHFlu2" = "2-FLUO", "OHFlu3" = "3-FLUO",
  "OHPHE1" = "1-PHEN", "PYR1" = "1-PYR"
)

weight_plot <- weight_plot +
  scale_x_discrete(labels = function(x) ifelse(x %in% names(label_map), label_map[x], x))

# Display plot
x11()
print(weight_plot)


# 6. Combine Panels A + B

library(patchwork)

final_figure <- forest_plot + weight_plot + 
  plot_layout(ncol = 2, widths = c(1, 2)) +
  plot_annotation(tag_levels = 'A') 

# View plot
x11()
final_figure

ggsave("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/Weighted Quantile Sum/WQS_figures.pdf",
       plot = final_figure, width = 15, height = 10, units = "in")







wqs_model_full <- gwqs(
  formula = CVD_risk ~ wqs,
  mix_name = exposure_vars,
  data = aim1_OR,
  q = 4,
  validation = 0,         # <- get WQS index for all rows
  b = 1000,
  family = "binomial"
)

# Extract and add the index directly
aim1_OR$wqs_index <- wqs_model_full$wqs

summary(aim1_OR$wqs_index)












# Remove "All EDCs" from the dataset
plot_data <- group_results %>%
  filter(Group != "All EDCs")

# Now plot
ggplot(plot_data, aes(y = Group, x = OR)) +
  geom_segment(aes(x = LCI, xend = UCI, yend = Group), color = "grey60", linewidth = 1) +
  geom_point(aes(fill = Group), shape = 21, size = 5, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("steelblue2", "lightgoldenrod3", "palegreen4")) +
  theme_minimal(base_size = 13) +
  labs(x = "Odds Ratio (95% CI)", y = NULL,
       title = "Associations Between EDC Mixtures and CVD Risk") +
  theme(legend.position = "none")





library(ggridges)

ggplot(weights_df, aes(x = weight, y = chem_clean, color = Group)) +
  geom_point(position = position_jitter(width = 0.01, height = 0.2), size = 2) +
  facet_wrap(~Group, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(x = "WQS Weight", y = "Chemical", title = "Weights within Each EDC Group") +
  scale_color_manual(values = c("steelblue2", "lightgoldenrod3", "palegreen4"))