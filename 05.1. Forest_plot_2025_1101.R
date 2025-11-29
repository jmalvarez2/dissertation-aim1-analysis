# ------------------------------------------------------------
# Purpose: Graph the results of the fully adjusted
#          logistic regression results of CVD risk for 17 EDCs
#
# Created by: Juan M. Alvarez
#
# Continuation of '05. Regression Analysis_2025_1004.R'
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(writexl)

# ---- Step 0: Define clean labels ----
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

# ---- Step 1: Filter Model 3 and clean chemical names ----
df <- results %>%
  filter(Model == "Model3") %>%
  mutate(
    P_value_label = ifelse(!is.na(P_value), sprintf("%.4g", P_value), "-"),
    Quartiles = case_when(
      grepl("IQR", Type, ignore.case = TRUE) ~ "IQR",
      grepl("Q2", Type) ~ "Q2",
      grepl("Q3", Type) ~ "Q3",
      grepl("Q4", Type) ~ "Q4",
      TRUE ~ "Q1"
    ),
    Chemical = ifelse(Chem %in% names(clean_labels), clean_labels[as.character(Chem)], Chem),
    sig_star = ifelse(P_value < 0.05, "*", "")
  )

# ---- Step 2: Define chemical order and groups ----
pfas <- c("PFHxS", "PFNA", "PFDE")
phthalates <- c("MBzP", "MCPP", "MEHHP", "MEHP", "MEOHP", "MEP", "MnBP", "MiBP")
pahs <- c("1-NAP", "2-NAP", "2-FLUO", "3-FLUO", "1-PHEN", "1-PYR")
chemicals_ordered <- c(pfas, phthalates, pahs)

df$Chemical <- factor(df$Chemical, levels = chemicals_ordered)

df$Group <- case_when(
  df$Chemical %in% pfas ~ "Perfluoroalkyl Substances",
  df$Chemical %in% phthalates ~ "Phthalates",
  df$Chemical %in% pahs ~ "Polycyclic Aromatic Hydrocarbons"
)

# ---- Step 3: Build dataframe with chemical headers ----
df_list <- list()
for (chem in levels(df$Chemical)) {
  # Header row
  df_list[[length(df_list) + 1]] <- data.frame(
    Chemical = chem, Quartiles = NA, OR = NA, LCI = NA, UCI = NA,
    P_value = NA, sig_star = NA, Group = NA, is_header = "chemical"
  )
  # Data rows
  data_chem <- df %>% filter(Chemical == chem)
  data_chem$is_header <- "data"
  df_list[[length(df_list) + 1]] <- data_chem
}

df_plot <- bind_rows(df_list) %>%
  mutate(
    y_pos = rev(seq_len(n())),
    label_left = ifelse(is_header == "chemical", as.character(Chemical), Quartiles),
    sig_star_pos = ifelse(is_header == "data" & sig_star == "*", UCI * 1.05, NA)
  )

# ---- Step 4: Plot ----
plot <- ggplot() +
  geom_point(data = filter(df_plot, is_header == "data" & !is.na(OR)),
             aes(x = OR, y = y_pos, color = Quartiles), size = 3) +
  geom_errorbarh(data = filter(df_plot, is_header == "data" & !is.na(OR)),
                 aes(y = y_pos, xmin = LCI, xmax = UCI, color = Quartiles),
                 height = 0.1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(data = df_plot,
            aes(x = 0.2, y = y_pos, label = label_left,
                fontface = ifelse(is_header == "chemical", "bold", "plain")),
            hjust = 0, size = 3.5) +
  geom_text(data = filter(df_plot, !is.na(sig_star_pos)),
            aes(x = sig_star_pos, y = y_pos, label = sig_star),
            hjust = 0, size = 5, color = "black") +
  annotate("text", x = 0.2, y = max(df_plot$y_pos) + 1.5,
           label = "Chemical exposure", hjust = 0, size = 4.0, fontface = "bold") +
  scale_color_manual(values = c("IQR" = "#5689b4", "Q1" = "#999999", "Q2" = "#1b9e77", 
                                "Q3" = "#d95f02", "Q4" = "#7570b3")) +
  scale_y_continuous(breaks = NULL, expand = expansion(add = 1)) +
  scale_x_continuous(trans = "log10", limits = c(0.1, 8), breaks = c(0.5, 1, 1.5)) +
  labs(x = "Odds Ratio", y = NULL, color = "Quartiles") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 120, b = 10, l = 10)
  )

# ---- Step 5: Display and save ----
x11()
plot

ggsave("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/Regression Analyses/EDC_CVD_Model3_2025_1101.pdf",
       plot = plot, width = 10, height = 30, units = "in")



#--------------------------------------------------------------
# Plot: Continuous Results Only (Group Colors, Tight Layout)
#--------------------------------------------------------------

library(ggplot2)
library(dplyr)

# ---- Step 1: Filter continuous results (IQR) and clean labels ----
df_continuous <- df_plot %>%
  filter(Quartiles == "IQR" & is_header == "data" & !is.na(OR)) %>%
  mutate(label_left = gsub("IQR", "", label_left, ignore.case = TRUE),
         label_left = trimws(label_left))

# Keep group/chemical headers
df_labels <- df_plot %>%
  filter(is_header %in% c("chemical", "Group")) %>%
  mutate(label_left = gsub("IQR", "", label_left, ignore.case = TRUE),
         label_left = trimws(label_left))

# Merge headers and continuous data for plotting
df_combined <- bind_rows(df_labels, df_continuous) %>%
  arrange(y_pos) %>%
  mutate(y_new = seq(from = 1, to = n(), by = 1))  # uniform spacing

# Update continuous data positions to match combined y_new
df_continuous <- df_combined %>%
  filter(Quartiles == "IQR" & is_header == "data")

#--------------------------------------------------------------
# Step 2: Define group colors
#--------------------------------------------------------------
group_colors <- c(
  "Perfluoroalkyl Substances" = "steelblue2",
  "Phthalates" = "lightgoldenrod3",
  "Polycyclic Aromatic Hydrocarbons" = "palegreen4"
)

#--------------------------------------------------------------
# Step 3: Create plot
#--------------------------------------------------------------
p_continuous <- ggplot() +
  
  # Error bars (whiskers)
  geom_errorbarh(data = df_continuous,
                 aes(x = 0.2, y = y_new, xmin = LCI, xmax = UCI, color = Group),
                 height = 0.03, linewidth = 0.8, vjust = 0.5) +
  
  # Points
  geom_point(data = df_continuous,
             aes(x = OR, y = y_new, color = Group),
             size = 3) +
  
  # Reference line at OR = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Chemical and group labels
  geom_text(data = df_combined,
            aes(x = 0.2, y = y_new, label = label_left,
                fontface = ifelse(is_header %in% c("chemical","Group"), "bold", "plain")),
            hjust = 0, size = 3.5, vjust = 0.5) +
  
  # Significance stars
  geom_text(data = filter(df_continuous, !is.na(sig_star_pos)),
            aes(x = sig_star_pos, y = y_new, label = sig_star),
            hjust = 0, size = 5, color = "black", vjust = 0.5) +
  
  # Top annotation
  annotate("text", x = 0.2, y = max(df_combined$y_new) + 1,
           label = "Chemical exposure", hjust = 0, size = 4, fontface = "bold") +
  
  # Axis and scales
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0.02, 0.05))) +
  scale_x_continuous(trans = "log10", limits = c(0.1, 8),
                     breaks = c(0.5, 1, 1.5)) +
  scale_color_manual(values = group_colors) +
  
  # Labels
  labs(x = "Odds Ratio", y = NULL, color = NULL) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 100, b = 10, l = 10)
  )

#--------------------------------------------------------------
# Step 4: Display plot
#--------------------------------------------------------------
x11()
p_continuous

#--------------------------------------------------------------
# Step 5: Save as PDF
#--------------------------------------------------------------
ggsave("C:/Users/juanmabetico/OneDrive/Desktop/Dissertation Files/1.0 Proposal/02. Chapter 2 - Aim 1 (EDCs and Oxidative Stress Mediation)/Tables and Figures/Figures/Regression Analyses/EDC_CVD_Model3_cont_2025_1101.pdf",
       plot = p_continuous, width = 10, height = 10, units = "in")
