library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(patchwork)
library(tibble)
library(readr)
library(ggbreak)

# ===== UNIFIED PANEL SETTINGS =====
panel_width <- 24
panel_ncol <- 4
panel_base_size <- 13
panel_axis_text_size <- 12
panel_axis_title_size <- 13
panel_title_size <- 23
panel_group_title_size <- 16
panel_group_title_size_small <- 12
panel_row_height <- 3.2   # per-row height for husbandry/animal health
panel_row_height_env <- 4.2   # per-row height for environment (taller as requested)

# ========== HUSBANDRY PANEL ==========
# ---- Descriptive Table for Continuous/Ordinal ----
cont_vars <- c(
  "herdsize",
  "number_of_origins",
  "caretaker_number",
  "visitors_cumulative_contact_hours"
)
cont_desc <- cont_vars %>% map_df(function(v) {
  map_df(list("All", TRUE, FALSE), function(g) {
    d <- if (g == "All") df3 else df3 %>% filter(symptomatic_report == g)
    x <- d[[v]]
    data.frame(
      variable = gsub("_", " ", v),
      group = case_when(
        g == "All" ~ "All farms",
        g == TRUE  ~ "Farm with symptom report",
        g == FALSE ~ "Farm without report"
      ),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      na_pct = mean(is.na(x)) * 100
    )
  })
})
write_csv(cont_desc, "descriptive_husbandry_continuous.csv")

# ---- Factor Groups ----
factor_groups <- list(
  GENERAL = c("production_type_factor","quarantaine_concept","other_pigs_closeby", "chicken_closeby", "outside_area_contact_poultry"),
  FARROWING_STABLE = c("mode_stable_occupation_farrowing_stable", "litter_equalization_farrowing_stable",
                       "cleaning_farrowing_stable", "desinfection_farrowing_stable", "drying_farrowing_stable"),
  WEANER_STABLE = c("mode_stable_occupation_weaner_stable", "cleaning_weaner_stable",
                    "desinfection_weaner_stable", "drying_weaner_stable"),
  FATTENING_STABLE = c("mode_stable_occupation_fattener_stable", "cleaning_fattener_stable",
                       "desinfection_fattener_stable", "drying_fattener_stable"),
  BIOSECURITY_AND_PERSONNEL = c("outside_area", "outside_area_contact_wild_birds","outside_area_contact_wild_boars", "contact_bird_in_stable",
                                "caretaker_ppe_stable", "caretaker_hands_washed_before_entry", "caretaker_disease_management",
                                "caretaker_boot_desinfection", "caretaker_contact_other_pigs",
                                "visitors_hands_washed_before_entry", "visitors_contact_other_pigs")
)
group_labels <- c(
  GENERAL = "General husbandry",
  FARROWING_STABLE = "Farrowing stable",
  WEANER_STABLE = "Weaner stable",
  FATTENING_STABLE = "Fattening stable",
  BIOSECURITY_AND_PERSONNEL = "Biosecurity & personnel"
)

# ---- Histogram ----
min_herd <- min(df3$herdsize, na.rm = TRUE)
max_herd <- max(df3$herdsize, na.rm = TRUE)
breaks_herd <- seq(floor(min_herd/400)*400, ceiling(max_herd/400)*400, by = 400)

hist_df <- df3 %>%
  filter(!is.na(herdsize)) %>%
  mutate(
    Group = ifelse(symptomatic_report, "Symptom report", "No report"),
    bin = cut(herdsize, breaks = breaks_herd, include.lowest = TRUE, right = FALSE, dig.lab = 4)
  ) %>%
  count(Group, bin) %>%
  complete(Group, bin, fill = list(n = 0))
hist_df$bin <- factor(hist_df$bin, levels = levels(hist_df$bin), ordered = TRUE)

p_hist <- ggplot(hist_df, aes(x = bin, y = n, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.95) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10), expand = expansion(mult = c(0,0.06))) +
  scale_fill_manual(values = c("Symptom report" = "#E41A1C", "No report" = "#4CAF50")) +
  labs(title = "Herd size", x = "Herd size", y = "Number of herds") +
  theme_minimal(base_size = panel_base_size) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = panel_axis_text_size),
    axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
    axis.title.y = element_text(size = panel_axis_title_size),
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 10), hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 24, l = 20),
    legend.position = "none"
  )

# ---- All categorical variable plots ----
all_vars <- unlist(factor_groups)
group_of_var <- rep(names(factor_groups), times = lengths(factor_groups))
plots <- vector("list", length(all_vars))
names(plots) <- all_vars

for (i in seq_along(all_vars)) {
  var <- all_vars[i]
  group <- group_of_var[i]
  all_vals <- df3[[var]]
  if (all(suppressWarnings(!is.na(as.numeric(as.character(na.omit(all_vals))))))) {
    ord_levs <- sort(unique(as.numeric(as.character(na.omit(all_vals)))))
    ord_levs <- as.character(ord_levs)
  } else {
    ord_levs <- sort(unique(as.character(na.omit(all_vals))))
  }
  df_sub <- df3 %>%
    mutate(
      Group = ifelse(symptomatic_report, "Symptom report", "No report"),
      value = as.character(.data[[var]])
    ) %>%
    count(Group, value) %>%
    complete(Group, value = ord_levs, fill = list(n = 0))
  df_sub$value <- factor(df_sub$value, levels = ord_levs, ordered = TRUE)
  var_label <- gsub("_", " ", var)
  plot_title <- if (var == factor_groups[[group]][1]) group_labels[group] else ""
  plots[[i]] <- ggplot(df_sub, aes(x = value, y = n, fill = Group)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Symptom report" = "#E41A1C", "No report" = "#4CAF50")) +
    labs(title = plot_title, x = var_label, y = "Number of herds") +
    theme_minimal(base_size = panel_base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = panel_axis_text_size),
      axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
      axis.title.y = element_text(size = panel_axis_title_size),
      plot.title = element_text(face = "bold", size = if (plot_title == "") panel_group_title_size_small else panel_group_title_size,
                                hjust = 0.5, margin = margin(b = if (plot_title == "") 4 else 18)),
      plot.margin = margin(t = if (plot_title == "") 20 else 32, r = 20, b = 24, l = 20),
      legend.position = "none"
    )
}

nrow_h <- ceiling((length(plots)+1)/panel_ncol)
panel_husbandry <- wrap_plots(c(list(p_hist), plots), ncol = panel_ncol, guides = "collect") &
  plot_annotation(
    title = "Husbandry",
    theme = theme(plot.title = element_text(size = panel_title_size, face = "bold", hjust = 0.5),
                  plot.margin = margin(b = 20))
  ) &
  theme(legend.position = "bottom",
        plot.margin = margin(t = 24, r = 10, b = 24, l = 10),
        plot.title = element_text(margin = margin(b = 12)),
        plot.background = element_rect(fill = "white", color = NA)
  )
# ========== ANIMAL HEALTH PANEL ==========
# ---- Factor/Binary Groups ----
factor_groups_animal <- list(
  GENERAL = c("time_respiratory_disease", "outbreak_since_examination", "influenza_vaccination"),
  SYMPTOMS = c("symptom_swine_sneezing", "symptom_swine_coughing", "symptom_swine_nasal_discharge", 
               "symptom_swine_fever", "symptom_swine_feed_intake_red", "symptom_swine_apathy", "symptom_swine_dyspnoea"),
  SIGNS = c("weaners_reduced_general_wellbeing", "fatteners_reduced_general_wellbeing")
)
group_labels_animal <- c(
  GENERAL = "General",
  SYMPTOMS = "Symptoms",
  SIGNS = "Clinical Examination"
)

all_vars_animal <- unlist(factor_groups_animal)
group_of_var_animal <- rep(names(factor_groups_animal), times = lengths(factor_groups_animal))
plots_animal <- vector("list", length(all_vars_animal))
names(plots_animal) <- all_vars_animal

for (i in seq_along(all_vars_animal)) {
  var <- all_vars_animal[i]
  group <- group_of_var_animal[i]
  all_vals <- df3[[var]]
  if (all(suppressWarnings(!is.na(as.numeric(as.character(na.omit(all_vals))))))) {
    ord_levs <- sort(unique(as.numeric(as.character(na.omit(all_vals)))))
    ord_levs <- as.character(ord_levs)
  } else {
    ord_levs <- sort(unique(as.character(na.omit(all_vals))))
  }
  df_sub <- df3 %>%
    mutate(
      Group = ifelse(symptomatic_report, "Symptom report", "No report"),
      value = as.character(.data[[var]])
    ) %>%
    count(Group, value) %>%
    complete(Group, value = ord_levs, fill = list(n = 0))
  df_sub$value <- factor(df_sub$value, levels = ord_levs, ordered = TRUE)
  var_label <- gsub("_", " ", var)
  plot_title <- if (var == factor_groups_animal[[group]][1]) group_labels_animal[group] else ""
  plots_animal[[i]] <- ggplot(df_sub, aes(x = value, y = n, fill = Group)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Symptom report" = "#E41A1C", "No report" = "#4CAF50")) +
    labs(title = plot_title, x = var_label, y = "Number of farms") +
    theme_minimal(base_size = panel_base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = panel_axis_text_size),
      axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
      axis.title.y = element_text(size = panel_axis_title_size),
      plot.title = element_text(face = "bold", size = if (plot_title == "") panel_group_title_size_small else panel_group_title_size,
                                hjust = 0.5, margin = margin(b = if (plot_title == "") 4 else 18)),
      plot.margin = margin(t = if (plot_title == "") 20 else 32, r = 20, b = 24, l = 20),
      legend.position = "none"
    )
}

# ---- Insert boxplots for continuous/ordinal variables in SIGNS section ----
cont_vars_animal <- c(
  "weaners_sneezing",
  "weaners_coughing",
  "fatteners_sneezing",
  "fatteners_coughing",
  "rectal_temperature_max",
  "rectal_temperature_avg"
)
cont_pretty_labels <- c(
  "Weaners sneezing",
  "Weaners coughing",
  "Fatteners sneezing",
  "Fatteners coughing",
  "Rectal temp. max",
  "Rectal temp. avg"
)
# Add y-axis units for each variable
cont_yaxis_units <- c("index (%/min)", "index (%/min)", "index (%/min)", "index (%/min)", "°C", "°C")

cont_boxplots_animal <- purrr::pmap(list(cont_vars_animal, cont_pretty_labels, cont_yaxis_units), function(var, pretty_label, y_unit) {
  p <- df3 %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(Group = ifelse(symptomatic_report, "Symptom report", "No report")) %>%
    ggplot(aes(x = Group, y = .data[[var]], fill = Group)) +
    geom_boxplot(
      alpha = 0.8,
      color = "black",
      width = 0.6,
      outlier.size = 0.8,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Symptom report" = "#E41A1C", "No report" = "#4CAF50")) +
    labs(title = NULL, x = pretty_label, y = y_unit) +
    theme_minimal(base_size = panel_base_size) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
      axis.title.y = element_text(size = panel_axis_title_size),
      plot.title = element_blank(),
      plot.margin = margin(20, 20, 24, 20)
    )
  p
})

# Insert boxplots after 'fatteners_reduced_general_wellbeing'
insert_after <- which(names(plots_animal) == "fatteners_reduced_general_wellbeing")
plots_animal <- append(plots_animal, cont_boxplots_animal, after = insert_after)

nrow_a <- ceiling(length(plots_animal)/panel_ncol)
panel_animal <- wrap_plots(plots_animal, ncol = panel_ncol, guides = "collect") &
  plot_annotation(
    title = "Animal health",
    theme = theme(plot.title = element_text(size = panel_title_size, face = "bold", hjust = 0.5),
                  plot.margin = margin(b = 20))
  ) &
  theme(legend.position = "bottom",
        plot.margin = margin(t = 24, r = 10, b = 24, l = 10),
        plot.title = element_text(margin = margin(b = 12)),
        plot.background = element_rect(fill = "white", color = NA)
  )


# ========== ENVIRONMENT PANEL ==========
# ---- No table needed here ----
df3$Group <- factor(
  ifelse(df3$symptomatic_report, "Farm with symptom report", "Farm without report"),
  levels = c("Farm with symptom report", "Farm without report")
)

# --- 1. Barplot of season_sampling, stacked by Group (variable name as x-axis label)
bar_season <- df3 %>%
  mutate(season_sampling = as.character(season_sampling)) %>%
  group_by(season_sampling, Group) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = season_sampling, y = n, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  scale_fill_manual(values = c("Farm with symptom report" = "#E41A1C", "Farm without report" = "#4CAF50")) +
  labs(
    title = NULL,
    x = "Sampling season",
    y = "Number of farms",
    fill = "Group"
  ) +
  theme_minimal(base_size = panel_base_size) +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = panel_axis_text_size),
    axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
    axis.title.y = element_text(size = panel_axis_title_size),
    plot.margin = margin(20, 20, 24, 20)
  )

cont_vars_env <- c(
  "farrowing_room_temperature",
  "farrowing_airflow",
  "weaners_room_temperature",
  "weaners_airflow",
  "fatteners_room_temperature",
  "fatteners_airflow"
)
pretty_names_env <- c(
  "Farrowing room temperature",
  "Farrowing airflow",
  "Weaners room temperature",
  "Weaners airflow",
  "Fatteners room temperature",
  "Fatteners airflow"
)
y_limits_env <- lapply(cont_vars_env, function(var) {
  rng <- range(df3[[var]], na.rm = TRUE)
  rng + c(-0.05, 0.05) * diff(rng)
})

cont_plots_env <- purrr::pmap(
  list(cont_vars_env, pretty_names_env, y_limits_env),
  function(var, pretty, ylim) {
    df3 %>%
      filter(!is.na(.data[[var]]), !is.na(season_sampling)) %>%
      ggplot(aes(x = as.character(season_sampling), y = .data[[var]], fill = Group)) +
      geom_boxplot(
        position = position_dodge(width = 0.7),
        outlier.size = 0.8,
        alpha = 0.8,
        color = "black",
        width = 0.5,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = c("Farm with symptom report" = "#E41A1C", "Farm without report" = "#4CAF50")) +
      labs(title = NULL, x = pretty, y = NULL) +
      coord_cartesian(ylim = ylim) +
      theme_minimal(base_size = panel_base_size) +
      theme(
        plot.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = panel_axis_text_size),
        axis.title.x = element_text(size = panel_axis_title_size, margin = margin(t = 10)),
        plot.margin = margin(20, 20, 24, 20)
      )
  }
)

nrow_e <- ceiling((length(cont_plots_env)+1)/panel_ncol)
panel_environment <- wrap_plots(c(list(bar_season), cont_plots_env), ncol = panel_ncol, guides = "collect") &
  plot_annotation(
    title = "Environment",
    theme = theme(plot.title = element_text(size = panel_title_size, face = "bold", hjust = 0.5),
                  plot.margin = margin(b = 20))
  ) &
  theme(legend.position = "bottom",
        plot.margin = margin(t = 24, r = 10, b = 24, l = 10),
        plot.title = element_text(margin = margin(b = 12)),
        plot.background = element_rect(fill = "white", color = NA)
  )

# ========== SAVE ALL PANELS ==========
ggsave("big_panel_husbandry.svg", panel_husbandry, width = panel_width, height = panel_row_height * nrow_h)
ggsave("big_panel_animal_health.svg", panel_animal, width = panel_width, height = panel_row_height * nrow_a)
ggsave("big_panel_environment.svg", panel_environment, width = panel_width, height = panel_row_height_env * nrow_e)



# ===================
# INFERENTIAL TABLES
# ==================

# Helper: Top 25 by association p-value (excluding NA), merged with correlation and formatted values
output_top25_inferential <- function(results_assoc, results_corr, filename, top_n = 25) {
  # Top 25 by association p-value (excluding NA)
  assoc_valid <- results_assoc %>% filter(!is.na(pval))
  top_vars <- assoc_valid %>%
    arrange(pval) %>%
    slice_head(n = top_n) %>%
    pull(variable)
  # Subset both association and correlation to these variables
  assoc_sub <- results_assoc %>%
    filter(variable %in% top_vars) %>%
    transmute(
      Variable = variable,
      N = formatC(N, format = "f", digits = 2),
      `NA%` = formatC(pct_NA, format = "f", digits = 2),
      Test = test,
      `p-value (association)` = ifelse(is.na(pval), "", formatC(pval, format = "f", digits = 2)),
      `FDR p-value (association)` = ifelse(is.na(pval_fdr), "", formatC(pval_fdr, format = "f", digits = 2))
    )
  cor_sub <- results_corr %>%
    filter(variable %in% top_vars) %>%
    transmute(
      Variable = variable,
      Correlation = ifelse(is.na(coef), "", formatC(coef, format = "f", digits = 2)),
      `Test (correlation)` = test
    )
  merged <- left_join(assoc_sub, cor_sub, by = "Variable")
  # Sort by p-value
  merged <- merged %>% arrange(`p-value (association)`)
  # Write to CSV
  write.csv(merged, filename, row.names = FALSE, na = "")
}

output_top25_inferential(results_husbandry,   corr_husbandry,   "publication_table_husbandry.csv")
output_top25_inferential(results_animals,     corr_animals,     "publication_table_animal_health.csv")
output_top25_inferential(results_environment, corr_environment, "publication_table_environment.csv")
output_top25_inferential(results_human,       corr_human,       "publication_table_human.csv")


# Publication Outputs for SIV PLS-DA: Husbandry & Animals
# Generates merged VIP/risk table, confusion matrix (wide format), metrics, and combined score/ROC plot for each subset.
# Exports only the TOP 20 VIP predictors to a single CSV file for both subsets.
# Combines confusion matrices and score/ROC plots into single outputs.
# Subset headings in plots are visually larger than plot titles.
# Annotated throughout for clarity.

library(tidyverse)
library(mixOmics)
library(pROC)
library(ggplot2)
library(gridExtra)
library(grid) # For custom grob (large heading)

# Path to the RDS results file
rds_path <- "C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/SIV_Projekt/Data_processing/SIV_farms/04_output/SIV_PLS_Analysis_Results/plsda_analysis_results_subsets.rds"

# Read in the results object containing both subsets
subset_results <- readRDS(rds_path)

# Output directory for all results
output_dir <- "Publication_Outputs"
dir.create(output_dir, showWarnings = FALSE)

#---------------------------
# Function: Calculate metrics from truth/prediction
#---------------------------
calc_metrics <- function(truth, pred_class, probs) {
  truth <- factor(truth, levels = c(FALSE, TRUE))
  pred_class <- factor(pred_class, levels = c(FALSE, TRUE))
  conf_mat <- table(True = truth, Predicted = pred_class)
  sens <- if ("TRUE" %in% rownames(conf_mat) && "TRUE" %in% colnames(conf_mat) && sum(conf_mat["TRUE", ]) > 0)
    conf_mat["TRUE", "TRUE"] / sum(conf_mat["TRUE", ]) else NA
  spec <- if ("FALSE" %in% rownames(conf_mat) && "FALSE" %in% colnames(conf_mat) && sum(conf_mat["FALSE", ]) > 0)
    conf_mat["FALSE", "FALSE"] / sum(conf_mat["FALSE", ]) else NA
  balanced_acc <- mean(c(sens, spec), na.rm = TRUE)
  auc <- tryCatch({
    roc_obj <- roc(truth, probs, levels = c(FALSE, TRUE), direction = "<")
    as.numeric(pROC::auc(roc_obj))
  }, error = function(e) NA)
  tibble(Metric = c("Sensitivity", "Specificity", "Balanced Accuracy", "AUC"),
         Value = round(c(sens, spec, balanced_acc, auc), 3))
}

#---------------------------
# Function: Merge VIP, loadings, and Risk/Protective assignment
#---------------------------
get_merged_table <- function(res, subset_name) {
  vip_table <- as.data.frame(res$coef_df)
  if (!"Predictor" %in% colnames(vip_table)) colnames(vip_table)[1] <- "Predictor"
  loadings <- res$loadings_df[, 1, drop = TRUE]
  predictors <- vip_table$Predictor
  # Ensure loading names match predictors
  if (is.null(names(loadings)) || any(names(loadings) == "")) names(loadings) <- rownames(res$loadings_df)
  loadings_vec <- loadings[predictors]
  # Risk/Protective assignment logic (note: direction differs by subset!)
  if (subset_name == "Husbandry") {
    assign_type <- ifelse(loadings_vec < 0, "Risk", "Protective")
  } else if (subset_name == "Animals") {
    assign_type <- ifelse(loadings_vec > 0, "Risk", "Protective")
  } else {
    assign_type <- rep(NA, length(loadings_vec))
  }
  risk_table <- tibble(Predictor = predictors,
                       Loading = as.numeric(loadings_vec),
                       Risk_Protective = assign_type)
  merged <- left_join(vip_table, risk_table, by = "Predictor")
  merged <- merged %>% arrange(desc(VIP))
  merged
}

#---------------------------
# Function: PLS-DA component score plot
#---------------------------
make_score_plot <- function(scores_df, SIV_positive, label) {
  if (!"SIV_positive" %in% colnames(scores_df)) {
    scores_df$SIV_positive <- factor(SIV_positive, levels = c(FALSE, TRUE), labels = c("negative", "positive"))
  }
  scores_df$SIV_positive <- factor(scores_df$SIV_positive, levels = c("negative", "positive"))
  ggplot(scores_df, aes(x = `Comp 1`, y = `Comp 2`, color = SIV_positive)) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(aes(group = SIV_positive), linetype = 2, size = 1) +
    labs(
      title = paste("PLS-DA Score Plot"),
      x = "Component 1", y = "Component 2", color = "SIV Status"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(size = 18))
}

#---------------------------
# Function: ROC curve plot
#---------------------------
make_roc_plot <- function(truth, probs, label) {
  roc_obj <- tryCatch(roc(truth, probs, levels = c(FALSE, TRUE), direction = "<"), error = function(e) NULL)
  auc_val <- if (!is.null(roc_obj)) round(as.numeric(pROC::auc(roc_obj)), 3) else NA
  roc_df <- if (!is.null(roc_obj)) {
    tibble(Specificity = 1 - roc_obj$specificities, Sensitivity = roc_obj$sensitivities)
  } else {
    tibble(Specificity = c(0,1), Sensitivity = c(0,1))
  }
  ggplot(roc_df, aes(x = Specificity, y = Sensitivity)) +
    geom_line(color = "blue", size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    labs(
      title = paste("ROC Curve\nAUC =", auc_val),
      x = "1 - Specificity", y = "Sensitivity"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(size = 18))
}

#---------------------------
# Containers for combined results
#---------------------------
all_merged_tables <- tibble()
all_conf_mats <- tibble()
all_metrics <- tibble()
all_panels <- list()

#---------------------------
# Main loop over both subsets
#---------------------------
for (subset_name in c("Husbandry", "Animals")) {
  res <- subset_results[[subset_name]]
  label <- ifelse(subset_name == "Husbandry", "Husbandry", "Animal Health")
  
  # 1. Merged VIP + Risk/Protective table, only top 20 VIP predictors
  merged_table <- get_merged_table(res, subset_name)
  merged_table_top <- head(merged_table, 20) # Top 20 VIP predictors
  merged_table_top$Subset <- label
  all_merged_tables <- bind_rows(all_merged_tables, merged_table_top)
  
  # 2. Confusion matrix & Performance metrics (wide format for confusion matrix)
  ncomp <- res$ncomp_optimal
  pred <- predict(res$plsda_model, res$X, dist = "max.dist", comp = ncomp)
  pred_class <- pred$class$max.dist[, ncomp]
  probs <- pred$predict[, "TRUE", ncomp]
  truth <- res$SIV_positive
  
  conf_mat <- table(True = truth, Predicted = pred_class)
  conf_mat_df <- as.data.frame.matrix(conf_mat)
  conf_mat_df$Subset <- label
  all_conf_mats <- bind_rows(all_conf_mats, conf_mat_df)
  
  metrics <- calc_metrics(truth, pred_class, probs)
  metrics$Subset <- label
  all_metrics <- bind_rows(all_metrics, metrics)
  
  # 3. Combined Score plot + ROC curve panel
  score_plot <- make_score_plot(res$scores_df, res$SIV_positive, label)
  roc_plot <- make_roc_plot(truth, probs, label)
  
  # Subset heading grob, bigger than plot titles
  subset_heading <- textGrob(label, gp = gpar(fontsize = 28, fontface = "bold"))
  
  # Combine plots under large heading
  panel <- arrangeGrob(
    score_plot, roc_plot,
    ncol = 2,
    top = subset_heading
  )
  all_panels[[subset_name]] <- panel
  
  # 4. Print top predictors to console for each subset
  cat("\n", label, "Top 20 predictors (Risk/Protective, VIP, Loading):\n")
  print(merged_table_top)
  cat("\nPerformance metrics:\n")
  print(metrics)
}

#---------------------------
# Write all results to files
#---------------------------

# Top 20 VIP predictors table for both subsets
write.csv(all_merged_tables, file.path(output_dir, "Top20_MergedTable_Combined.csv"), row.names = FALSE)

# Confusion matrices for both subsets (wide format)
write.csv(all_conf_mats, file.path(output_dir, "ConfusionMatrix_Combined.csv"), row.names = FALSE)

# Performance metrics for both subsets
write.csv(all_metrics, file.path(output_dir, "PerformanceMetrics_Combined.csv"), row.names = FALSE)

# Combined score/ROC plot panels for both subsets in one PNG
combined_panel <- arrangeGrob(grobs = all_panels, ncol = 1)
ggsave(file.path(output_dir, "Score_ROC_Combined.png"), combined_panel, width = 14, height = 12)

#---------------------------
# Final message
#---------------------------
cat("\nTop 20 VIP tables, confusion matrices, performance metrics, and combined score/ROC panels exported to CSV/PNG in", output_dir, "\n")


# SIV Logistic Regression: Export Exploratory Tables & ROC Plots Panel (No p-values)
# Author: jonasalexandersteiner
# Date: 2025-09-25 (husbandry-only version)

library(dplyr)
library(pROC)
library(ggplot2)
library(gridExtra)

# --- 1. Output folder ---
output_dir <- "Publication_Outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, showWarnings = FALSE)

# --- 2. Model coefficients table function (no p-values) ---
extract_coef_table <- function(fit, subset_name, n) {
  if (is.null(fit)) stop(paste("Model object for", subset_name, "is NULL!"))
  coefs <- coef(summary(fit))
  or_ci <- exp(cbind(OR = coef(fit), confint(fit)))
  tab <- data.frame(
    Predictor = rownames(coefs),
    Estimate = round(coefs[, "Estimate"], 3),
    Std_Error = round(coefs[, "Std. Error"], 3),
    Odds_Ratio = round(or_ci[, "OR"], 3),
    CI_2.5 = round(or_ci[, "2.5 %"], 3),
    CI_97.5 = round(or_ci[, "97.5 %"], 3),
    Subset = subset_name,
    N = n,
    row.names = NULL
  )
  tab
}

# --- 3. ROC Plot function ---
plot_roc <- function(roc_obj, label) {
  if (is.null(roc_obj) || !inherits(roc_obj, "roc")) {
    warning(paste("ROC object for", label, "is missing or invalid. Empty plot returned."))
    return(ggplot() + labs(title = paste("ROC Curve:", label, "\n(no data)")) + theme_void())
  }
  auc_val <- round(as.numeric(auc(roc_obj)), 3)
  roc_df <- tibble(Specificity = 1 - roc_obj$specificities, Sensitivity = roc_obj$sensitivities)
  ggplot(roc_df, aes(x = Specificity, y = Sensitivity)) +
    geom_line(color = "blue", size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    labs(title = paste("ROC Curve:", label, "- AUC =", auc_val),
         x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal(base_size = 15)
}

# --- 4. Export tables and combined ROC panel ---

required_objs <- c("fit_husbandry", "roc_obj_husbandry", "df_logit_husbandry")
missing <- required_objs[!sapply(required_objs, function(obj) exists(obj, inherits = TRUE))]
if (length(missing) > 0) {
  stop("Missing required objects: ", paste(missing, collapse = ", "))
}

# Husbandry subset only
n_h <- nrow(df_logit_husbandry)
coef_tab_h <- extract_coef_table(fit_husbandry, "Husbandry", n_h)
write.csv(coef_tab_h, file.path(output_dir, "Logit_Coefficients_Husbandry.csv"), row.names = FALSE)

# ROC panel (just one plot)
roc_plot_h <- plot_roc(roc_obj_husbandry, "Husbandry")
if (!inherits(roc_plot_h, "grob")) roc_plot_h <- ggplotGrob(roc_plot_h)
# Single plot: still use grid.arrange for consistency
panel <- gridExtra::grid.arrange(roc_plot_h, ncol = 1)
ggsave(file.path(output_dir, "ROC_Panel_Logit.png"), panel, width = 7, height = 6)

# Save session info for reproducibility
writeLines(capture.output(sessionInfo()), file.path(output_dir, "logit_export_sessionInfo.txt"))

cat("Exported: model coefficients table and ROC panel to", output_dir, "\n")