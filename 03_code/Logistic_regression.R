# SIV Logistic Regression Analysis Script (VIP-based Selection, Best Practice)
# Author: jonasalexandersteiner
# Date: 2025-09-17

# --- 0. Load required libraries ---
library(dplyr)
library(pROC)
library(car)
library(ResourceSelection) # For calibration test (Hosmer-Lemeshow)
library(rms)               # For calibration curve/plot

# --- 1. Data Preparation: VIP-based Variable Selection -----------------------

# 1.1 Load PLS-DA results and cleaned/dummy-encoded data
plsda_results <- readRDS("04_output/SIV_PLS_Analysis_Results/plsda_analysis_results_subsets.rds")
# df3_pls must already be loaded in the workspace

# 1.2 Helper: Extract Top N VIP Predictors for a Subset
get_top_vip <- function(plsda_subset, n = 25) {
  plsda_subset$coef_df$Predictor[order(-plsda_subset$coef_df$VIP)][1:n]
}

# 1.3 Get Top 25 VIP predictors for husbandry model
top25_husbandry <- get_top_vip(plsda_results$Husbandry, 25)

# 1.4 Assemble VIP table for transparency and Rmd compatibility
vip_husbandry <- plsda_results$Husbandry$coef_df$VIP
names(vip_husbandry) <- plsda_results$Husbandry$coef_df$Predictor
vip_table_husbandry <- data.frame(
  variable = names(vip_husbandry),
  VIP = as.numeric(vip_husbandry),
  stringsAsFactors = FALSE
)
vip_table_husbandry <- vip_table_husbandry[vip_table_husbandry$variable %in% top25_husbandry, ]

# 1.5 Biological plausibility exclusion: annotate by position (for husbandry)
exclusion_reasons_husbandry <- rep("", length(top25_husbandry))
exclusion_reasons_husbandry[c(2, 5, 6, 8, 9, 12, 13, 14)] <- "more intensive farming"
exclusion_reasons_husbandry[21] <- "missingness indicator"
exclusion_reasons_husbandry[c(23,25)] <- "explained by production_cycle_1"
exclusion_reasons_husbandry[20] <- "explained by litter_equalization_farrowign_stable_2"
exclusion_reasons_husbandry[10] <- "explained by seperation_quarantaine_area_3"
vip_table_husbandry$exclusion_due_to_biological_plausibility <- exclusion_reasons_husbandry

# 1.6 Select final 4 predictors: top 4 by highest VIP, then exclude by rationale
select_final4 <- function(vip_table) {
  vip_table %>%
    arrange(desc(VIP)) %>%
    filter(is.na(exclusion_due_to_biological_plausibility) |
             exclusion_due_to_biological_plausibility == "" |
             exclusion_due_to_biological_plausibility == "-") %>%
    slice_head(n = 4) %>%
    pull(variable)
}
final4_husbandry <- select_final4(vip_table_husbandry)

# 1.7 Prepare modeling dataset (with SIV_positive and symptom_report as confounder)
confounder_var <- "symptomatic_report_TRUE"

husbandry_vars <- unique(c(final4_husbandry, confounder_var, "SIV_positive"))
stopifnot(all(husbandry_vars %in% names(df3_pls)))
df_logit_husbandry <- df3_pls[, husbandry_vars] %>% na.omit()

# 1.8 Ensure SIV_positive is consistently coded as binary factor
prep_siv_factor <- function(x) {
  x <- as.factor(x)
  factor(
    ifelse(x %in% c(1, "1", TRUE, "TRUE", "positive"), "positive",
           ifelse(x %in% c(0, "0", FALSE, "FALSE", "negative"), "negative", NA)),
    levels = c("negative", "positive")
  )
}
df_logit_husbandry$SIV_positive <- prep_siv_factor(df_logit_husbandry$SIV_positive)

# --- Helper for safe formula creation (handles non-syntactic names) ---
backtick_vars <- function(vars) paste0("`", vars, "`")

# --- 2. Fit Logistic Regression Model (VIP-selected predictors + confounder) --------------

# 2.1 Fit logistic regression for husbandry subset
formula_husbandry <- as.formula(
  paste("SIV_positive ~", paste(backtick_vars(setdiff(husbandry_vars, "SIV_positive")), collapse = " + "))
)
fit_husbandry <- glm(formula_husbandry, data = df_logit_husbandry, family = binomial)

# --- 3. Model Summaries, Diagnostics, and Calibration --------------------------------------

# 3.1 Husbandry model diagnostics
summary_fit_husbandry  <- summary(fit_husbandry)
or_ci_husbandry        <- exp(cbind(OR = coef(fit_husbandry), confint(fit_husbandry)))
nullmod_husbandry      <- glm(SIV_positive ~ 1, data = df_logit_husbandry, family = binomial)
pseudo_r2_husbandry    <- 1 - as.numeric(logLik(fit_husbandry)) / as.numeric(logLik(nullmod_husbandry))
roc_obj_husbandry      <- roc(df_logit_husbandry$SIV_positive, fitted(fit_husbandry))
auc_value_husbandry    <- auc(roc_obj_husbandry)
vif_vals_husbandry     <- vif(fit_husbandry)
cooksd_husbandry       <- cooks.distance(fit_husbandry)

# 3.2 Calibration assessment
# Hosmer-Lemeshow test
calib_hl_husbandry <- tryCatch({
  ResourceSelection::hoslem.test(
    as.numeric(df_logit_husbandry$SIV_positive) - 1,  # y must be 0/1
    fitted(fit_husbandry),
    g = 10
  )
}, error = function(e) NULL)

# Calibration plot using rms::calibrate
calib_plot_husbandry <- NULL
calib_rms_husbandry <- NULL
try({
  dd <- rms::datadist(df_logit_husbandry)
  options(datadist = "dd")
  fit_rms <- rms::lrm(formula_husbandry, data = df_logit_husbandry, x = TRUE, y = TRUE)
  calib_rms_husbandry <- rms::calibrate(fit_rms, method = "boot", B = 200)
  calib_plot_husbandry <- function() { plot(calib_rms_husbandry, main = "Calibration Curve (Husbandry Model)") }
}, silent = TRUE)

# --- 4. Save Results for R Markdown ------------------------------------------
output_dir <- "C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/SIV_Projekt/Data_processing/SIV_farms/04_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save(
  summary_fit_husbandry, or_ci_husbandry, pseudo_r2_husbandry, auc_value_husbandry, vif_vals_husbandry, cooksd_husbandry,
  roc_obj_husbandry,
  calib_hl_husbandry, calib_rms_husbandry, calib_plot_husbandry,
  df_logit_husbandry,
  vip_table_husbandry,
  file = file.path(output_dir, "SIV_logistic_regression_results.RData")
)

# Save session info for reproducibility
writeLines(capture.output(sessionInfo()), file.path(output_dir, "SIV_logistic_regression_sessionInfo.txt"))

# --- 5. Render R Markdown Report ---------------------------------------------
library(rmarkdown)
render(
  input = "C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/SIV_Projekt/Data_processing/SIV_farms/03_code/SIV_logistic_regression_results.Rmd",
  output_file = "SIV_logistic_regression_results.html",
  output_dir  = output_dir
)

# --- End of Script ---