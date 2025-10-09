# =====================================================================
# INFERENTIAL ANALYSIS: SIV_positive Only (Association + Correlation)
# ---------------------------------------------------------------------
# For SIV_positive outcome:
#   - For each subset (Husbandry, Animals, Environment, Human)
#     - Univariate association (FDR correction, interactive tables, highlight significant)
#     - Custom statistical test assignment per variable
# Output: association_SIV_positive_by_subset.html
#
# For all variables (not just significant), show correlation with SIV_positive:
#     - Spearman (numeric/binary pairs)
#     - Cramér's V (categorical pairs with >2 levels)
#     - Show: p-value, FDR-p, correlation coefficient, N
# Output: correlation_SIV_positive_by_subset.html
# =====================================================================

library(dplyr)
library(purrr)
library(DT)
library(htmltools)
library(openxlsx)


# ------------------ Variable and Subset Definitions -----------------------

#meta variables that are irrelevant for risk factor analysis
exclude_vars_base <- c(
  "farm_id", "total_samples", "positive_pigs",
  "percent_positive_pigs", "max_ct", "min_ct", "mean_ct", "sd_ct",
  "date_sampling", "source_of_contact_grouped"
)
#include to include if the associated general variable is significant 
overview_dependent_vars <- c(
  "quarantine_time", "outside_area_ai_centre", "outside_area_gestation_stable",
  "outside_area_farrowing_stable", "outside_area_weaner_stable", "outside_area_fattening_stable", "verification_contact_poultry_stable"
)
make_exclude_vars <- function(outcome_var) unique(c(exclude_vars_base, overview_dependent_vars, outcome_var))

get_husbandry_vars <- function(exclude_vars) setdiff(
  c("canton_factor", "herdsize", "production_type_factor",
    "Farrowing_on_farm", "Isemination_on_farm", "Gestation_on_farm", "Weaners_on_farm", "Fattening_on_farm",
    "horses_closeby", "dogs_closeby", "chicken_closeby", "turkey_closeby",
    "cattle_closeby", "cats_closeby", "other_pigs_closeby", "other_poultry_closeby",
    "number_suckling_piglets", "number_weaners", "number_fattening_pigs", "number_young_sows",
    "number_old_sows", "number_boars", "number_of_origins", "quarantine_concept",
    "quarantine_in_herd_contact", "herds_of_origin_respiratory_symptoms", "herds_of_origin_influenza_diagnosis",
    "production_cycle", "mode_stable_occupation_ai_centre", "mode_stable_occupation_gestation_stable",
    "mode_stable_occupation_farrowing_stable", "cross_fostering_farrowing_stable",
    "mode_stable_occupation_weaner_stable", "mode_stable_occupation_fattening_stable",
    "passing_through_other_age_group", "outside_area",
    "outside_area_contact_poultry", "outside_area_contact_wild_birds", "outside_area_contact_wild_boars",
    "contact_bird_in_stable", "cleaning_ai_centre", "cleaning_gestation_stable", "cleaning_farrowing_stable",
    "cleaning_weaner_stable", "cleaning_fattening_stable", "cleaning_quarantine", "disinfection_ai_centre",
    "disinfection_gestation_stable", "disinfection_farrowing_stable", "disinfection_weaner_stable",
    "disinfection_fattening_stable", "disinfection_quarantine", "drying_ai_centre", "drying_gestation_stable",
    "drying_farrowing_stable", "drying_weaner_stable", "drying_fattening_stable", "drying_quarantine",
    "cleaning_disinfection_transport_vehicle", "cleaning_shipment_area", "caretaker_type", "caretaker_number",
    "caretaker_ppe_stable", "caretaker_ppe_washing_interval", "caretaker_ppe_per_unit", "caretaker_per_unit",
    "caretaker_work_flow_hygiene_between_units", "caretaker_entry_ppe_only", "caretaker_disease_management",
    "caretaker_hands_washed_before_entry", "caretaker_boot_disinfection", "caretaker_contact_other_pigs",
    "caretaker_contact_poultry", "visitors_in_stable_recent", "visitors_cumulative_contact_hours",
    "visitors_list", "ppe_visitors", "visitors_hands_washed_before_entry", "visitors_disease_management",
    "visitors_contact_other_pigs", "visitors_respiratory_symptoms", "seperation_between_production_units",
    "seperation_within_production_units", "seperation_quarantine_area", "bird_nests",
    "verification_outside_area_contact_poultry", "verification_outside_area_contact_wild_birds",
    "verification_contact_poultry_stable", "verification_outside_area_contact_wild_boars",
    "farrowing_airspace_with_other_agegroup","ai_airspace_with_other_agegroup",
    "gestation_sows_airspace_with_other_agegroup", "weaners_airspace_with_other_agegroup",
    "fattening_pigs_airspace_with_other_agegroup", "vet_consultation", "influenza_diagnosis", "influenza_vaccination"),
  exclude_vars)
get_animal_vars <- function(exclude_vars) setdiff(
  c("Farrowing_on_farm", "Isemination_on_farm", "Gestation_on_farm", "Weaners_on_farm", "Fattening_on_farm", "symptomatic_report", "ili_signs", "return_to_service_rate", "farrowing_rate", "piglets_per_sow_year",
    "abortions_per_sow_year", "piglet_mortality", "feed_conversion_rate_fattening_pigs",
    "respiratory_history_swine", "time_respiratory_disease", "frequency_respi_outbreak",
    "start_time_current_outbreak", "outbreak_since_examination", "suckling_piglets_diseased",
    "weaners_diseased", "fattening_pigs_diseased", "young_sows_diseased", "old_sows_diseased",
    "boars_diseased", "report_killed_suckling_piglets", "report_killed_weaners",
    "report_killed_fattening_pigs", "report_killed_young_sows", "report_killed_old_sows",
    "symptom_swine_sneezing", "symptom_swine_coughing", "symptom_swine_nasal_discharge",
    "symptom_swine_fever", "symptom_swine_feed_intake_red", "symptom_swine_apathy",
    "symptom_swine_dyspnoea", "Age_weeks_factor", "farrowing_sows_coughing",
    "farrowing_piglets_reduced_general_wellbeing", "farrowing_piglet_litters_sneezing_percentage",
    "farrowing_piglet_litters_coughing_percentage", "weaners_reduced_general_wellbeing",
    "weaners_sneezing", "weaners_coughing", "weaners_discharge", "rectal_temperature_max","rectal_temperature_avg",
    "fattening_pigs_reduced_general_wellbeing", "fattening_pigs_sneezing",
    "fattening_pigs_coughing", "fattening_pigs_discharge"),
  exclude_vars)
get_environment_vars <- function(exclude_vars) setdiff(
  c("Farrowing_on_farm", "Isemination_on_farm", "Gestation_on_farm", "Weaners_on_farm", "Fattening_on_farm", "season_sampling","farrowing_room_temperature", "farrowing_nest_temperature_ok", "farrowing_airflow",
    "farrowing_air_quality", "ai_sows_room_temperature", "ai_sows_airflow", "ai_sows_air_quality",
    "gestation_sows_qm_per_animal", "gestation_sows_animals_per_water_source", "gestation_sows_room_temperature",
    "gestation_sows_airflow", "gestation_sows_air_quality", "weaners_qm_per_animal", "weaners_animals_per_feeding_site_factor",
    "weaners_animals_per_water_source", "weaners_room_temperature", "weaners_airflow",
    "weaners_air_quality", "fattening_pigs_qm_per_animal", "fattening_pigs_feeding_site_per_animal_factor",
    "fattening_pigs_animals_per_water_source", "fattening_pigs_room_temperature", "fattening_pigs_airflow",
    "fattening_pigs_air_quality"),
  exclude_vars)
get_human_vars <- function(exclude_vars) setdiff(
  c("respiratory_history_human", "season_sampling", "respiratory_history_contact_person", "starting_point_current_disease",
    "symptom_human_sneezing", "symptom_human_coughing", "symptom_human_brobchitis",
    "symptom_human_pneumonia", "symptom_human_fever", "symptom_human_headache",
    "symptom_human_myalgia", "symptom_severity", "physician_consultation", "flu_vaccination",
    "flu_vaccination_contacts", "chronic_disease_condition", "smoker"),
  exclude_vars)

# ----- Custom statistical test assignment for continuous vars -------
wilcox_vars <- c(
  "number_fattening_pigs","number_boars","number_of_origins",
  "production_cycle","caretaker_number","visitors_in_stable_recent","visitors_cumulative_contact_hours",
  "start_time_current_outbreak","farrowing_airflow","ai_sows_airflow",
  "gestation_sows_qm_per_animal","gestation_sows_airflow",
  "weaners_sneezing","weaners_coughing","rectal_temperature_max","rectal_temperature_avg","weaners_qm_per_animal",
  "weaners_animals_per_feeding_site_factor","weaners_animals_per_water_source","weaners_airflow","weaners_air_quality","weaners_airspace_with_other_agegroup",
  "fattening_pigs_reduced_general_wellbeing","fattening_pigs_sneezing","fattening_pigs_coughing","fattening_pigs_discharge","fattening_pigs_airflow","fattening_pigs_air_quality","fattening_pigs_airspace_with_other_agegroup",
  "fattening_pigs_feeding_site_per_animal_factor",
  "starting_point_current_disease",
  "farrowing_piglet_litters_sneezing_percentage",
  "farrowing_piglet_litters_coughing_percentage"
)
ttest_vars <- c(
  "herdsize","number_suckling_piglets","number_weaners","number_young_sows","number_old_sows",
  "gitls_animals_per_water_source","gestation_sows_room_temperature",
  "weaners_room_temperature","fattening_pigs_qm_per_animal",
  "fattening_pigs_animals_per_water_source","fattening_pigs_room_temperature",
  "farrowing_room_temperature","ai_sows_room_temperature"
)

chisq_or_fisher <- function(x, y) {
  tbl <- table(x, y)
  if (any(dim(tbl) < 2) || sum(tbl) == 0) return(list(test = "Not analyzable", pval = NA))
  expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl)
  if (any(is.na(expected))) return(list(test = "Not analyzable", pval = NA))
  if (any(expected < 5, na.rm = TRUE)) {
    pval <- tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
    return(list(test = "Fisher's exact", pval = pval))
  } else {
    pval <- tryCatch(chisq.test(tbl)$p.value, error = function(e) NA)
    return(list(test = "Chi-squared", pval = pval))
  }
}

get_num_test <- function(x, y, var_name) {
  idx <- rep(TRUE, length(x))
  # Outlier removal for specific vars (domain knowledge)
  if (var_name == "herdsize") idx <- x != max(x, na.rm=TRUE)
  if (var_name == "number_suckling_piglets") idx <- x != max(x, na.rm=TRUE)
  if (var_name == "number_weaners") idx <- x <= 1000
  if (var_name == "number_young_sows") idx <- x <= 100
  if (var_name == "number_old_sows") idx <- x <= 400
  if (var_name == "gitls_animals_per_water_source") idx <- x <= 25
  if (var_name == "fattening_pigs_qm_per_animal") idx <- x <= 4
  if (var_name == "fattening_pigs_animals_per_water_source") idx <- x <= 30
  x <- x[idx]; y <- y[idx]
  if (var_name %in% wilcox_vars) {
    pval <- tryCatch(wilcox.test(x ~ y, exact = FALSE)$p.value, error = function(e) NA)
    return(list(test = "Wilcoxon Mann-Whitney", pval = pval))
  } else if (var_name %in% ttest_vars) {
    pval <- tryCatch(t.test(x ~ y)$p.value, error = function(e) NA)
    return(list(test = "t-test", pval = pval))
  } else {
    return(list(test = "Not analyzable", pval = NA))
  }
}

perform_test <- function(var, outcome, var_name) {
  if (is.null(var) || length(var) == 0 || all(is.na(var))) {
    return(list(variable = var_name, class = NA, levels = NA, pct_NA = 100, test = "Not analyzable", pval = NA, N = 0))
  }
  df <- data.frame(x = var, y = outcome)
  df <- df[complete.cases(df), ]
  N <- nrow(df)
  if (N == 0) {
    return(list(variable = var_name, class = class(var)[1], levels = NA, pct_NA = 100, test = "Not analyzable", pval = NA, N = 0))
  }
  df$y <- factor(df$y, levels = c(FALSE, TRUE))
  if (is.logical(df$x)) df$x <- factor(df$x, levels = c(FALSE, TRUE))
  na_pct <- mean(is.na(var)) * 100
  var_class <- class(var)
  if (is.factor(var)) {
    var_levels <- paste0(levels(var), collapse = ", ")
  } else if (is.logical(var)) {
    var_levels <- "FALSE, TRUE"
  } else {
    var_levels <- "continuous"
  }
  if (is.factor(var) | is.logical(var)) {
    res <- chisq_or_fisher(df$x, df$y)
  } else if (is.numeric(var)) {
    res <- get_num_test(df$x, df$y, var_name)
  } else {
    res <- list(test = "Not analyzable", pval = NA)
  }
  list(variable = var_name, class = var_class[1], levels = var_levels, pct_NA = round(na_pct, 1), test = res$test, pval = res$pval, N = N)
}

analyze_subset <- function(var_names, subset_name, df, outcome_var) {
  univ_results <- purrr::map(var_names, ~perform_test(df[[.x]], df[[outcome_var]], .x)) %>%
    dplyr::bind_rows()
  univ_results$pval_fdr <- p.adjust(univ_results$pval, method = "fdr")
  univ_results$subset <- subset_name
  univ_results
}

highlight_fdr <- function(pval_fdr) {
  if (is.na(pval_fdr)) return("")
  if (pval_fdr <= 0.1) {
    return(sprintf('<span style="color:green;font-weight:bold;">%.3g</span>', pval_fdr))
  } else {
    return(sprintf('%.3g', pval_fdr))
  }
}
make_dt <- function(results, subset_name, outcome_name) {
  results %>%
    mutate(
      pval = signif(pval, 3),
      pval_fdr_html = purrr::map_chr(pval_fdr, highlight_fdr)
    ) %>%
    dplyr::select(variable, class, levels, pct_NA, test, N, pval, pval_fdr_html) %>%
    datatable(
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        htmltools::tags$strong(
          paste0(subset_name, ": Association with ", outcome_name, " (univariate only)")
        ),
        htmltools::tags$br(),
        htmltools::tags$span(
          style = "font-size: 0.95em; color: #555;",
          "Statistical tests: See 'Test' column. Categorical/discrete: chi-squared or Fisher's exact; continuous: t-test or Wilcoxon.",
          htmltools::tags$br(),
          htmltools::tags$strong(
            "p_FDR: Benjamini-Hochberg FDR correction (values ≤ 0.1 highlighted in green)."
          ),
          htmltools::tags$br(),
          "Missing values: See '% NA'. Outliers may be removed for specific variables (see code)."
        )
      ),
      filter = "top", rownames = FALSE, escape = FALSE,
      options = list(pageLength = 25, autoWidth = TRUE),
      colnames = c("Variable", "Class", "Levels", "% NA", "Test", "N", "p-Value", "p_FDR")
    )
}

# ---------------- Correlation for ALL Variables --------------------
spearman_cor <- function(x, y) {
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  n <- length(x)
  if (n < 3) return(list(rho=NA, pval=NA, n=n))
  ct <- suppressWarnings(cor.test(x, y, method = "spearman"))
  return(list(rho=ct$estimate, pval=ct$p.value, n=n))
}
cramers_v <- function(x, y) {
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  n <- length(x)
  if (n < 3) return(list(V=NA, pval=NA, n=n))
  tbl <- table(x, y)
  if (any(dim(tbl) < 2) || sum(tbl) == 0) return(list(V=NA, pval=NA, n=n))
  chi2test <- tryCatch(chisq.test(tbl, correct = FALSE), error = function(e) NULL)
  if (is.null(chi2test)) return(list(V=NA, pval=NA, n=n))
  chi2 <- chi2test$statistic
  k <- min(nrow(tbl), ncol(tbl))
  V <- sqrt(chi2 / (n * (k - 1)))
  pval <- chi2test$p.value
  return(list(V=as.numeric(V), pval=pval, n=n))
}
is_binary <- function(x) {
  (is.factor(x) && length(levels(x)) == 2) ||
    (is.logical(x)) ||
    (is.numeric(x) && length(unique(na.omit(x))) == 2)
}
is_numeric <- function(x) is.numeric(x)
is_categorical <- function(x) is.factor(x) && length(levels(x)) > 2

get_var_correlation <- function(var_name, df) {
  x <- df[[var_name]]
  y <- df[["SIV_positive"]]
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  n <- length(x)
  # If insufficient data, return NA for all
  if (n < 3) {
    return(list(
      coef = NA, pval = NA, N = n, type = "", html = '<span style="color:gray;" title="N too small">Insufficient data</span>'
    ))
  }
  # Try to determine type
  if ((is_numeric(x) || is_binary(x)) && !is_categorical(x)) {
    # Spearman rank correlation for numeric/binary
    if (is.logical(x)) x <- as.numeric(x)
    if (is.factor(x) && length(levels(x)) == 2) x <- as.numeric(x) - 1
    if (is.logical(y)) y <- as.numeric(y)
    if (is.factor(y) && length(levels(y)) == 2) y <- as.numeric(y) - 1
    res <- spearman_cor(x, y)
    coef <- as.numeric(res$rho)
    pval <- res$pval
    html <- sprintf("%.3f", coef)
    type <- "Spearman"
  } else {
    # Cramér's V for categorical (>2 levels); use chi2 test p-value
    res <- cramers_v(x, y)
    coef <- as.numeric(res$V)
    pval <- res$pval
    html <- sprintf("%.3f", coef)
    type <- "Cramér's V"
  }
  list(
    coef = coef,
    pval = pval,
    N = n,
    type = type,
    html = html
  )
}

make_correlation_results <- function(results, subset_name, df) {
  all_vars <- results$variable
  corrs <- lapply(all_vars, function(v) {
    cor_res <- get_var_correlation(v, df)
    data.frame(
      variable = v,
      N = cor_res$N,
      coef_html = cor_res$html,
      coef = cor_res$coef,
      pval = cor_res$pval,
      test = cor_res$type,
      stringsAsFactors = FALSE
    )
  })
  results_df <- bind_rows(corrs)
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df
}

make_corr_dt <- function(results, subset_name) {
  results %>%
    mutate(
      pval = ifelse(is.na(pval), "", signif(pval, 3)),
      pval_fdr = ifelse(is.na(pval_fdr), "", signif(pval_fdr, 3)),
      coef_html = coef_html
    ) %>%
    dplyr::select(variable, N, coef_html, pval, pval_fdr, test) %>%
    datatable(
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        htmltools::tags$strong(
          paste0(subset_name, ": Correlation with SIV_positive (All variables)")
        ),
        htmltools::tags$br(),
        htmltools::tags$span(
          style = "font-size: 0.95em; color: #555;",
          "All variables (not just significant from association) in this subset are shown.",
          htmltools::tags$br(),
          "Correlation: Spearman for numeric/binary, Cramér's V for categorical (>2 levels).",
          htmltools::tags$br(),
          "FDR correction is applied to correlation p-values where available (Spearman and Cramér's V)."
        )
      ),
      filter = "top", rownames = FALSE, escape = FALSE,
      options = list(pageLength = 25, autoWidth = TRUE),
      colnames = c("Variable", "N", "Correlation", "p-Value", "p_FDR", "Test")
    )
}

# ======================== MAIN ANALYSIS =========================
outcome_var <- "SIV_positive"
exclude_vars <- make_exclude_vars(outcome_var)
husbandry_vars   <- get_husbandry_vars(exclude_vars)
animal_vars      <- get_animal_vars(exclude_vars)
environment_vars <- get_environment_vars(exclude_vars)
human_vars       <- get_human_vars(exclude_vars)

# Association analysis for each subset
results_husbandry   <- analyze_subset(husbandry_vars,   "Husbandry",   df3, outcome_var)
results_animals     <- analyze_subset(animal_vars,      "Animals",     df3, outcome_var)
results_environment <- analyze_subset(environment_vars, "Environment", df3, outcome_var)
results_human       <- analyze_subset(human_vars,       "Human",       df3, outcome_var)

# Save association table HTML for all subsets
dt_husbandry   <- make_dt(results_husbandry,   "Husbandry",   outcome_var)
dt_animals     <- make_dt(results_animals,     "Animals",     outcome_var)
dt_environment <- make_dt(results_environment, "Environment", outcome_var)
dt_human       <- make_dt(results_human,       "Human",       outcome_var)

save_html(
  tagList(
    tags$h2("Univariate Association with SIV_positive by Subset"),
    dt_husbandry, tags$hr(),
    dt_animals, tags$hr(),
    dt_environment, tags$hr(),
    dt_human
  ),
  file = "04_output/association_SIV_positive_by_subset.html"
)

# Correlation table for ALL variables in each subset (not just significant)
corr_husbandry   <- make_correlation_results(results_husbandry,   "Husbandry",   df3)
corr_animals     <- make_correlation_results(results_animals,     "Animals",     df3)
corr_environment <- make_correlation_results(results_environment, "Environment", df3)
corr_human       <- make_correlation_results(results_human,       "Human",       df3)

save_html(
  tagList(
    tags$h2("Correlation of All Variables with SIV_positive by Subset"),
    if (!is.null(corr_husbandry))   make_corr_dt(corr_husbandry,   "Husbandry")   else tags$p("No variables for Husbandry."),
    tags$hr(),
    if (!is.null(corr_animals))     make_corr_dt(corr_animals,     "Animals")     else tags$p("No variables for Animals."),
    tags$hr(),
    if (!is.null(corr_environment)) make_corr_dt(corr_environment, "Environment") else tags$p("No variables for Environment."),
    tags$hr(),
    if (!is.null(corr_human))       make_corr_dt(corr_human,       "Human")       else tags$p("No variables for Human.")
  ),
  file = "04_output/correlation_SIV_positive_by_subset.html"
)





# Create a new workbook
wb <- createWorkbook()

# ================= Association Tables =================
assoc_list <- list(
  Husbandry   = results_husbandry,
  Animals     = results_animals,
  Environment = results_environment,
  Human       = results_human
)

# Add each association table as a sheet
for (subset_name in names(assoc_list)) {
  df <- assoc_list[[subset_name]] %>%
    mutate(pval = signif(pval, 3),
           pval_fdr = signif(pval_fdr, 3))
  addWorksheet(wb, paste0("Assoc_", subset_name))
  writeData(wb, sheet = paste0("Assoc_", subset_name), df)
}

# ================= Correlation Tables =================
corr_list <- list(
  Husbandry   = corr_husbandry,
  Animals     = corr_animals,
  Environment = corr_environment,
  Human       = corr_human
)

# Add each correlation table as a sheet
for (subset_name in names(corr_list)) {
  df <- corr_list[[subset_name]] %>%
    mutate(pval = signif(pval, 3),
           pval_fdr = signif(pval_fdr, 3),
           coef = signif(coef, 3))
  addWorksheet(wb, paste0("Corr_", subset_name))
  writeData(wb, sheet = paste0("Corr_", subset_name), df)
}

# ================= Save Workbook =================
saveWorkbook(wb, file = "04_output/SIV_positive_Assoc_Corr.xlsx", overwrite = TRUE)




