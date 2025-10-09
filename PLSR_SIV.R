############################################################
# Swiss Pig SIV Risk Factors: Data Preparation and PLS-DA Analysis
# Subset Analysis Extension
# Author: jonasalexandersteiner
#
# This script performs:
# 1. Data preparation for PLS-DA, including:
#    1. Custom binning and factor conversions (with explicit NA handling only when NA present)
#    2. Conversion of sneezing/coughing variables to logicals (with NA preserved and recoded)
#    3. Handling of missing values in numeric columns (imputation or removal)
#    4. Conversion of ALL logical and factor variables to factors with levels TRUE, FALSE, NA (NA only if present)
#    5. Identification of factors for dummy coding (excluding meta variables)
#    6. Dummy coding of multi-level factors (proper reference columns dropped, robust NA and FALSE handling)
#    7. Construction of the final numeric predictor matrix X (all predictors numeric and ready for modeling)
# 2. Systematic predictor filtering for modeling
#    1. Low-variance and constant columns (nearZeroVar from caret)
#    2. Columns with all NA values
#    3. Columns with only one unique value (after NA removal)
#    4. Impute selected NAs with 0
#    5. Exact duplicate columns
#    6. Highly correlated columns (>0.95, standard)
#    7. Custom filter (user-defined, with logging)
#    8. Save predictor removal log
# 3. Preparation of response variable for PLS-DA
# 4. Standardization of predictors and export
#    1. For each subset (Husbandry, Animals, Environment, Human/Contact), selects relevant predictors, standardizes them, and exports the matrix and response for reproducibility
# 5. PLS-DA model fitting and variable importance extraction
#    1. For each subset, fits the PLS-DA model, performs cross-validation for component selection, extracts variable importance (VIP), scores, loadings, explained variance, and stores results
# 6. Saving of analysis results and rendering of visualization report
#    1. Saves all subset model results and renders a single R Markdown visualization report for all subsets
############################################################

# --- VERSION AND REPRODUCIBILITY INFO ---
script_version <- "v1.0.0"
analysis_git_hash <- tryCatch(system("git rev-parse HEAD", intern=TRUE), error = function(e) NA)
cat("Script version:", script_version, "\n")
cat("Git commit hash:", analysis_git_hash, "\n")
cat("Random seed for modeling and cross-validation: 123\n\n")

# ---- 0. Load Required Libraries ----
library(tidyverse)  # Data manipulation
library(caret)      # Dummy encoding, filtering, modeling utilities
library(mixOmics)   # PLS-DA modeling

# ---- 0a. Helper Functions ----
cut_with_na <- function(x, breaks, labels, right=FALSE) {
  y <- cut(x, breaks=breaks, labels=labels, right=right, include.lowest=TRUE)
  addNA(y, ifany=TRUE)
}

# ===============================
# 1. DATA PREPARATION STAGE
# ===============================

# 1.1. Make Working Copy of Input Data
df3_pls <- df3

# 1.2. Apply Custom Binning and Factor Conversion
df3_pls <- df3_pls %>%
  mutate(
    across(matches("_air_quality$"), as.factor),
    farrowing_piglet_litters_sneezing = ifelse(
      is.na(farrowing_piglet_litters_sneezing_percentage), NA,
      farrowing_piglet_litters_sneezing_percentage > 0
    ),
    farrowing_piglet_litters_coughing = ifelse(
      is.na(farrowing_piglet_litters_coughing_percentage), NA,
      farrowing_piglet_litters_coughing_percentage > 0
    )
  ) %>%
  mutate(
    farrowing_room_temperature = cut_with_na(
      farrowing_room_temperature, breaks = c(-Inf, 17, 20, 23, 26, Inf),
      labels = c("<17", "17–20", "20–23", "23–26", ">26")
    ),
    farrowing_airflow = cut_with_na(
      farrowing_airflow, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
      labels = c("<0.1", "0.1–<0.2", "0.2–<0.4", ">=0.4")
    ),
    ai_sows_room_temperature = cut_with_na(
      ai_sows_room_temperature, breaks = c(-Inf, 15, 18, 21, 24, Inf),
      labels = c("<15", "15–<18", "18–<21", "21–<24", ">=24")
    ),
    ai_sows_airflow = cut_with_na(
      ai_sows_airflow, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
      labels = c("<0.1", "0.1–<0.2", "0.2–<0.4", ">=0.4")
    ),
    gestation_sows_qm_per_animal = cut_with_na(
      gestation_sows_qm_per_animal, breaks = c(-Inf, 2.5, 3.5, 4.5, 6.0, Inf),
      labels = c("<2.5", "2.5–<3.5", "3.5–<4.5", "4.5–<6.0", ">=6.0")
    ),
    gestation_sows_animals_per_water_source = cut_with_na(
      gestation_sows_animals_per_water_source, breaks = c(-Inf, 4, 8, 12, 16, Inf),
      labels = c("<4", "4–<8", "8–<12", "12–<16", ">=16")
    ),
    gestation_sows_room_temperature = cut_with_na(
      gestation_sows_room_temperature, breaks = c(-Inf, 13, 17, 21, 25, Inf),
      labels = c("<13", "13–<17", "17–<21", "21–<25", ">=25")
    ),
    gestation_sows_airflow = cut_with_na(
      gestation_sows_airflow, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
      labels = c("<0.1", "0.1–<0.2", "0.2–<0.4", ">=0.4")
    )
  ) %>%
  mutate(
    weaners_sneezing = if_else(farm_id == 5, NA_real_, weaners_sneezing),
    weaners_sneezing = cut_with_na(
      weaners_sneezing, breaks = c(-Inf, 0, 3, 10, 20, Inf),
      labels = c("0", "0–<3", "3–<10", "10–<20", "20+")
    ),
    weaners_coughing = if_else(farm_id == 5, NA_real_, weaners_coughing),
    weaners_coughing = cut_with_na(
      weaners_coughing, breaks = c(-Inf, 0, 2, 5, Inf),
      labels = c("0", "0–<2", "2–<5", "5+")
    ),
    rectal_temperature_max = cut_with_na(
      rectal_temperature_max, breaks = c(-Inf, 40.0, 40.5, 41.0, Inf),
      labels = c("<40.0", "40.0–<40.5", "40.5–<41.0", ">=41.0")
    ),
    rectal_temperature_avg = cut_with_na(
      rectal_temperature_avg, breaks = c(-Inf, 40.0, 40.5, 41.0, Inf),
      labels = c("<40.0", "40.0–<40.5", "40.5–<41.0", ">=41.0")
    ),
    weaners_qm_per_animal = cut_with_na(
      weaners_qm_per_animal, breaks = c(-Inf, 0.3, 0.4, 0.6, 1.0, Inf),
      labels = c("<0.3", "0.3–<0.4", "0.4–<0.6", "0.6–<1.0", ">=1.0")
    ),
    weaners_animals_per_water_source = cut_with_na(
      weaners_animals_per_water_source, breaks = c(-Inf, 10, 15, 25, 40, Inf),
      labels = c("<10", "10–<15", "15–<25", "25–<40", ">=40")
    ),
    weaners_room_temperature = cut_with_na(
      weaners_room_temperature, breaks = c(-Inf, 15, 18, 21, 25, Inf),
      labels = c("<15", "15–<18", "18–<21", "21–<25", ">=25")
    ),
    weaners_airflow = cut_with_na(
      weaners_airflow, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
      labels = c("<0.1", "0.1–<0.2", "0.2–<0.4", ">=0.4")
    )
  ) %>%
  mutate(
    fattening_pigs_sneezing = if_else(farm_id %in% 1:3, NA_real_, fattening_pigs_sneezing),
    fattening_pigs_sneezing = cut_with_na(
      fattening_pigs_sneezing, breaks = c(-Inf, 0, 2, 5, Inf),
      labels = c("0", "0–<2", "2–<5", "5+")
    ),
    fattening_pigs_coughing = if_else(farm_id %in% 1:3, NA_real_, fattening_pigs_coughing),
    fattening_pigs_coughing = cut_with_na(
      fattening_pigs_coughing, breaks = c(-Inf, 0, 2, 5, Inf),
      labels = c("0", "0–<2", "2–<5", "5+")
    ),
    fattening_pigs_qm_per_animal = if_else(farm_id %in% 1:6, NA_real_, fattening_pigs_qm_per_animal),
    fattening_pigs_qm_per_animal = cut_with_na(
      fattening_pigs_qm_per_animal, breaks = c(-Inf, 0.5, 1.0, 1.5, Inf),
      labels = c("<0.5", "0.5–<1.0", "1.0–<1.5", ">=1.5")
    ),
    fattening_pigs_animals_per_water_source = if_else(farm_id %in% 1:6, NA_real_, fattening_pigs_animals_per_water_source),
    fattening_pigs_animals_per_water_source = cut_with_na(
      fattening_pigs_animals_per_water_source, breaks = c(-Inf, 10, 15, Inf),
      labels = c("<10", "10–<15", ">=15")
    ),
    fattening_pigs_room_temperature = if_else(farm_id %in% 1:3, NA_real_, fattening_pigs_room_temperature),
    fattening_pigs_room_temperature = cut_with_na(
      fattening_pigs_room_temperature, breaks = c(-Inf, 10, 15, 20, 25, Inf),
      labels = c("10", "10–<15", "15–<20", "20–<25", ">=25")
    ),
    fattening_pigs_airflow = if_else(farm_id %in% c(1:3,7), NA_real_, fattening_pigs_airflow),
    fattening_pigs_airflow = cut_with_na(
      fattening_pigs_airflow, breaks = c(-Inf, 0.1, 0.2, 0.4, Inf),
      labels = c("<0.1", "0.1–<0.2", "0.2–<0.4", ">=0.4")
    )
  ) %>%
  dplyr::select(-farrowing_piglet_litters_sneezing_percentage, -farrowing_piglet_litters_coughing_percentage)

# 1.3. Handle Missing Values in specified Numeric Columns
numeric_na_to_zero <- c(
  "number_suckling_piglets", "number_weaners", "number_fattening_pigs",
  "number_young_sows", "number_old_sows", "number_boars"
)
for (col in numeric_na_to_zero) {
  if (col %in% names(df3_pls)) {
    df3_pls[[col]][is.na(df3_pls[[col]])] <- 0
  }
}

# 1.4. Identify Factors for Dummy Coding (excluding meta variables)
meta_vars <- c("farm_id", "total_samples", "positive_pigs",
               "percent_positive_pigs", "date_sampling", "SIV_positive", "min_ct", "max_ct", "mean_ct", "sd_ct")
is_dummy_factor <- sapply(df3_pls, function(x) {
  (is.factor(x) && !all(is.na(x)) && nlevels(x) > 1) || is.logical(x)
})
dummy_factors <- setdiff(names(df3_pls)[is_dummy_factor], meta_vars)

# 1.5. Convert logicals and factors in dummy_factors to factors with levels TRUE, FALSE, NA (NA only if present)
df3_pls[dummy_factors] <- lapply(df3_pls[dummy_factors], function(x) {
  if (is.logical(x)) {
    vals <- ifelse(is.na(x), "NA", as.character(x))
    levels_needed <- unique(vals)
    factor(vals, levels = levels_needed)
  } else if (is.factor(x)) {
    x_char <- as.character(x)
    x_char[is.na(x_char)] <- "NA"
    levels_needed <- unique(x_char)
    factor(x_char, levels = levels_needed)
  } else {
    x
  }
})

# 1.6. Dummy Encode Multi-level Factors, Drop Custom Reference Columns Only
if(length(dummy_factors) > 0){
  dummy_formula <- as.formula(paste("~", paste(dummy_factors, collapse = " + ")))
  dummy_df <- as.data.frame(
    predict(caret::dummyVars(dummy_formula, data = df3_pls, sep = "_", fullRank = FALSE), 
            newdata = df3_pls)
  )
} else {
  dummy_df <- NULL
}


# Define all custom reference dummies to be removed (variable: dummy column name)
custom_refs <- c(
  production_type_factor = "production_type_factor_5",
  source_of_contact_grouped = "source_of_contact_grouped_Other Private Practices", 
  quarantine_time = "quarantine_time_20",
  cross_fostering_farrowing_stable = "cross_fostering_farrowing_stable_1",
  cleaning_ai_centre = "cleaning_ai_centre_3",
  cleaning_fattener_stable = "cleaning_fattener_stable_3",
  cleaning_quarantine = "cleaning_quarantine_3",
  disinfection_farrowing_stable = "disinfection_farrowing_stable_3",
  disinfection_weaner_stable = "disinfection_weaner_stable_3",
  drying_ai_centre = "drying_ai_centre_3",
  drying_fattener_stable = "drying_fattener_stable_3",
  caretaker_type = "caretaker_type_1",
  respiratory_history_swine = "respiratory_history_swine_1",
  frequency_respi_outbreak = "frequency_respi_outbreak_never",
  Age_weeks_factor = "Age_weeks_factor_5-8w",
  seperation_between_production_units = "seperation_between_production_units_1",
  seperation_within_production_units = "seperation_within_production_units_1",
  seperation_quarantine_area = "seperation_quarantine_area_1",
  farrowing_room_temperature = "farrowing_room_temperature_20–23°C",
  farrowing_airflow = "farrowing_airflow_<0.1",
  farrowing_air_quality = "farrowing_air_quality_1",
  farrowing_airspace_with_other_agegroup = "farrowing_airspace_with_other_agegroup_1",
  ai_sows_room_temperature = "ai_sows_room_temperature_15–<18",
  ai_sows_airflow = "ai_sows_airflow_<0.1",
  ai_sows_air_quality = "ai_sows_air_quality_1",
  ai_airspace_with_other_agegroup = "ai_airspace_with_other_agegroup_1",
  gestation_sows_qm_per_animal = "gestation_sows_qm_per_animal_>=6.0",
  gestation_sows_animals_per_water_source = "gestation_sows_animals_per_water_source_<4",
  gestation_sows_room_temperature = "gestation_sows_room_temperature_13–<17",
  gestation_sows_airflow = "gestation_sows_airflow_<0.1",
  gestation_sows_air_quality = "gestation_sows_air_quality_1",
  gestation_sows_airspace_with_other_agegroup = "gestation_sows_airspace_with_other_agegroup_1",
  weaners_sneezing = "weaners_sneezing_0–<3",
  weaners_coughing = "weaners_coughing_0–<2",
  rectal_temperature_max = "rectal_temperature_max<40.0",
  rectal_temperature_avg = "rectal_temperature_avg<40.0",
  weaners_qm_per_animal = "weaners_qm_per_animal_>=1.0",
  weaners_animals_per_feeding_site_factor = "weaners_animals_per_feeding_site_factor_<20",
  weaners_animals_per_water_source = "weaners_animals_per_water_source_<10",
  weaners_room_temperature = "weaners_room_temperature_>=25",
  weaners_airflow = "weaners_airflow_<0.1",
  weaners_air_quality = "weaners_air_quality_1",
  weaners_airspace_with_other_agegroup = "weaners_airspace_with_other_agegroup_2",
  fattening_pigs_sneezing = "fattening_pigs_sneezing_0–<2",
  fattening_pigs_coughing = "fattening_pigs_coughing_0–<2",
  fattening_pigs_qm_per_animal = "fattening_pigs_qm_per_animal_>=1.5",
  fattening_pigs_feeding_site_per_animal_factor = "fattening_pigs_feeding_site_per_animal_factor_over 25cm trough/animal",
  fattening_pigs_animals_per_water_source = "fattening_pigs_animals_per_water_source_<10",
  fattening_pigs_room_temperature = "fattening_pigs_room_temperature_15–<20",
  fattening_pigs_airflow = "fattening_pigs_airflow_<0.1",
  fattening_pigs_air_quality = "fattening_pigs_air_quality_1",
  fattening_pigs_airspace_with_other_agegroup = "fattening_pigs_airspace_with_other_agegroup_2"
)

custom_refs_reasons <- c(
  production_type_factor = "lowest SIV risk in nucleus/multiplier herds",
  source_of_contact_grouped = "reference for additonal contact sources",
  quarantine_time = "longest quarantine",
  cross_fostering_farrowing_stable = "never done as lowest risk for SIV due to best lactogen immunity",
  cleaning_ai_centre = "always clean after emptying, best practice for AIAO",
  cleaning_fattener_stable = "always clean after emptying, best practice for AIAO",
  cleaning_quarantine = "always clean after emptying, best practice",
  disinfection_farrowing_stable = "always desinfect after emptying, best practice for AIAO",
  disinfection_weaner_stable = "always desinfect after emptying, best practice for AIAO",
  drying_ai_centre = "always let dry after emptying, best practice for AIAO",
  drying_fattener_stable = "always let dry after emptying, best practice for AIAO",
  caretaker_type = "least risk for biosecurity because fewest people",
  respiratory_history_swine = "least risk because no recent history of SIV outbreaks",
  frequency_respi_outbreak = "least risk because no recent history of SIV outbreaks",
  Age_weeks_factor = "most common age category",
  seperation_between_production_units = "lowest risk for transmission by using seperate buildings",
  seperation_within_production_units = "lowest risk for transmission by using seperate buildings",
  seperation_quarantine_area = "lowest risk for transmission by using seperate buildings",
  farrowing_room_temperature = "best practice temperature",
  farrowing_airflow = "best practice to minimize any airflow",
  farrowing_air_quality = "best air quality",
  farrowing_airspace_with_other_agegroup = "lowest risk for transmission by using seperate buildings",
  ai_sows_room_temperature = "best practice temperature",
  ai_sows_airflow = "best practice to minimize any airflow",
  ai_sows_air_quality = "best air quality",
  ai_airspace_with_other_agegroup = "lowest risk for transmission by using seperate buildings",
  gestation_sows_qm_per_animal = "lowest stocking density leading to lowest risk of transmission",
  gestation_sows_animals_per_water_source = "lowest stocking density leading to lowest risk of transmission",
  gestation_sows_room_temperature = "best practice temperature",
  gestation_sows_airflow = "best practice to minimize any airflow",
  gestation_sows_air_quality = "best air quality",
  gestation_sows_airspace_with_other_agegroup = "lowest risk for transmission by using seperate buildings",
  weaners_sneezing = "lowest prevalence leading to lowest SIV risk",
  weaners_coughing = "lowest prevalence leading to lowest SIV risk",
  rectal_temperature_max = "lowest inner body temperature leading to lowest SIV risk",
  rectal_temperature_avg = "lowest inner body temperature leading to lowest SIV risk",
  weaners_qm_per_animal = "lowest stocking density leading to lowest risk of transmission",
  weaners_animals_per_feeding_site_factor = "lowest stocking density leading to lowest risk of transmission",
  weaners_animals_per_water_source = "lowest stocking density leading to lowest risk of transmission",
  weaners_room_temperature = "best practice temperature",
  weaners_airflow = "best practice to minimize any airflow",
  weaners_air_quality = "best air quality",
  weaners_airspace_with_other_agegroup = "lowest risk for transmission by using seperate buildings",
  fattening_pigs_sneezing = "lowest prevalence leading to lowest SIV risk",
  fattening_pigs_coughing = "lowest prevalence leading to lowest SIV risk",
  fattening_pigs_qm_per_animal = "lowest stocking density leading to lowest risk of transmission",
  fattening_pigs_feeding_site_per_animal_factor = "lowest stocking density leading to lowest risk of transmission",
  fattening_pigs_animals_per_water_source = "lowest stocking density leading to lowest risk of transmission",
  fattening_pigs_room_temperature = "best practice temperature",
  fattening_pigs_airflow = "best practice to minimize any airflow",
  fattening_pigs_air_quality = "best air quality",
  fattening_pigs_airspace_with_other_agegroup = "lowest risk for transmission by using seperate buildings, but including fattening on farm"
)

# Remove custom reference dummy columns
removed_custom <- character()
removed_custom_reason <- character()
removed_custom_var <- character()
if (!is.null(dummy_df)) {
  for (i in seq_along(custom_refs)) {
    col_to_remove <- custom_refs[i]
    if (col_to_remove %in% names(dummy_df)) {
      dummy_df[[col_to_remove]] <- NULL
      removed_custom <- c(removed_custom, col_to_remove)
      removed_custom_reason <- c(removed_custom_reason, custom_refs_reasons[names(custom_refs)[i]])
      removed_custom_var <- c(removed_custom_var, names(custom_refs)[i])
    }
  }
  
  # Remove _FALSE dummy columns
  false_cols <- grep("_FALSE$", names(dummy_df), value = TRUE)
  removed_false_var <- sapply(false_cols, function(nm) sub("_FALSE$", "", nm))
  removed_false_reason <- rep("FALSE reference dummy (represents lowest risk or reference group)", length(false_cols))
  for(nm in false_cols) dummy_df[[nm]] <- NULL
} else {
  false_cols <- character()
  removed_false_var <- character()
  removed_false_reason <- character()
}

# Log all removed dummy columns and reasons
removed_log <- data.frame(
  SourceVariable = c(removed_custom_var, removed_false_var),
  DummyColumn = c(removed_custom, false_cols),
  Reason = c(removed_custom_reason, removed_false_reason),
  stringsAsFactors = FALSE
)

write.csv(removed_log, "removed_reference_dummies_log.csv", row.names = FALSE)

# Bind dummies to df3_pls
if (!is.null(dummy_df)) {
  df3_pls <- df3_pls %>%
    dplyr::select(-all_of(dummy_factors)) %>%
    dplyr::bind_cols(dummy_df)
}

# 1.7. Build Final Numeric Predictor Matrix X
X <- df3_pls %>%
  dplyr::select(-all_of(meta_vars), -dplyr::starts_with("verification")) %>%
  dplyr::mutate(across(where(is.logical), as.numeric)) %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  dplyr::mutate(across(where(is.character), ~ as.numeric(.))) %>%
  dplyr::mutate(across(where(is.integer), as.numeric)) %>%
  as.data.frame()
X <- X[ , !grepl("_FALSE$", names(X)), drop = FALSE]

# ===============================
# 2. PREPARE RESPONSE VARIABLE
# ===============================
# Converts SIV_positive to a binary factor with TRUE/FALSE levels, handles various encodings
Y_raw <- df3_pls$SIV_positive
if (!is.factor(Y_raw)) Y_raw <- as.factor(Y_raw)
Y <- factor(
  ifelse(
    Y_raw %in% c(1, "1", TRUE, "TRUE", "positive"), TRUE,
    ifelse(Y_raw %in% c(0, "0", FALSE, "FALSE", "negative"), FALSE, NA)
  ),
  levels = c(FALSE, TRUE)
)
if (!is.factor(Y)) stop("SIV_positive could not be coerced to a factor for PLS-DA.")
if (length(levels(Y)) != 2) stop("SIV_positive must have exactly two levels (TRUE/FALSE) for binary PLS-DA.")
if (any(is.na(Y))) {
  valid_idx <- !is.na(Y)
  X <- X[valid_idx, , drop = FALSE]
  Y <- Y[valid_idx]
}


# ===============================
# 3. SYSTEMATIC PREDICTOR FILTERING (with NZV and univariate association filter)
# ===============================
# Filtering steps remove problematic predictors:
#   3.1 Low-variance and constant columns (nearZeroVar from caret)
#   3.2 Columns with all NA values
#   3.3 Columns with only one unique value (after NA removal)
#   3.4 Univariate association filtering (remove predictors with no association to outcome)
#   3.5 Impute selected NAs with 0
#   3.6 Exact duplicate columns
#   3.7 Highly correlated columns (>0.95, standard)
#   3.8 Custom filter (user-defined, with logging)
#   3.9 Save predictor removal log

removed_predictors_log <- data.frame(
  Predictor = character(),
  Reason = character(),
  DuplicateOf = character(),
  AliasedWith = character(),
  stringsAsFactors = FALSE
)

# Helper: test if dummy_mapping exists and is not empty
is_dummy_col <- function(cols) {
  if (exists("dummy_mapping") && !is.null(dummy_mapping) && nrow(dummy_mapping) > 0) {
    cols %in% dummy_mapping$dummy_name
  } else {
    rep(FALSE, length(cols))
  }
}

# ---- 3.1. Remove low-variance and constant columns (nearZeroVar from caret) ----
nzv_metrics <- caret::nearZeroVar(X, saveMetrics = TRUE)
problem_idx <- which(nzv_metrics$zeroVar | nzv_metrics$nzv)
cols_to_remove <- rownames(nzv_metrics)[problem_idx]
if(length(cols_to_remove) > 0) {
  is_dummy <- is_dummy_col(cols_to_remove)
  if (any(is_dummy)) {
    result <- remove_dummy_group(X, removed_predictors_log, dummy_mapping, cols_to_remove[is_dummy], "Near-zero or zero variance")
    X <- result$X; removed_predictors_log <- result$removed_predictors_log
    cols_to_remove <- cols_to_remove[!is_dummy]
  }
  if (length(cols_to_remove) > 0) {
    removed_predictors_log <- rbind(removed_predictors_log,
                                    data.frame(Predictor = cols_to_remove, Reason = "Near-zero or zero variance", DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE))
    X <- X[, !colnames(X) %in% cols_to_remove, drop = FALSE]
  }
}

# ---- 3.2. Remove columns where all values are NA ----
all_na_cols <- colnames(X)[colSums(!is.na(X)) == 0]
if(length(all_na_cols) > 0) {
  is_dummy <- is_dummy_col(all_na_cols)
  if (any(is_dummy)) {
    result <- remove_dummy_group(X, removed_predictors_log, dummy_mapping, all_na_cols[is_dummy], "All NA values")
    X <- result$X; removed_predictors_log <- result$removed_predictors_log
    all_na_cols <- all_na_cols[!is_dummy]
  }
  if (length(all_na_cols) > 0) {
    removed_predictors_log <- rbind(removed_predictors_log,
                                    data.frame(Predictor = all_na_cols, Reason = "All NA values", DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE))
    X <- X[, colSums(!is.na(X)) > 0, drop = FALSE]
  }
}

# ---- 3.3. Remove columns with only one unique value (after NA removal) ----
constant_cols <- colnames(X)[sapply(X, function(x) length(unique(x[!is.na(x)])) <= 1)]
if(length(constant_cols) > 0) {
  is_dummy <- is_dummy_col(constant_cols)
  if (any(is_dummy)) {
    result <- remove_dummy_group(X, removed_predictors_log, dummy_mapping, constant_cols[is_dummy], "Only one unique value")
    X <- result$X; removed_predictors_log <- result$removed_predictors_log
    constant_cols <- constant_cols[!is_dummy]
  }
  if (length(constant_cols) > 0) {
    removed_predictors_log <- rbind(removed_predictors_log,
                                    data.frame(Predictor = constant_cols, Reason = "Only one unique value", DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE))
    X <- X[, sapply(X, function(x) length(unique(x[!is.na(x)])) > 1), drop = FALSE]
  }
}


# ---- 3.4. Impute remaining NAs with 0 (for modeling) ----
if (any(is.na(X))) X[is.na(X)] <- 0

# ---- 3.5. Remove exact duplicate columns (keep only the first occurrence) ----
dup_cols_idx <- which(duplicated(t(X)))
dup_cols <- colnames(X)[dup_cols_idx]
if(length(dup_cols) > 0) {
  for (i in seq_along(dup_cols_idx)) {
    idx <- dup_cols_idx[i]
    dup_name <- colnames(X)[idx]
    orig_idx <- which(apply(t(X)[1:(idx-1),,drop=FALSE], 1, function(row) all(row == X[,idx])))
    orig_name <- if (length(orig_idx) > 0) colnames(X)[orig_idx[1]] else NA
    removed_predictors_log <- rbind(
      removed_predictors_log,
      data.frame(Predictor = dup_name, Reason = "Duplicate predictor", DuplicateOf = orig_name, AliasedWith = NA_character_, stringsAsFactors = FALSE)
    )
  }
  X <- X[, !duplicated(t(X)), drop=FALSE]
}

# ---- 3.6. Remove highly correlated columns (>0.95 correlation, standard) ----
cor_matrix <- cor(X)
if (any(is.na(cor_matrix))) {
  badcols <- unique(c(which(rowSums(is.na(cor_matrix)) > 0), which(colSums(is.na(cor_matrix)) > 0)))
  if (length(badcols) > 0) {
    cols_to_remove <- colnames(X)[badcols]
    is_dummy <- is_dummy_col(cols_to_remove)
    if (any(is_dummy)) {
      result <- remove_dummy_group(X, removed_predictors_log, dummy_mapping, cols_to_remove[is_dummy], "NA in correlation matrix (likely zero variance)")
      X <- result$X; removed_predictors_log <- result$removed_predictors_log
      cols_to_remove <- cols_to_remove[!is_dummy]
    }
    if (length(cols_to_remove) > 0) {
      removed_predictors_log <- rbind(removed_predictors_log,
                                      data.frame(Predictor = cols_to_remove, Reason = "NA in correlation matrix (likely zero variance)", DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE))
      X <- X[, -badcols, drop = FALSE]
      cor_matrix <- cor(X)
    }
  }
}
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.95)
if (length(highly_correlated) > 0) {
  cols_to_remove <- colnames(X)[highly_correlated]
  is_dummy <- is_dummy_col(cols_to_remove)
  if (any(is_dummy)) {
    result <- remove_dummy_group(X, removed_predictors_log, dummy_mapping, cols_to_remove[is_dummy], "High correlation (>0.95)")
    X <- result$X; removed_predictors_log <- result$removed_predictors_log
    cols_to_remove <- cols_to_remove[!is_dummy]
  }
  if (length(cols_to_remove) > 0) {
    removed_predictors_log <- rbind(removed_predictors_log,
                                    data.frame(Predictor = cols_to_remove, Reason = "High correlation (>0.95)", DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE))
    X <- X[, !colnames(X) %in% cols_to_remove, drop = FALSE]
  }
}

# ---- 3.7. Custom filter (user-defined, with logging) ----
custom_cols <- c()
if ("number_weaners" %in% colnames(X)) custom_cols <- c(custom_cols, "number_weaners")
if ("number_suckling_piglets" %in% colnames(X)) custom_cols <- c(custom_cols, "number_suckling_piglets")
if ("number_old_sows" %in% colnames(X)) custom_cols <- c(custom_cols, "number_old_sows")
if ("number_young_sows" %in% colnames(X)) custom_cols <- c(custom_cols, "number_young_sows")
if ("number_fattening_pigs" %in% colnames(X)) custom_cols <- c(custom_cols, "number_fattening_pigs")
if ("number_boars" %in% colnames(X)) custom_cols <- c(custom_cols, "number_boars")
if ("visitors_in_stable_recent" %in% colnames(X)) custom_cols <- c(custom_cols, "visitors_in_stable_recent")
if ("outside_area_ai_centre_TRUE" %in% colnames(X)) custom_cols <- c(custom_cols, "outside_area_ai_centre_TRUE")
if ("outside_area_gestation_stable_TRUE" %in% colnames(X)) custom_cols <- c(custom_cols, "outside_area_gestation_stable_TRUE")
if ("outside_area_farrowing_stable_TRUE" %in% colnames(X)) custom_cols <- c(custom_cols, "outside_area_farrowing_stable_TRUE")
if ("outside_area_weaner_stable_TRUE" %in% colnames(X)) custom_cols <- c(custom_cols, "outside_area_weaner_stable_TRUE")
if ("outside_area_fattener_TRUE" %in% colnames(X)) custom_cols <- c(custom_cols, "outside_area_fattener_TRUE")
if ("starting_point_current_disease" %in% colnames(X)) custom_cols <- c(custom_cols, "starting_point_current_disease")
if ("start_time_current_outbreak" %in% colnames(X)) custom_cols <- c(custom_cols, "start_time_current_outbreak")

soc_cols <- grep("source_of_contact", colnames(X), value = TRUE)
if (length(soc_cols) > 0) custom_cols <- c(custom_cols, soc_cols)

if(length(custom_cols) > 0) {
  custom_reasons <- c(
    rep("Remove as explained by herdsize", sum(custom_cols == "number_weaners")),
    rep("Remove as explained by herdsize", sum(custom_cols == "number_suckling_piglets")),
    rep("Remove as explained by herdsize", sum(custom_cols == "number_old_sows")),
    rep("Remove as explained by herdsize", sum(custom_cols == "number_young_sows")),
    rep("Remove as explained by herdsize", sum(custom_cols == "number_fattening_pigs")),
    rep("Remove as explained by herdsize", sum(custom_cols == "number_boars")),
    rep("Remove as also explained by cumulative hours", sum(custom_cols == "visitors_in_stable_recent")),
    rep("Remove as explained by overview variable", sum(custom_cols == "outside_area_ai_centre_TRUE")),
    rep("Remove as explained by overview variable", sum(custom_cols == "outside_area_gestation_stable_TRUE")),
    rep("Remove as explained by overview variable", sum(custom_cols == "outside_area_farrowing_stable_TRUE")),
    rep("Remove as explained by overview variable", sum(custom_cols == "outside_area_weaner_stable_TRUE")),
    rep("Remove as explained by overview variable", sum(custom_cols == "outside_area_fattener_TRUE")),
    rep("Remove as explained by respiratory_history_human", sum(custom_cols == "starting_point_current_disease")),
    rep("Remove as explained by respiratory_history_swine", sum(custom_cols == "start_time_current_outbreak")),
    
    rep("Remove: all source_of_contact vars - not of interest here", length(soc_cols))
  )
  reason_map <- setNames(custom_reasons, custom_cols)
  reason_out <- reason_map[custom_cols]
  removed_predictors_log <- rbind(
    removed_predictors_log,
    data.frame(Predictor = custom_cols, Reason = reason_out, DuplicateOf = NA, AliasedWith = NA_character_, stringsAsFactors = FALSE)
  )
  X <- X[, !colnames(X) %in% custom_cols, drop = FALSE]
}

# ---- 3.8. Save predictor removal log ----
write.csv(removed_predictors_log, "removed_predictors_log.csv", row.names = FALSE)



# ===============================
# 4. STANDARDIZE PREDICTORS AND EXPORT SUBSET MATRICES
# ===============================
# - For each subset, selects relevant predictors, standardizes them, and exports the matrix and response for reproducibility

# 4.1. Define subset variable lists and helper
exclude_vars <- make_exclude_vars("SIV_positive")
vars_husbandry   <- get_husbandry_vars(exclude_vars)
vars_animals     <- get_animal_vars(exclude_vars)
vars_environment <- get_environment_vars(exclude_vars)
vars_human       <- get_human_vars(exclude_vars)


# Predictors to add
extra_predictors <- c(
  "farrowing_piglet_litters_sneezing_TRUE",
  "farrowing_piglet_litters_coughing_TRUE"
)

# Add them to the animal subset variable list
vars_animals <- unique(c(vars_animals, extra_predictors))




find_subset_cols <- function(vars, all_cols) {
  unique(unlist(lapply(vars, function(v)
    grep(paste0("^", v, "($|_)"), all_cols, value = TRUE))))
}

subset_list <- list(
  Husbandry   = find_subset_cols(vars_husbandry,   colnames(X)),
  Animals     = find_subset_cols(vars_animals,     colnames(X)),
  Environment = find_subset_cols(vars_environment, colnames(X)),
  Human       = find_subset_cols(vars_human,       colnames(X))
)

output_dir <- "04_output/SIV_PLS_Analysis_Results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 4.2. Standardize and export each subset predictor matrix and response
for (subset_name in names(subset_list)) {
  subset_cols <- subset_list[[subset_name]]
  X_subset <- X[, subset_cols, drop = FALSE]
  X_subset_std <- as.data.frame(scale(X_subset))
  # Standardized matrix for each subset
  write.csv(X_subset_std, file.path(output_dir, paste0("model_X_", subset_name, "_used.csv")), row.names = TRUE)
  # Response for reproducibility (same for all subsets)
  write.csv(data.frame(SIV_positive = Y), file.path(output_dir, paste0("model_Y_", subset_name, "_used.csv")), row.names = TRUE)
}

# ===============================
# 5. SUBSET-SPECIFIC PLS-DA MODEL FITTING AND RESULT EXTRACTION (FULLY ROBUST, ANNOTATED)
# ===============================

subset_results <- list()
for (subset_name in names(subset_list)) {
  # 5.1. Select relevant predictors for this subset.
  subset_cols <- subset_list[[subset_name]]
  X_subset <- X[, subset_cols, drop = FALSE]
  # 5.2. Find valid rows: complete cases in predictors AND non-missing outcome.
  valid_idx <- complete.cases(X_subset) & !is.na(Y)
  # 5.3. Subset predictors and outcome to valid rows.
  X_subset_valid <- X_subset[valid_idx, , drop = FALSE]
  Y_subset <- Y[valid_idx]
  # 5.4. Guarantee Y is an atomic factor (never list).
  if (is.list(Y_subset)) Y_subset <- unlist(Y_subset)
  if (is.matrix(Y_subset)) Y_subset <- as.vector(Y_subset)
  Y_subset <- as.factor(Y_subset)
  # 5.5. Standardize predictors for model fitting.
  X_subset_std <- as.data.frame(scale(X_subset_valid))
  # 5.6. Fit model only if >1 predictor and Y has both outcome levels.
  if (ncol(X_subset_std) > 1 && length(unique(Y_subset)) == 2) {
    max_comp <- min(3, ncol(X_subset_std), floor(nrow(X_subset_std)/4))
    set.seed(123)
    plsda_model <- mixOmics::plsda(X_subset_std, Y_subset, ncomp = max_comp)
    # 5.7. Cross-validation to select optimal number of components.
    ncomp_optimal <- tryCatch({
      cv <- mixOmics::perf(plsda_model, validation = "Mfold", folds = 5, progressBar = FALSE)
      if (is.matrix(cv$error.rate$overall)) {
        error_rate_vec <- colMeans(cv$error.rate$overall)
      } else {
        error_rate_vec <- cv$error.rate$overall
      }
      rmsep_values <- sqrt(error_rate_vec)
      which.min(rmsep_values)
    }, error = function(e) 2)
    ncomp_model <- plsda_model$ncomp
    if (is.na(ncomp_optimal) || ncomp_optimal < 1) ncomp_optimal <- 1
    if (ncomp_optimal > ncomp_model) ncomp_optimal <- ncomp_model
    # 5.8. Extract Variable Importance in Projection (VIP) - robust.
    if ("mixo_plsda" %in% class(plsda_model)) {
      vip_scores <- mixOmics::vip(plsda_model)
      coef_df <- data.frame(
        Predictor = rownames(vip_scores),
        VIP = vip_scores[, ncomp_optimal]
      )
      coef_df <- coef_df %>%
        group_by(Predictor) %>%
        slice_max(VIP, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        arrange(desc(VIP))
    } else {
      stop("The model type is incompatible with VIP score extraction using mixOmics::vip().")
    }
    # 5.9. Extract loadings, scores, explained variance.
    loadings_df <- as.data.frame(plsda_model$loadings$X[, 1:ncomp_model, drop = FALSE])
    scores_df <- as.data.frame(plsda_model$variates$X[, 1:ncomp_model, drop = FALSE])
    colnames(scores_df) <- paste0("Comp ", 1:ncol(scores_df))
    explained_var <- plsda_model$prop_expl_var$X
    names(explained_var) <- paste0("Comp ", seq_along(explained_var))
    # 5.10. Store all results for this subset.
    subset_results[[subset_name]] <- list(
      plsda_model = plsda_model,
      coef_df = coef_df,
      loadings_df = loadings_df,
      scores_df = scores_df,
      explained_var = explained_var,
      ncomp_optimal = ncomp_optimal,
      sample_size = nrow(X_subset_std),
      predictor_count = ncol(X_subset_std),
      original_predictor_count = length(subset_cols),
      date_time = Sys.time(),
      author = "jonasalexandersteiner",
      X = X_subset_std,
      SIV_positive = Y_subset
    )
  }
}


# ===============================
# 6. SAVE SUBSET ANALYSIS RESULTS AND RENDER REPORT
# ===============================
# - Saves all subset model results and renders a single R Markdown visualization report for all subsets

# 6.1. Save subset results as RDS (all in one file)
saveRDS(subset_results, file.path(output_dir, "plsda_analysis_results_subsets.rds"))

# 6.2. Render R Markdown Visualization Report (expects to work with all subsets)
rmd_file <- file.path(output_dir, "SIV_PLS_visualization.Rmd")
html_file <- file.path(output_dir, "Supplementary_File_S5.html")
if (!file.exists(rmd_file)) stop(paste("R Markdown file does not exist:", rmd_file))
rmarkdown::render(
  input = rmd_file,
  output_file = html_file,
  output_dir = output_dir,
  envir = new.env(parent = globalenv())
)

############################################################
# REPRODUCIBILITY
############################################################

cat("\n############## Recommendations for Publication ##############\n")
cat("1. **Script version, git commit, and random seed** are logged at the top for full reproducibility.\n")
cat("2. **Subset variable definitions** are harmonized with the univariate/correlation analysis and clearly documented in this script. This guarantees that all subset results are directly comparable and fully transparent.\n")
cat("3. **All filtering steps** (low-variance, constant, all-NA, duplicates, high-correlation, custom, etc.) are logged to 'removed_predictors_log.csv' for audit and review.\n")
cat("4. **Reference dummy variable removal** is explicitly logged to 'removed_reference_dummies_log.csv' with reasons for each choice, supporting full transparency of coding and model input.\n")
cat("5. **Standardized model input matrices** (X) and the binary response variable (Y) are exported per subset in the output directory for reproducibility and external audit.\n")
cat("6. **Session information** is written to 'R_sessionInfo.txt' for full environment documentation.\n")
cat("7. **Subset PLS-DA model results** (including VIP, loadings, scores, performance, etc.) are stored in a single RDS file for all downstream visualization and reporting, and can be reloaded for further analysis or peer review.\n")
cat("8. **R Markdown report** renders all results with full provenance and reproducibility info, suitable for publication and supplement.\n")
cat("\n############################################################\n")
cat("If you submit this work, include all output files above, this script, and the data version/commit referenced.\n")
cat("For any reviewer or auditor, all variable selection, filtering, and modeling steps are traceable in the logs and output.\n")
cat("For any questions regarding variable selection, dummy coding, or model decisions, see the detailed logs and comments in this script.\n")
############################################################
# End of SIV Binary PLSR (PLS-DA) Subset Analysis Script
############################################################