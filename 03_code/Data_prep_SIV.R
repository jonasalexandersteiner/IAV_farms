# ===========================
# SIV Farms Data Cleaning Pipeline
# ===========================
# This script performs robust, transparent cleaning and transformation of farm-level data
# for SIV risk analysis, following best practices in reproducible data science.
#
# STRUCTURE:
# 1. Environment Setup
# 2. Library Management
# 3. Directory Preparation
# 4. Data Loading and Initial Cleaning
# 5. Helper Functions (with detailed logic & error handling)
# 6. Data Transformation Pipeline (with explicit type conversions and relocation)
# 7. Quick Data Overview
# ===========================

# ---- 1. Environment Setup ----
Sys.setenv(LANG = "en")     # Ensure English messages
options(scipen = 999)       # Avoid scientific notation for numerics
rm(list = ls())             # Remove all objects from workspace for a clean start

# ---- 2. Library Management ----
library(pacman)
library(dplyr)
# Load all required packages (will install if missing)
pacman::p_load(
  tidyverse, haven, readxl, writexl, janitor, lubridate, DT,
  broom, purrr, stringr, knitr, kableExtra ,deeplr
)

# ---- 3. Directory Preparation ----
d_proj <- "C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/SIV_Projekt/Data_processing/SIV_farms"
setwd(d_proj)
dirs <- file.path(d_proj, c("01_oridata", "02_data", "03_code", "04_output"))
# Create directories if they do not exist (recursive for nested folders)
walk(dirs, ~ if(!dir.exists(.x)) dir.create(.x, recursive = TRUE))
d_oridata <- dirs[1]

# ---- 4. Data Loading and Initial Cleaning ----
# Read main Excel file, treat "NA" as missing, clean column names to snake_case
sivfarms <- read_excel(file.path(d_oridata, "SIV_farms.xlsx"), na = "NA") %>%
  clean_names()
df1 <- sivfarms

# ---- 5. Helper Functions ----

# 5a. Feeding Site Category Extraction
#    - Extract, categorize feeding site information using pattern matching and numeric binning
make_category <- function(x) {
  if (is.na(x)) return("NA")
  # Pattern: "Tiere/Automat" with leading number
  if (grepl("Tiere/Automat", x, ignore.case = TRUE)) {
    num <- as.numeric(sub("^([0-9]+\\.?[0-9]*)\\s*Tiere/Automat.*$", "\\1", x))
    if (!is.na(num)) {
      if (num < 25) return("under 25 Animals/feeder")
      return("over 25 Animals/feeder")
    }
  }
  # Pattern: just a number (trough size)
  if (grepl("^\\s*-?\\d*\\.?\\d+\\s*$", x)) {
    num <- as.numeric(x)
    if (!is.na(num)) {
      if (num < 25) return("under 25cm trough/animal")
      return("over 25cm trough/animal")
    }
  }
  return("Other")
}
category_levels <- c(
  "under 25 Animals/feeder", "over 25 Animals/feeder",
  "under 25cm trough/animal", "over 25cm trough/animal",
  "Other", "NA"
)

# 5b. Age (weeks) Factor Extraction
#    - Convert age group text to categorical bins ("0-4w", "5-8w", ...)
age_weeks_factor <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  x <- trimws(x)
  nums <- as.numeric(str_extract_all(x, "\\d+")[[1]])
  if (length(nums) == 0) return(NA_character_)
  get_bin <- function(n) {
    cut(n, breaks = c(-Inf, 4, 8, 12, Inf),
        labels = c("0-4w", "5-8w", "9-12w", "13+w"),
        right = TRUE)
  }
  bins <- unique(as.character(get_bin(nums)))
  if (length(bins) == 1) {
    return(bins)
  } else {
    if (max(nums, na.rm = TRUE) <= 10) return("mixed_ages<10w")
    return("mixed_ages>10w")
  }
}

# 5c. Logical Conversion
#    - Map German, English, numeric and boolean values to TRUE/FALSE/NA
make_logical <- function(x) {
  case_when(
    is.na(x) ~ NA,
    x %in% c("WAHR", "Ja", "ja", "1", 1, TRUE, T, "TRUE", "true") ~ TRUE,
    x %in% c("FALSCH", "Nein", "nein", "0", 0, FALSE, F, "FALSE", "false") ~ FALSE,
    TRUE ~ NA
  )
}

# 5d. Safe pattern matching function (error-proof)
#    - Safely match patterns without regex errors
safe_match <- function(pattern, x) {
  if (is.na(x) || !is.character(x)) return(FALSE)
  tryCatch(grepl(pattern, x, fixed = TRUE), error = function(e) FALSE)
}

# ---- 6. Pre-processing for problematic frequency_respi_outbreak field ----
# Create safe temporary data frame for categorization
df_temp <- data.frame(
  original = df1$frequency_respiratory_disease_outbreaks,
  category = rep("once", nrow(df1))  # Default value
)

# Process each row individually with safe pattern matching
for (i in 1:nrow(df_temp)) {
  val <- df_temp$original[i]
  
  # NA or empty handling
  if (is.na(val) || trimws(as.character(val)) == "") {
    df_temp$category[i] <- "never"
    next
  }
  
  # Convert to character and lowercase
  val_char <- tolower(as.character(val))
  
  # Simple matching with fixed=TRUE to avoid regex issues
  if (safe_match("few pigs", val_char) || 
      safe_match("kein ausbruch", val_char) || 
      safe_match("keine klinik", val_char)) {
    df_temp$category[i] <- "few pigs affected no outbreak"
  } else if (safe_match("vereinzelt", val_char)) {
    df_temp$category[i] <- "few pigs affected no outbreak"
  } else if (safe_match("wiederkehrend", val_char) || 
             safe_match("regelmässig", val_char) || 
             safe_match("regelmaessig", val_char)) {
    df_temp$category[i] <- "3x or more per year"
  } else if (safe_match("1x/mal pro jahr", val_char) || 
             safe_match("1 mal pro jahr", val_char) || 
             safe_match("einmal pro jahr", val_char) || 
             safe_match("1mal pro jahr", val_char)) {
    df_temp$category[i] <- "1x/year"
  } else if (safe_match("2x/mal pro jahr", val_char) || 
             safe_match("2 mal pro jahr", val_char)) {
    df_temp$category[i] <- "2x/year"
  } else if (safe_match("3x/mal pro jahr", val_char) || 
             safe_match("3 mal pro jahr", val_char)) {
    df_temp$category[i] <- "3x or more per year"
  } else if (safe_match("einmal", val_char)) {
    df_temp$category[i] <- "once"
  } else if (val_char == "1x" || val_char == "1") {
    df_temp$category[i] <- "once"
  } else if (val_char == "2x" || val_char == "2") {
    df_temp$category[i] <- "2x/year"
  } else if (val_char == "3x" || val_char == "3") {
    df_temp$category[i] <- "3x or more per year"
  } else if (safe_match("pro winter", val_char) || 
             safe_match("pro jahr", val_char)) {
    # Extract number if possible
    num_match <- regexpr("\\d+", val_char)
    if (num_match > 0) {
      num_str <- substr(val_char, num_match, num_match + attr(num_match, "match.length") - 1)
      num <- as.numeric(num_str)
      if (!is.na(num)) {
        if (num == 1) df_temp$category[i] <- "1x/year"
        else if (num == 2) df_temp$category[i] <- "2x/year"
        else if (num >= 3) df_temp$category[i] <- "3x or more per year"
      }
    }
  }
}

# ---- 7. Data Transformation Pipeline ----

# --- 7.1 Import and join rectal temperature summary, insert .before = weaners_reduced_general_wellbeing ---
sample_pigs_path <- "C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/SIV_Projekt/Data_processing/SIV_farms/01_oridata/Samples_pigs.xlsx"
df_sample_pigs <- read_excel(sample_pigs_path)

df_temp_summary <- df_sample_pigs %>%
  mutate(Rectal_temperature = as.numeric(Rectal_temperature)) %>%
  dplyr::group_by(Farm_ID) %>%   # <--- ADD THIS
  dplyr::summarize(
    rectal_temperature_max = if (all(is.na(Rectal_temperature))) NA_real_ else round(max(Rectal_temperature, na.rm = TRUE), 1),
    rectal_temperature_avg = if (all(is.na(Rectal_temperature))) NA_real_ else round(mean(Rectal_temperature, na.rm = TRUE), 1)
  ) %>%
  dplyr::ungroup()

df2 <- df1 %>%
  left_join(df_temp_summary, by = c("farm_id" = "Farm_ID")) %>%
  relocate(rectal_temperature_max, rectal_temperature_avg, .before = weaners_reduced_general_wellbeing) %>%
  
  
  # 7.2 Factor conversions (explicit for categorical variables)
  mutate(
    canton = as.factor(canton),
    production_type = as.factor(production_type),
    sgd_qgs = as.factor(sgd_qgs),
    mode_stable_occupation_ai_centre = as.factor(mode_stable_occupation_ai_centre),
    mode_stable_occupation_gilts_stable = as.factor(mode_stable_occupation_gilts_stable),
    mode_stable_occupation_farrowing_stable = as.factor(mode_stable_occupation_farrowing_stable),
    litter_equalization_farrowing_stable = as.factor(litter_equalization_farrowing_stable),
    mode_stable_occupation_weaner_stable = as.factor(mode_stable_occupation_weaner_stable),
    mode_stable_occupation_fattener_stable = as.factor(mode_stable_occupation_fattener_stable),
    isolation_respiratory_dieseased_pigs = as.factor(isolation_respiratory_dieseased_pigs),
    cleaning_ai_centre = as.factor(cleaning_ai_centre),
    cleaning_gilts_stable = as.factor(cleaning_gilts_stable),
    cleaning_farrowing_stable = as.factor(cleaning_farrowing_stable),
    cleaning_weaner_stable = as.factor(cleaning_weaner_stable),
    cleaning_fattener_stable = as.factor(cleaning_fattener_stable),
    cleaning_quarantaine = as.factor(cleaning_quarantaine),
    desinfection_ai_centre = as.factor(desinfection_ai_centre),
    desinfection_gilts_stable = as.factor(desinfection_gilts_stable),
    desinfection_farrowing_stable = as.factor(desinfection_farrowing_stable),
    desinfection_weaner_stable = as.factor(desinfection_weaner_stable),
    desinfection_fattener_stable = as.factor(desinfection_fattener_stable),
    desinfection_quarantaine = as.factor(desinfection_quarantaine),
    drying_ai_centre = as.factor(drying_ai_centre),
    drying_gilts_stable = as.factor(drying_gilts_stable),
    drying_farrowing_stable = as.factor(drying_farrowing_stable),
    drying_weaner_stable = as.factor(drying_weaner_stable),
    drying_fattener_stable = as.factor(drying_fattener_stable),
    drying_quarantaine = as.factor(drying_quarantaine),
    cleaning_desinfection_transport_vehicle = as.factor(cleaning_desinfection_transport_vehicle),
    cleaning_shipment_area = as.factor(cleaning_shipment_area),
    caretaker_type = as.factor(caretaker_type),
    caretaker_ppe_stable = as.factor(caretaker_ppe_stable),
    caretaker_ppe_washing_interval = as.factor(caretaker_ppe_washing_interval),
    caretaker_ppe_per_unit = as.factor(caretaker_ppe_per_unit),
    caretaker_disease_management = as.factor(caretaker_disease_management),
    visitors_disease_management = as.factor(visitors_disease_management),
    visitors_contact_other_pigs = as.factor(visitors_contact_other_pigs),
    visitors_contact_poultry = as.factor(visitors_contact_poultry),
    return_to_service_rate = as.factor(return_to_service_rate),
    farrowing_rate = as.factor(farrowing_rate),
    piglets_per_sow_year = as.factor(piglets_per_sow_year),
    abortions_per_sow_year = as.factor(abortions_per_sow_year),
    piglet_mortality = as.factor(piglet_mortality),
    growing_rate_weaners = as.factor(growing_rate_weaners),
    growing_rate_fatteners = as.factor(growing_rate_fatteners),
    feed_conversion_rate_weaners = as.factor(feed_conversion_rate_weaners),
    feed_conversion_rate_fatteners = as.factor(feed_conversion_rate_fatteners),
    respiratory_history_swine = as.factor(respiratory_history_swine),
    time_respiratory_disease = as.factor(time_respiratory_disease),
    visitors_in_stable = as.factor(visitors_in_stable),
    symptom_severity = as.factor(symptom_severity),
    visitors_cumulative_contact_hours = as.factor(visitors_cumulative_contact_hours),
    ppe_visitors = as.factor(ppe_visitors),
    seperation_between_production_units = as.factor(seperation_between_production_units),
    seperation_within_production_units = as.factor(seperation_within_production_units),
    seperation_quarantaine_area = as.factor(seperation_quarantaine_area),
    bird_nests = as.factor(bird_nests),
    verification_outside_area_contact_poultry = as.factor(verification_outside_area_contact_poultry),
    verification_outside_area_contact_wild_birds = as.factor(verification_outside_area_contact_wild_birds),
    verification_contact_poultry_stable = as.factor(verification_contact_poultry_stable),
    verification_isolation_respiratory_dieseased_pigs = as.factor(verification_isolation_respiratory_dieseased_pigs),
    farrowing_air_quality = as.factor(farrowing_air_quality),
    farrowing_airspace_with_other_agegroup = as.factor(farrowing_airspace_with_other_agegroup),
    ai_sows_air_quality = as.factor(ai_sows_air_quality),
    ai_airspace_with_other_agegroup = as.factor(ai_airspace_with_other_agegroup),
    gilts_reduced_general_wellbeing = as.factor(gilts_reduced_general_wellbeing),
    gilts_air_quality = as.factor(gilts_air_quality),
    gilts_airspace_with_other_agegroup = as.factor(gilts_airspace_with_other_agegroup),
    weaners_air_quality = as.factor(weaners_air_quality),
    weaners_airspace_with_other_agegroup = as.factor(weaners_airspace_with_other_agegroup),
    fatteners_air_quality = as.factor(fatteners_air_quality),
    fatteners_airspace_with_other_agegroup = as.factor(fatteners_airspace_with_other_agegroup),
    caretaker_contact_other_pigs = as.factor(caretaker_contact_other_pigs),
    caretaker_contact_poultry = as.factor(caretaker_contact_poultry),
    production_cycle = factor(
      case_when(
        production_cycle %in% c(3,2,1,6,5,4,7) ~ as.character(production_cycle),
        TRUE ~ "other"
      ),
      levels = c("3","2","1","6","5","4","7","other")
    ),
    quarantaine_time = factor(
      case_when(
        quarantaine_time %in% c(16,4,3,6,5,20,2) ~ as.character(quarantaine_time),
        !is.na(quarantaine_time) ~ "other"
        
      ),
      levels = c("16", "4", "3", "6", "5", "20", "2", "other")
    ),
    source_of_contact = trimws(source_of_contact),
    source_of_contact_grouped = case_when(
      source_of_contact == "Schweineklinik Bern" ~ "Schweineklinik Bern",
      source_of_contact == "Tierarztpraxis Hutter" ~ "Tierarztpraxis Hutter",
      source_of_contact == "Jean-Luc Charbon" ~ "Jean-Luc Charbon",
      source_of_contact == "Vetteam Willisau" ~ "Vetteam Willisau",
      source_of_contact == "Stadtkind im Schweinestall" ~ "Stadtkind im Schweinestall",
      source_of_contact == "Tezet Müllheim" ~ "Tezet Müllheim",
      source_of_contact %in% c("SGD", "Qualiporc", "Profera", "Krieger AG Lüftungen") ~ "Company/Association",
      source_of_contact %in% c("Direkte Anfrage", "Philipp Egli", "Samuel Ritter", "Self report", "Christoph Meister", "Toni Schönbächler") ~ "Private Contact",
      TRUE ~ "Other Private Practices"
    ),
    source_of_contact_grouped = factor(
      source_of_contact_grouped,
      levels = c(
        "Schweineklinik Bern", "Tierarztpraxis Hutter", "Jean-Luc Charbon", 
        "Vetteam Willisau", "Stadtkind im Schweinestall", "Tezet Müllheim",
        "Company/Association", "Private Contact", "Other Private Practices"
      )
    ),
    Age_weeks_factor = factor(
      sapply(age_group_weeks, age_weeks_factor),
      levels = c("0-4w", "5-8w", "9-12w", "13+w", "mixed_ages<10w", "mixed_ages>10w")
    ),
    # Use pre-processed respiratory disease outbreak frequencies
    frequency_respi_outbreak = factor(
      df_temp$category,
      levels = c("never", "few pigs affected no outbreak", "once", "1x/year", "2x/year", "3x or more per year")
    ),
    weaners_animals_per_feeding_site_factor = factor(
      case_when(
        is.na(weaners_animals_per_feeding_site) ~ NA_character_,
        grepl("cm/Tier", weaners_animals_per_feeding_site, ignore.case = TRUE) ~ "trough",
        grepl("^[0-9]", weaners_animals_per_feeding_site) ~ as.character(
          cut(
            as.numeric(sub("^([0-9]+\\.?[0-9]*).*", "\\1", weaners_animals_per_feeding_site)),
            breaks = c(-Inf, 20, 30, 40, Inf),
            labels = c("<20", "<30", "<40", ">40"),
            right = FALSE
          )
        ),
        TRUE ~ NA_character_
      ),
      levels = c("<20", "<30", "<40", ">40", "trough")
    ),
    fatteners_feeding_site_per_animal_factor = factor(
      sapply(fatteners_feeding_site_width_per_animal, make_category),
      levels = category_levels
    )
  ) %>%
  # 7.3 Logical conversions (robust, explicit)
  mutate(
    Farrowing_on_farm   = as.logical(ifelse(number_suckling_piglets > 0, TRUE, FALSE)),
    Isemination_on_farm = as.logical(ifelse(number_boars > 0, TRUE, FALSE)),
    Gestation_on_farm   = as.logical(ifelse(number_boars > 0, TRUE, FALSE)),
    Weaners_on_farm     = as.logical(ifelse(number_weaners > 0, TRUE, FALSE)),
    Fattening_on_farm   = as.logical(ifelse(number_fattening_pigs > 0, TRUE, FALSE)),
    outside_area_contact_poultry = ifelse(outside_area_contact_poultry == 1, FALSE, TRUE),
    outside_area_contact_wild_birds = ifelse(outside_area_contact_wild_birds == 1, FALSE, TRUE),
    contact_poultry_in_stable = ifelse(contact_poultry_in_stable == 1, FALSE, TRUE),
    caretaker_hands_washed_before_entry = as.logical(caretaker_hands_washed_before_entry),
    respiratory_history_contact_person = ifelse(is.na(respiratory_history_contact_person), FALSE, TRUE),
    caretaker_boot_desinfection = ifelse(caretaker_boot_desinfection == 1, FALSE, TRUE),
    horses_closeby  = make_logical(grepl("\\b2\\b", other_animals)),
    dogs_closeby    = make_logical(grepl("\\b3\\b", other_animals)),
    chicken_closeby = make_logical(grepl("\\b4\\b", other_animals)),
    turkey_closeby  = make_logical(grepl("\\b5\\b", other_animals)),
    cattle_closeby  = make_logical(grepl("\\b6\\b", other_animals)),
    cats_closeby    = make_logical(grepl("\\b7\\b", other_animals)),
    symptom_swine_sneezing        = make_logical(grepl("\\b1\\b", symptom_type_swine)),
    symptom_swine_coughing        = make_logical(grepl("\\b2\\b", symptom_type_swine)),
    symptom_swine_nasal_discharge = make_logical(grepl("\\b3\\b", symptom_type_swine)),
    symptom_swine_fever           = make_logical(grepl("\\b4\\b", symptom_type_swine)),
    symptom_swine_feed_intake_red = make_logical(grepl("\\b5\\b", symptom_type_swine)),
    symptom_swine_apathy          = make_logical(grepl("\\b6\\b", symptom_type_swine)),
    symptom_swine_dyspnoea        = make_logical(grepl("\\b7\\b", symptom_type_swine)),
    symptom_human_sneezing         = make_logical(grepl("\\b1\\b", symptom_type_human)),
    symptom_human_coughing         = make_logical(grepl("\\b2\\b", symptom_type_human)),
    symptom_human_brobchitis       = make_logical(grepl("\\b3\\b", symptom_type_human)),
    symptom_human_pneumonia        = make_logical(grepl("\\b4\\b", symptom_type_human)),
    symptom_human_fever            = make_logical(grepl("\\b5\\b", symptom_type_human)),
    symptom_human_headache         = make_logical(grepl("\\b6\\b", symptom_type_human)),
    symptom_human_myalgia          = make_logical(grepl("\\b7\\b", symptom_type_human)),
    bird_nests = ifelse(bird_nests == 1, FALSE, TRUE),
    chronic_disease_conditon = ifelse(
      is.na(chronic_disease_conditon) | chronic_disease_conditon == "",
      FALSE,
      TRUE
    ),
    farrowings_nest_temperature = ifelse(
      is.na(farrowings_nest_temperature),
      NA,
      farrowings_nest_temperature == "acceptable"
    ),
    SIV_positive = no_positive_pigs > 0
  ) %>%
  # 7.4 Integer/Numeric conversions
  mutate(
    caretaker_number = as.integer(as.character(caretaker_number)),
    gilts_delta_room_temperature = as.numeric(gilts_delta_room_temperature),
    farrowing_delta_room_temperature = as.numeric(farrowing_delta_room_temperature),
    visitors_in_stable_recent = as.integer(visitors_in_stable_recent),
    visitors_cumulative_contact_hours = as.integer(visitors_cumulative_contact_hours),
    across(
      intersect(
        c(
          "farm_id", "total_no_of_samples", "no_positive_pigs", "herdsize",
          "number_suckling_piglets", "number_weaners", "number_fattening_pigs", "number_young_sows",
          "number_old_sows", "number_boars",
          "number_of_origins", "number_of_origins_suckiling_piglets", "number_of_origins_weaners", "number_of_origins_fattening_pigs",
          "number_of_origins_young_sows", "number_of_origins_old_sows", "number_of_origins_boars",
          "start_time_current_outbreak", "starting_point_current_disease"
        ), names(.)
      ),
      ~as.integer(.)
    ),
    across(
      intersect(
        c(
          "percentage_positive_pigs", "min_cp", "max_value", "average_cp", "std_dev",
          "farrowing_piglets_sneezing", "farrowing_piglets_coughing",
          "farrowing_delta_room_temperature", "farrowing_airflow",
          "ai_sows_delta_room_temperature", "ai_sows_airflow", "gilts_qm_per_animal", "gitls_animals_per_water_source",
          "gilts_delta_room_temperature", "gilts_airflow", "weaners_sneezing", "weaners_coughing",
          "weaners_qm_per_animal", "weaners_animals_per_water_source", "weaners_delta_room_temperature", "weaners_airflow",
          "fatteners_sneezing", "fatteners_coughing", "fatteners_qm_per_animal",
          "fatteners_animals_per_water_source", "fatteners_room_temperature", "fatteners_airflow",
          "percent_diseased_suckling_piglets", "percent_diseased_weaners", "percent_diseased_fatteners",
          "percent_diseased_young_sows", "percent_diseased_old_sows", "percent_diseased_boars",
          "visitors_cumulative_contact_hours", "weaners_reduced_general_wellbeing", "weaners_discharge",
          "fatteners_reduced_general_wellbeing", "fatteners_discharge", "fatteners_delta_room_temperature"
        ), names(.)
      ),
      as.numeric
    )
  ) %>%
  # 7.5 Renaming and relocating columns for logical grouping
  rename(
    farrowing_piglet_litters_sneezing_percentage = farrowing_piglets_sneezing,
    farrowing_piglet_litters_coughing_percentage = farrowing_piglets_coughing,
    farrowing_room_temperature = farrowing_delta_room_temperature,
    weaners_room_temperature = weaners_delta_room_temperature,
    farrowing_nest_temperature_ok = farrowings_nest_temperature,
    ai_sows_room_temperature = ai_sows_delta_room_temperature,
    gilts_room_temperature = gilts_delta_room_temperature,
    fatteners_room_temperature = fatteners_delta_room_temperature,
    chronic_disease_condition = chronic_disease_conditon,
    max_cp = max_value, 
    contact_bird_in_stable = contact_poultry_in_stable
  ) %>%
  relocate(
    SIV_positive, .after = farm_id
  ) %>%
  relocate(
    total_no_of_samples, no_positive_pigs, percentage_positive_pigs,
    min_cp, max_cp, average_cp, std_dev, symptomatic_report,
    .after = SIV_positive
  ) %>%
  relocate(
    date_sampling, canton, herdsize, production_type,
    Farrowing_on_farm, Isemination_on_farm, Gestation_on_farm, Weaners_on_farm, Fattening_on_farm,
    .after = std_dev
  ) %>%
  relocate(
    weaners_animals_per_feeding_site, .before = weaners_animals_per_water_source
  ) %>%
  relocate(
    weaners_animals_per_feeding_site_factor, .after = weaners_animals_per_feeding_site
  ) %>%
  relocate(
    fatteners_feeding_site_width_per_animal, .before = fatteners_animals_per_water_source
  ) %>%
  relocate(
    fatteners_feeding_site_per_animal_factor, .after = fatteners_feeding_site_width_per_animal
  ) %>%
  relocate(
    horses_closeby, dogs_closeby, chicken_closeby, turkey_closeby, cattle_closeby, cats_closeby, .after = other_animals
  ) %>%
  relocate(
    symptom_swine_sneezing, symptom_swine_coughing, symptom_swine_nasal_discharge,
    symptom_swine_fever, symptom_swine_feed_intake_red, symptom_swine_apathy, symptom_swine_dyspnoea,
    .after = symptom_type_swine
  ) %>%
  relocate(
    symptom_human_sneezing, symptom_human_coughing, symptom_human_brobchitis,
    symptom_human_pneumonia, symptom_human_fever, symptom_human_headache, symptom_human_myalgia,
    .after = symptom_type_human
  ) %>%
  relocate(
    Age_weeks_factor, .after = age_group_weeks
  ) %>%
  relocate(
    frequency_respi_outbreak, .after = frequency_respiratory_disease_outbreaks
  ) %>%
  relocate(
    source_of_contact_grouped, .after = source_of_contact
  ) %>%
  relocate(
    weaners_animals_per_feeding_site_factor, .after = weaners_qm_per_animal
  )

# ---- 8. Save cleaned data to RDS ----
#Uncomment to save data after validation
saveRDS(df2, file.path(dirs[2], "siv_farms_cleaned.rds"))


# ---- 9. Quick Data Overview ----
# Use these for further inspection and QA
# glimpse(df2)
# DT::datatable(df2)
# utils::View(df2)
# sapply(df2, class)
#rmarkdown::render("eda_report.Rmd", 
#output_file = "siv_farms_First_lokk.html", 
#output_dir = "04_output", 
#envir = globalenv())


# Script metadata
# Generated by: Jonas Alexander Steiner
# Last updated: 2025-07-11




## Creation of df3 after first look on the EDA
# ------------------------------------------------------------------------------
# Best practice annotation:
# This script processes the cleaned SIV farms data (df2) into a new dataframe (df3)
# with domain-specific transformations and column selection, then renders an RMarkdown
# report for descriptive statistics. All code is explicit and reproducible.
# ------------------------------------------------------------------------------
# Author: jonasalexandersteiner
# Date: 2025-07-11
# Project: SIV_Projekt - SIV farms data processing and reporting
# ------------------------------------------------------------------------------
# Prerequisites:
# - df2 must be loaded in the workspace (see data_cleaning_pipeline)
# - Required R packages: tidyverse, rmarkdown, htmlwidgets, DT
# - RMarkdown file 'eda_report2.Rmd' must exist in the working directory
# ------------------------------------------------------------------------------

df3 <- df2 %>%
  # 1. Exclude all rows of Farm_ID 2, 8, and 106 (outliers or problematic farms)
  filter(!(farm_id %in% c(2, 8, 106))) %>%
  
  # 2. Mutations and transformations (see domain documentation for details)
   

   mutate(
     
    #create ili_signs
     ili_signs = case_when(
       farm_id %in% c(5, 7,71,72,73) ~ TRUE,  # Manual override for outbreak farms: 5,7 piglet outbreak, 71/72/73 external observer no CI/SI
       if_all(
         c("weaners_coughing", "fatteners_coughing",
           "rectal_temperature_max",
           "weaners_sneezing", "fatteners_sneezing",
           "weaners_reduced_general_wellbeing", "fatteners_reduced_general_wellbeing"),
         is.na
       ) ~ NA,
       TRUE ~ (
         coalesce(weaners_coughing >= 2.5, FALSE) |
           coalesce(fatteners_coughing >= 2.5, FALSE) |
           (
             coalesce(rectal_temperature_max >= 40.5, FALSE) &
               (
                 coalesce(weaners_sneezing >= 20, FALSE) |
                   coalesce(fatteners_sneezing >= 20, FALSE) |
                   coalesce(weaners_reduced_general_wellbeing, FALSE) |
                   coalesce(fatteners_reduced_general_wellbeing, FALSE)
               )
           )
       )
     ),
     
    # Format date_sampling as "MON YYYY" (e.g. "JAN 2024")
    date_sampling = paste(toupper(format(date_sampling, "%b")), format(date_sampling, "%Y")),        
    
    
    
    # 5d. Safe pattern matching function (error-proof)
      
      # Create season_sampling as a factor with 4 levels based on month in date_sampling
      season_sampling = factor(
        case_when(
          str_detect(date_sampling, "DEC|JAN|FEB") ~ "winter",
          str_detect(date_sampling, "MAR|APR|MAY") ~ "spring",
          str_detect(date_sampling, "JUN|JUL|AUG") ~ "summer",
          str_detect(date_sampling, "SEP|OCT|NOV") ~ "autumn",
          TRUE ~ NA_character_
        ),
        levels = c("winter", "spring", "summer", "autumn")
      ),
    
    
    
    # Canton_factor: aggregate Swiss cantons into study regions to ensure sufficient observations per level
    canton_factor = case_when(
      canton %in% c("Aargau", "Basel Land") ~ "North-West",
      canton %in% c("Bern", "Solothurn") ~ "Berne + Solothurn",
      canton %in% c("Schaffhausen", "Solothurn", "St. Gallen", "Thurgau", "Z\u00fcrich") ~ "East & Zurich",
      canton %in% c("Fribourg", "Waadt", "Jura") ~ "Romandy",
      canton == "Luzern" ~ "Lucerne",
      TRUE ~ as.character(canton)
    ) %>% factor(levels = c(
      "North-West",
      "Berne + Solothurn",
      "East & Zurich",
      "Romandy",
      "Lucerne"
    )),
    
    # Production_type_factor: group production types into 5 categories to ensure sufficient observations per level
    production_type_factor = case_when(
      production_type %in% c(1, 11, 12) ~ 1L,
      production_type  %in% c(2, 4) ~ 2L,
      production_type == 3 ~ 3L,
      production_type %in% c(5, 6, 7, 8) ~ 4L,
      production_type %in% c(9, 10) ~ 5L,
      TRUE ~ NA_integer_
    ) %>% factor(levels = 1:5),
    
    # Replace zeros with NA in all number_* columns
    across(starts_with("number_"), ~na_if(., 0)),
    
    # Simplify production_cycle into factor levels 1/2/3 or 'other' to ensure sufficient observations per level
    production_cycle = factor(
      case_when(
        production_cycle %in% c(1, 2, 3) ~ as.character(production_cycle),
        !is.na(production_cycle) ~ "other",
        TRUE ~ NA_character_
      ),
      levels = c("1", "2", "3", "other")
    ),
    
    # Transform occupancy and cleaning variables to binary/factor to ensure sufficient observations per level 
    mode_stable_occupation_ai_centre = case_when(
      is.na(mode_stable_occupation_ai_centre) ~ NA,
      mode_stable_occupation_ai_centre == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    mode_stable_occupation_gilts_stable = case_when(
      is.na(mode_stable_occupation_gilts_stable) ~ NA,
      mode_stable_occupation_gilts_stable == 6 ~ TRUE,
      TRUE ~ FALSE
    ),
    mode_stable_occupation_farrowing_stable = case_when(
      is.na(mode_stable_occupation_farrowing_stable) ~ NA,
      mode_stable_occupation_farrowing_stable == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    litter_equalization_farrowing_stable = factor(case_when(
      is.na(litter_equalization_farrowing_stable) ~ NA_character_,
      litter_equalization_farrowing_stable %in% c(1,2,3) ~ "1",
      litter_equalization_farrowing_stable == 4 ~ "2",
      litter_equalization_farrowing_stable == 5 ~ "3",
    ), levels = c("1","2","3")),
    mode_stable_occupation_weaner_stable = case_when(
      is.na(mode_stable_occupation_weaner_stable) ~ NA,
      mode_stable_occupation_weaner_stable == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    mode_stable_occupation_fattener_stable = case_when(
      is.na(mode_stable_occupation_fattener_stable) ~ NA,
      mode_stable_occupation_fattener_stable == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    outside_area = if_any(starts_with("outside_area"), ~.x == TRUE),
    cleaning_ai_centre = factor(case_when(
      is.na(cleaning_ai_centre) ~ NA_character_,
      cleaning_ai_centre == 2 ~ "1",
      cleaning_ai_centre == 4 ~ "2",
      cleaning_ai_centre == 5 ~ "3"
    ), levels = c("1","2","3")),
    cleaning_gilts_stable = case_when(
      is.na(cleaning_gilts_stable) ~ NA,
      cleaning_gilts_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    cleaning_farrowing_stable = case_when(
      is.na(cleaning_farrowing_stable) ~ NA,
      cleaning_farrowing_stable == 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    cleaning_weaner_stable = case_when(
      is.na(cleaning_weaner_stable) ~ NA,
      cleaning_weaner_stable == 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    cleaning_fattener_stable = factor(case_when(
      is.na(cleaning_fattener_stable) | cleaning_fattener_stable == 1 ~ NA_character_,
      cleaning_fattener_stable == 2 ~ "1",
      cleaning_fattener_stable %in% c(3,4) ~ "2",
      cleaning_fattener_stable %in% c(5) ~ "3"
    ), levels = c("1","2","3")),
    cleaning_quarantaine = factor(case_when(
      is.na(cleaning_quarantaine) | cleaning_quarantaine == 1 ~ NA_character_,
      cleaning_quarantaine == 2 ~ "1",
      cleaning_quarantaine == 4 ~ "2",
      cleaning_quarantaine == 5 ~ "3"
    ), levels = c("1","2","3")),
    desinfection_ai_centre = case_when(
      is.na(desinfection_ai_centre) ~ NA,
      desinfection_ai_centre == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    desinfection_gilts_stable = case_when(
      is.na(desinfection_gilts_stable) ~ NA,
      desinfection_gilts_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    desinfection_farrowing_stable = factor(case_when(
      is.na(desinfection_farrowing_stable) ~ NA_character_,
      desinfection_farrowing_stable == 2 ~ "1",
      desinfection_farrowing_stable == 4 ~ "2",
      desinfection_farrowing_stable == 5 ~ "3"
    ), levels = c("1", "2", "3")),
    desinfection_weaner_stable = factor(case_when(
      is.na(desinfection_weaner_stable) ~ NA_character_,
      desinfection_weaner_stable == 2 ~ "1",
      desinfection_weaner_stable %in% c(3,4) ~ "2",
      desinfection_weaner_stable == 5 ~ "3"
    ), levels = c("1", "2", "3")),
    desinfection_fattener_stable = case_when(
      is.na(desinfection_fattener_stable) ~ NA,
      desinfection_fattener_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    desinfection_quarantaine = case_when(
      is.na(desinfection_quarantaine) | desinfection_quarantaine == 1 ~ NA,
      desinfection_quarantaine == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    drying_ai_centre = factor(case_when(
      is.na(drying_ai_centre) ~ NA_character_,
      drying_ai_centre == 2 ~ "1",
      drying_ai_centre == 4 ~ "2",
      drying_ai_centre == 5 ~ "3"
    ), levels = c("1", "2", "3")),
    drying_gilts_stable = case_when(
      is.na(drying_gilts_stable) ~ NA,
      drying_gilts_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    drying_farrowing_stable = case_when(
      is.na(drying_farrowing_stable) ~ NA,
      drying_farrowing_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    drying_weaner_stable = case_when(
      is.na(drying_weaner_stable) ~ NA,
      drying_weaner_stable == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    drying_fattener_stable = factor(case_when(
      is.na(drying_fattener_stable) | drying_fattener_stable == 1 ~ NA_character_,
      drying_fattener_stable == 2 ~ "1",
      drying_fattener_stable %in% c(3,4) ~ "2",
      drying_fattener_stable == 5 ~ "3"
    ), levels = c("1", "2", "3")),
    drying_quarantaine = case_when(
      is.na(drying_quarantaine) | drying_quarantaine == 1 ~ NA,
      drying_quarantaine == 2 ~ FALSE,
      TRUE ~ TRUE
    ),
    cleaning_desinfection_transport_vehicle = case_when(
      is.na(cleaning_desinfection_transport_vehicle) | cleaning_desinfection_transport_vehicle == 1 ~ NA,
      cleaning_desinfection_transport_vehicle == 5 ~ TRUE,
      cleaning_desinfection_transport_vehicle %in% c(2,4) ~ FALSE
    ),
    cleaning_shipment_area = case_when(
      is.na(cleaning_shipment_area) ~ NA,
      cleaning_shipment_area == 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    caretaker_ppe_stable = case_when(
      caretaker_ppe_stable == 3 ~ TRUE,
      caretaker_ppe_stable %in% c(1, 2) ~ FALSE,
      TRUE ~ NA
    ),
    caretaker_ppe_washing_interval = case_when(
      is.na(caretaker_ppe_washing_interval) | caretaker_ppe_washing_interval == 1 ~ NA,
      caretaker_ppe_washing_interval == 4 ~ FALSE,
      caretaker_ppe_washing_interval == 5 ~ TRUE
    ),
    caretaker_ppe_per_unit = case_when(
      is.na(caretaker_ppe_per_unit) ~ NA,
      caretaker_ppe_per_unit == 1 ~ FALSE,
      caretaker_ppe_per_unit %in% c(2,3) ~ TRUE
    ),
    caretaker_disease_management = case_when(
      is.na(caretaker_disease_management) | caretaker_disease_management == 1 ~ NA,
      caretaker_disease_management == 2 ~ FALSE,
      caretaker_disease_management %in% c(3,4) ~ TRUE
    ),
    caretaker_contact_other_pigs = case_when(
      caretaker_contact_other_pigs %in% c(3, 4) ~ TRUE,
      caretaker_contact_other_pigs %in% c(1, 2) ~ FALSE,
      caretaker_contact_other_pigs == 5 | is.na(caretaker_contact_other_pigs) ~ NA
    ),
    caretaker_contact_poultry = case_when(
      is.na(caretaker_contact_poultry) | caretaker_contact_poultry == 5 ~ NA,
      caretaker_contact_poultry %in% c(3,4) ~ TRUE,
      caretaker_contact_poultry %in% c(1,2) ~ FALSE
    ),
    ppe_visitors = case_when(
      is.na(ppe_visitors) ~ NA,
      ppe_visitors == 3 ~ TRUE,
      ppe_visitors == 1 ~ FALSE
    ),
    visitors_disease_management = case_when(
      is.na(visitors_disease_management) | visitors_disease_management == 1 ~ NA,
      visitors_disease_management %in% c(3,4) ~ TRUE,
      visitors_disease_management == 2 ~ FALSE
    ),
    visitors_contact_other_pigs = case_when(
      visitors_contact_other_pigs == 4 ~ TRUE,
      visitors_contact_other_pigs %in% c(1, 2, 3) ~ FALSE,
      visitors_contact_other_pigs == 5 | is.na(visitors_contact_other_pigs) ~ NA
    ),
    return_to_service_rate = case_when(
      is.na(return_to_service_rate) ~ NA,
      return_to_service_rate == 1 ~ TRUE,
      return_to_service_rate == 2 ~ FALSE
    ),
    farrowing_rate = case_when(
      is.na(farrowing_rate) ~ NA,
      farrowing_rate == 1 ~ TRUE,
      farrowing_rate == 2 ~ FALSE
    ),
    piglets_per_sow_year = case_when(
      is.na(piglets_per_sow_year) ~ NA,
      piglets_per_sow_year == 1 ~ TRUE,
      piglets_per_sow_year == 2 ~ FALSE
    ),
    abortions_per_sow_year = case_when(
      is.na(abortions_per_sow_year) ~ NA,
      abortions_per_sow_year == 1 ~ TRUE,
      abortions_per_sow_year %in% c(2, 3) ~ FALSE
    ),
    piglet_mortality = case_when(
      is.na(piglet_mortality) ~ NA,
      piglet_mortality == 1 ~ TRUE,
      piglet_mortality == 2 ~ FALSE
    ),
    feed_conversion_rate_fatteners = case_when(
      is.na(feed_conversion_rate_fatteners) ~ NA,
      feed_conversion_rate_fatteners == 1 ~ TRUE,
      feed_conversion_rate_fatteners == 2 ~ FALSE
    ),
    respiratory_history_swine = case_when(
      respiratory_history_swine == 1 ~ NA_integer_,
      respiratory_history_swine == 2 ~ 1,
      respiratory_history_swine %in% c(3, 4) ~ 2,
      respiratory_history_swine == 5 ~ 3
    ),respiratory_history_swine = factor(respiratory_history_swine),
    time_respiratory_disease = case_when(
      is.na(time_respiratory_disease) | time_respiratory_disease == 1 ~ NA,
      time_respiratory_disease %in% c(2, 3) ~ FALSE,
      time_respiratory_disease == 4 ~ TRUE
    ),
    suckling_piglets_diseased = percent_diseased_suckling_piglets != 0,
    weaners_diseased = percent_diseased_weaners != 0,
    fatteners_diseased = percent_diseased_fatteners != 0,
    young_sows_diseased = percent_diseased_young_sows != 0,
    old_sows_diseased = percent_diseased_old_sows != 0,
    boars_diseased = percent_diseased_boars != 0,
    report_killed_suckling_piglets = percent_killed_suckling_piglets != 0,
    report_killed_weaners = percent_killed_weaners != 0,
    report_killed_fatteners = percent_killed_fatteners != 0,
    report_killed_young_sows = percent_killed_young_sows != 0,
    report_killed_old_sows = percent_killed_old_sows != 0,
    starting_point_current_disease = na_if(starting_point_current_disease, 0),
    symptom_severity = case_when(
      is.na(symptom_severity) ~ NA,
      symptom_severity == 1 ~ FALSE,
      symptom_severity %in% c(2,3) ~ TRUE
    ),
    seperation_between_production_units = factor(
      case_when(
        seperation_between_production_units == 2 ~ "1",
        seperation_between_production_units == 3 ~ "2",
        seperation_between_production_units %in% c(1, 4) ~ "3",
        TRUE ~ NA_character_
      ),
      levels = c("1", "2", "3"),
    ),
    seperation_within_production_units = factor(
      case_when(
        seperation_within_production_units == 2 ~ "1",
        seperation_within_production_units == 3 ~ "2",
        seperation_within_production_units %in% c(1, 4) ~ "3",
        TRUE ~ NA_character_
      ),
      levels = c("1", "2", "3"),
    ),
    seperation_quarantaine_area = factor(
      case_when(
        seperation_quarantaine_area == 2 ~ "1",
        seperation_quarantaine_area == 3 ~ "2",
        seperation_quarantaine_area %in% c(1, 4) ~ "3",
        TRUE ~ NA_character_
      ),
      levels = c("1", "2", "3"),
    ),
    verification_outside_area_contact_poultry = case_when(
      is.na(verification_outside_area_contact_poultry) ~ NA,
      verification_outside_area_contact_poultry == 1 ~ FALSE,
      verification_outside_area_contact_poultry %in% c(3,4) ~ TRUE
    ),
    verification_outside_area_contact_wild_birds = case_when(
      is.na(verification_outside_area_contact_wild_birds) ~ NA,
      verification_outside_area_contact_wild_birds == 1 ~ FALSE,
      verification_outside_area_contact_wild_birds %in% c(3,4) ~ TRUE
    ),
    verification_contact_poultry_stable = case_when(
      is.na(verification_contact_poultry_stable) ~ NA,
      verification_contact_poultry_stable == 1 ~ FALSE,
      verification_contact_poultry_stable %in% c(3,4) ~ TRUE
    ),
    farrowing_sows_coughing = farrowing_sows_coughing != 0,
    farrowing_piglets_reduced_general_wellbeing = farrowing_piglets_reduced_general_wellbeing != 0,
    weaners_reduced_general_wellbeing = weaners_reduced_general_wellbeing != 0,
    weaners_discharge = weaners_discharge != 0,
    fatteners_reduced_general_wellbeing = fatteners_reduced_general_wellbeing != 0,
    fatteners_discharge = fatteners_discharge != 0,
    farrowing_airspace_with_other_agegroup = factor(case_when(
      is.na(farrowing_airspace_with_other_agegroup) ~ NA_character_,
      farrowing_airspace_with_other_agegroup == 2 ~ "1",
      farrowing_airspace_with_other_agegroup == 3 ~ "2",
      farrowing_airspace_with_other_agegroup == 4 ~ "3"
    ), levels = c("1", "2", "3")),
    ai_airspace_with_other_agegroup = factor(case_when(
      is.na(ai_airspace_with_other_agegroup) ~ NA_character_,
      ai_airspace_with_other_agegroup == 2 ~ "1",
      ai_airspace_with_other_agegroup == 3 ~ "2",
      ai_airspace_with_other_agegroup == 4 ~ "3"
    ), levels = c("1", "2", "3")),
    gilts_airspace_with_other_agegroup = factor(case_when(
      is.na(gilts_airspace_with_other_agegroup) ~ NA_character_,
      gilts_airspace_with_other_agegroup == 2 ~ "1",
      gilts_airspace_with_other_agegroup == 3 ~ "2",
      gilts_airspace_with_other_agegroup == 4 ~ "3"
    ), levels = c("1", "2", "3"))
  ) %>%
  # 3. Remove columns
  dplyr::select(
    -any_of(c(
      

      #redundant
      "canton",  "production_type", "source_of_contact", "frequency_respiratory_disease_outbreaks",
      "other_animals", "quarantaine_suckling_piglets", "quarantaine_weaners", "quarantaine_fatteners", 
      "quarantaine_young_sows", "quarantaine_old_sows", "quarantaine_boars", "visitors_in_stable","fatteners_feeding_site_width_per_animal", "outbreak_since_examination_description",
      "symptom_type_swine", "symptom_type_human", "percent_killed_boars",
      "percent_diseased_suckling_piglets", "percent_diseased_weaners", "percent_diseased_fatteners",
      "percent_diseased_young_sows", "percent_diseased_old_sows", "percent_diseased_boars",
      "percent_killed_suckling_piglets", "percent_killed_weaners", "percent_killed_fatteners",
      "percent_killed_young_sows", "percent_killed_old_sows", "age_group_weeks", "weaners_animals_per_feeding_site",
      "number_of_origins_suckiling_piglets", "number_of_origins_weaners", "number_of_origins_fattening_pigs",
      "number_of_origins_young_sows", "number_of_origins_old_sows", "number_of_origins_boars", "notes",

      #no variance or all NA
      "growing_rate_weaners","growing_rate_fatteners", "feed_conversion_rate_weaners", "influenza_diagnosis_hum_an",
      "antiviral_treatment", "farrowing_sows_reduced_general_wellbeing", "farrowing_sows_sneezing", "farrowing_sows_nasal_discharge",
      "farrowing_piglets_nasal_discharge",
      "ai_sows_reduced_general_wellbeing", "ai_sows_sneezing", "ai_sows_coughing", "ai_sows_discharge",
      "gilts_reduced_general_wellbeing", "gilts_sneezing", "gilts_coughing", "gilts_discharge","sgd_qgs",
      
      
      #data integrity concerns as answers of farmers where mostly too inaccurate
      "verification_isolation_respiratory_dieseased_pigs","isolation_respiratory_dieseased_pigs","visitors_contact_poultry"
      
       
    ))
  )%>%
  
  # 4. Relocate new or renamed variables directly after old ones for clarity
  relocate(
    canton_factor, .after = date_sampling
  ) %>%
  relocate(
    production_type_factor, .after = herdsize
  ) %>%
  relocate(
    suckling_piglets_diseased, .after = outbreak_since_examination
  ) %>%
  relocate(
    weaners_diseased, .after = suckling_piglets_diseased
  ) %>%
  relocate(
    fatteners_diseased, .after = weaners_diseased
  ) %>%
  relocate(
    young_sows_diseased, .after = fatteners_diseased
  ) %>%
  relocate(
    old_sows_diseased, .after =  young_sows_diseased
  ) %>%
  relocate(
    boars_diseased, .after = old_sows_diseased
  ) %>%
  relocate(
    report_killed_suckling_piglets, .after =  boars_diseased
  ) %>%
  relocate(
    report_killed_weaners, .after = report_killed_suckling_piglets
  ) %>%
  relocate(
    report_killed_fatteners, .after = report_killed_weaners
  ) %>%
  relocate(
    report_killed_young_sows, .after = report_killed_fatteners
  ) %>%
  relocate(
    report_killed_old_sows, .after = report_killed_young_sows
  ) %>%
  relocate(
    outside_area, .before = outside_area_ai_centre
  ) %>%
  relocate(
    season_sampling, .after = date_sampling
  ) %>%
  relocate(ili_signs, .after = Fattening_on_farm)



# ------------------------------------------------------------------------------
# Quick overview (optional: uncomment for interactive use)
# glimpse(df3)
# DT::datatable(df3) %>%
#   htmlwidgets::saveWidget("df3_table.html")
# sapply(df3, class)
# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Render R Markdown for descriptive statistics report.
# - The report 'eda_report2.Rmd' uses df3 from the global environment.
# - Output file is saved in the '04_output' folder.
# ------------------------------------------------------------------------------
#rmarkdown::render(
#"eda_report2.Rmd", 
#output_file = "siv_farms_descriptive_stats.html", 
#output_dir = "04_output", 
#envir = globalenv()
#)

# =====================================================================
# VISUAL SUMMARY PLOTS FOR KEY VARIABLES IN df3
# ---------------------------------------------------------------------
# Creates and saves (in output_folder):
#   - Pie charts for SIV_positive, symptomatic_report, ili_signs (TRUE vs FALSE, percent labels)
#   - Bar chart for no_positive_pigs (excluding 0)
#   - Whisker (box) plot for average_cp, stratified by ili_signs (TRUE/FALSE)
# =====================================================================
# Best Practice:
# - Use tidy evaluation (.data[[var]]) in ggplot2 aesthetics (never aes_string).
# - Always filter out NA, Inf, -Inf before plotting continuous variables.
# - Use colorblind-friendly palettes for categorical variables.
# - Save plots in a reproducible output folder.
# - Modular plotting functions for easy extension.
# =====================================================================

library(ggplot2)
library(dplyr)
library(scales)

# ------------------ User-Defined Output Folder -----------------------
output_folder <- "04_output"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# ------------------ PIE CHART FUNCTION -------------------------------
# Plots TRUE vs FALSE proportions, labels percent on each slice.
# - Uses factor conversion for logicals (ensures TRUE, FALSE order).
# - Handles NA as a separate slice if present.
# - Colorblind-friendly palette.
save_pie_chart <- function(df, var, filename, title) {
  tab <- df %>%
    mutate(!!var := factor(.data[[var]], levels = c(TRUE, FALSE))) %>%
    group_by(.data[[var]]) %>%
    summarise(n = n()) %>%
    mutate(
      perc = n / sum(n) * 100,
      label = paste0(ifelse(is.na(.data[[var]]), "NA", as.character(.data[[var]])), "\n", sprintf("%.1f%%", perc))
    )
  gg <- ggplot(tab, aes(x = "", y = perc, fill = .data[[var]])) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C", "NA"="#999999")) +
    labs(title = title, fill = var) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  ggsave(filename, gg, width = 6, height = 6)
}

# ------------------ BAR CHART FUNCTION -------------------------------
# Plots counts for non-zero values of a discrete variable.
# - Uses tidy evaluation for aesthetics.
# - Excludes zero values for cleaner presentation.
# - Handles missing values robustly.
save_bar_chart <- function(df, var, filename, title) {
  tab <- df %>%
    filter(.data[[var]] > 0 & !is.na(.data[[var]])) %>%
    count(.data[[var]])
  gg <- ggplot(tab, aes(x = .data[[var]], y = n)) +
    geom_bar(stat = "identity", fill = "#377EB8") +
    labs(title = title, x = var, y = "Count") +
    theme_minimal(base_size = 16)
  ggsave(filename, gg, width = 8, height = 6)
}

# ------------------ WHISKER (BOX) PLOT FUNCTION ----------------------
# Compares distributions of a continuous variable by binary grouping.
# - Filters out non-finite values (best practice for boxplots).
# - Ensures group_var is a factor with TRUE, FALSE order.
# - Colorblind-friendly fill.
save_whisker_plot <- function(df, value_var, group_var, filename, title) {
  df <- df %>%
    filter(is.finite(.data[[value_var]])) %>%
    mutate(!!group_var := factor(.data[[group_var]], levels = c(TRUE, FALSE)))
  gg <- ggplot(df, aes(x = .data[[group_var]], y = .data[[value_var]], fill = .data[[group_var]])) +
    geom_boxplot(width = 0.6, outlier.colour = "gray", outlier.shape = 16) +
    scale_fill_manual(values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C")) +
    labs(title = title, x = group_var, y = value_var) +
    theme_minimal(base_size = 16)
  ggsave(filename, gg, width = 8, height = 6)
}


# ------------------ BAR CHART: SIV_positive % by ili_signs --------------------
# This plot shows the percentage of SIV_positive in the ili_signs TRUE and FALSE groups.

save_SIV_positive_bar_by_ili <- function(df, SIV_var, group_var, filename, title) {
  # Prepare table: group by group_var and SIV_var, then calculate percent positive within each group
  tab <- df %>%
    filter(!is.na(.data[[SIV_var]]), !is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]], .data[[SIV_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[group_var]]) %>%
    mutate(
      perc = n / sum(n) * 100,
      SIV_label = ifelse(.data[[SIV_var]] == TRUE, "SIV+", "SIV-")
    )
  
  gg <- ggplot(tab %>% filter(SIV_label == "SIV+"),
               aes(x = factor(.data[[group_var]], levels = c(TRUE, FALSE)),
                   y = perc, fill = factor(.data[[group_var]], levels = c(TRUE, FALSE)))) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", perc)), vjust = -0.5, size = 6) +
    scale_fill_manual(values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C")) +
    scale_x_discrete(labels = c("TRUE" = "TRUE", "FALSE" = "FALSE")) +
    labs(title = title,
         x = "ili symptoms",
         y = "Percentage SIV positive",
         fill = "ili symptoms") +
    ylim(0, 100) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  ggsave(filename, gg, width = 8, height = 6)
}

# ------------------ PIE CHART: ili_signs TRUE/FALSE among SIV_positive --------------------
# This plot shows the percentage of ili_signs TRUE and FALSE among SIV_positive farms.

save_ili_signs_pie_among_SIVpos <- function(df, SIV_var, ili_var, filename, title) {
  tab <- df %>%
    filter(.data[[SIV_var]] == TRUE & !is.na(.data[[ili_var]])) %>%
    group_by(.data[[ili_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      perc = n / sum(n) * 100,
      label = paste0(ifelse(is.na(.data[[ili_var]]), "NA", as.character(.data[[ili_var]])), "\n", sprintf("%.1f%%", perc))
    )
  gg <- ggplot(tab, aes(x = "", y = perc, fill = factor(.data[[ili_var]], levels = c(TRUE, FALSE)))) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C")) +
    labs(title = title, fill = "ili symptoms") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  ggsave(filename, gg, width = 6, height = 6)
}

# ------------------ BOX PLOT: number_weaners by SIV_positive Group --------------------
# This plot shows the distribution of number_weaners for SIV_positive TRUE and FALSE groups.

save_number_weaners_boxplot_by_SIVpos <- function(df, weaners_var, SIV_var, filename, title) {
  # Only include rows with non-missing number_weaners and SIV_positive
  df_plot <- df %>%
    filter(!is.na(.data[[weaners_var]]), !is.na(.data[[SIV_var]])) %>%
    mutate(
      SIV_positive = factor(.data[[SIV_var]], levels = c(TRUE, FALSE))
    )
  gg <- ggplot(df_plot, aes(x = SIV_positive, y = .data[[weaners_var]], fill = SIV_positive)) +
    geom_boxplot(width = 0.6, outlier.colour = "gray", outlier.shape = 16) +
    scale_fill_manual(values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C")) +
    scale_x_discrete(labels = c("TRUE" = "positive", "FALSE" = "negative")) +
    labs(
      title = title,
      x = "SIV positive",
      y = "Number of weaners",
      fill = "SIV"
    ) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  ggsave(filename, gg, width = 8, height = 6)
}

# ------------------ PROPORTION BAR PLOT: contact_bird_in_stable by SIV_positive -------------------
# This plot shows the proportion of TRUE and FALSE for contact_bird_in_stable in SIV_positive TRUE and FALSE groups.

save_contact_bird_in_stable_propplot_by_SIVpos <- function(df, contact_var, SIV_var, filename, title) {
  # Prepare data: drop NAs in both variables
  tab <- df %>%
    filter(!is.na(.data[[contact_var]]), !is.na(.data[[SIV_var]])) %>%
    group_by(contact_bird_in_stable = .data[[contact_var]], SIV_positive = .data[[SIV_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(contact_bird_in_stable) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  gg <- ggplot(tab, aes(
    x = factor(contact_bird_in_stable, levels = c(TRUE, FALSE)),
    y = prop,
    fill = factor(SIV_positive, levels = c(TRUE, FALSE))
  )) +
    geom_col(width = 0.6, position = "fill") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text(
      aes(label = scales::percent(prop, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 6, color = "white"
    ) +
    scale_fill_manual(
      values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C"),
      labels = c("TRUE" = "positive", "FALSE" = "negative")
    ) +
    scale_x_discrete(
      labels = c("TRUE" = "Contact: YES", "FALSE" = "Contact: NO")
    ) +
    labs(
      title = title,
      x = "Contact with birds in stable",
      y = "Proportion",
      fill = "SIV"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.position = "top"
    )
  ggsave(filename, gg, width = 8, height = 6)
}



# ------------------ PROPORTION BAR PLOT: SIV_positive by respiratory_history_human -------------------
# This plot shows, for each group of respiratory_history_human (TRUE/FALSE), the proportion of SIV_positive (TRUE/FALSE).

save_SIVpositive_propplot_by_resp_history_human <- function(df, SIV_var, resp_var, filename, title) {
  # Prepare data: drop NAs in both variables
  tab <- df %>%
    filter(!is.na(.data[[resp_var]]), !is.na(.data[[SIV_var]])) %>%
    group_by(respiratory_history_human = .data[[resp_var]], SIV_positive = .data[[SIV_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(respiratory_history_human) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  gg <- ggplot(tab, aes(
    x = factor(respiratory_history_human, levels = c(TRUE, FALSE)),
    y = prop,
    fill = factor(SIV_positive, levels = c(TRUE, FALSE))
  )) +
    geom_col(width = 0.6, position = "fill") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text(
      aes(label = scales::percent(prop, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 6, color = "white"
    ) +
    scale_fill_manual(
      values = c("TRUE"="#4DAF4A", "FALSE"="#E41A1C"),
      labels = c("TRUE" = "positive", "FALSE" = "negative")
    ) +
    scale_x_discrete(
      labels = c("TRUE" = "Resp. history: YES", "FALSE" = "Resp. history: NO")
    ) +
    labs(
      title = title,
      x = "Respiratory history (human)",
      y = "Proportion",
      fill = "SIV"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.position = "top"
    )
  ggsave(filename, gg, width = 8, height = 6)
}




# =====================================================================
# ======================= CREATE AND SAVE PLOTS =======================
# =====================================================================

# ------------------ PIE CHARTS FOR TRUE/FALSE ------------------------
save_pie_chart(
  df3, "SIV_positive",
  file.path(output_folder, "pie_SIV_positive.png"),
  "SIV positive herds"
)
save_pie_chart(
  df3, "symptomatic_report",
  file.path(output_folder, "pie_symptomatic_report.png"),
  "Reported clinical signs"
)
save_pie_chart(
  df3, "ili_signs",
  file.path(output_folder, "pie_ili_signs.png"),
  "Occurence of clinical signs"
)

# ------------------ BAR CHART FOR no_positive_pigs -------------------
save_bar_chart(
  df3, "no_positive_pigs",
  file.path(output_folder, "bar_no_positive_pigs.png"),
  "Number of positive pigs in positive herds"
)

# ---- WHISKER PLOT FOR average_cp STRATIFIED BY symptom_report ---------
save_whisker_plot(
  df3, "average_cp", "symptomatic_report",
  file.path(output_folder, "boxplot_average_cp_by_symptomatic_report.svg"),
  "Average Cp stratified by clinical signs occurence"
)


# ---- BAR CHART: SIV_positive % by ili_signs --------------------

save_SIV_positive_bar_by_ili(
  df3,
  SIV_var = "SIV_positive",
  group_var = "ili_signs",
  filename = file.path(output_folder, "bar_SIV_positive_by_ili_signs.png"),
  title = "Percentage of SIV positive herds by clinical signs occurence"
)

# ---- PIE CHART: ili_signs TRUE/FALSE among SIV_positive ---------------

save_ili_signs_pie_among_SIVpos(
  df3,
  SIV_var = "SIV_positive",
  ili_var = "ili_signs",
  filename = file.path(output_folder, "pie_ili_signs_among_SIV_positive.png"),
  title = "Clinical signs occurence among SIV positive herds"
)

# ---- BOX PLOT: Weaners number by SIV Status ------------


save_number_weaners_boxplot_by_SIVpos(
  df3,
  weaners_var = "number_weaners",
  SIV_var = "SIV_positive",
  filename = file.path(output_folder, "boxplot_number_weaners_by_SIV_positive.png"),
  title = "Number of weaners by SIV Status"
)

# ---- PROPORTION BAR PLOT: contact_bird_in_stable by SIV_positive Group -------------------

save_contact_bird_in_stable_propplot_by_SIVpos(
  df3,
  contact_var = "contact_bird_in_stable",
  SIV_var = "SIV_positive",
  filename = file.path(output_folder, "propplot_contact_bird_in_stable_by_SIV_positive.png"),
  title = "SIV status by contact with birds in stable"
)

# ------------------ PROPORTION BAR PLOT: respiratory_history_human by SIV_positive -------------------
save_SIVpositive_propplot_by_resp_history_human(
  df2,
  SIV_var = "SIV_positive",
  resp_var = "respiratory_history_human",
  filename = file.path(output_folder, "propplot_SIV_positive_by_respiratory_history_human.png"),
  title = "Pig SIV status by farmer`s respiratory history"
)



# ------------------ BOX PLOT: percentage_postive_pigs by symptom_report with mean line -------------------
library(ggplot2)
library(ggbeeswarm)
library(dplyr)

# Filter for strictly positive percentages
df_plot <- df3 %>%
  filter(!is.na(percentage_positive_pigs), percentage_positive_pigs > 0) %>%
  mutate(symptomatic_report = factor(symptomatic_report, levels = c(FALSE, TRUE)))

cols <- c("FALSE" = "red", "TRUE" = "green")

p <- ggplot(df_plot, aes(x = symptomatic_report, y = percentage_positive_pigs, fill = symptomatic_report)) +
  # Mean segment (black)
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y..),
    width = 0.4,
    color = "black",
    size = 2,
    show.legend = FALSE
  ) +
  # Mean label (just the mean value, smaller font)
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = sprintf("%.1f", ..y..)),
    vjust = -0.6, # slightly above the mean line
    color = "black",
    size = 6,
    fontface = "plain",
    show.legend = FALSE
  ) +
  geom_boxplot(alpha = 0.7, outlier.color = NA) +
  geom_beeswarm(color = "black", size = 2, cex = 1.2) +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
  labs(
    title = "SIV positive pigs by symptomatic report (>0%)",
    x = "symptomatic report",
    y = "Percentage of positive pigs"
  ) +
  coord_cartesian(ylim = c(0, 100), clip = "on") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(20, 20, 40, 20)
  )

ggsave("SIV_positive_pigs_symptomatic_report.svg", plot = p, width = 7, height = 7, dpi = 300)

# =====================================================================
# END OF SCRIPT
# =====================================================================
# - All plots saved as PNGs in the output folder.
# - Functions are robust to missing/non-finite values.
# - Tidy evaluation ensures compatibility with latest ggplot2.
# - Color palettes chosen for clarity and accessibility.
# - Modular functions enable easy extension for more plots or variables.
# - All outputs are ready for publication or reporting.

