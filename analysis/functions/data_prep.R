# data_prep.R - Literate Programming Version
# =========================================================
# Data Preparation and Loading Functions
# 
# This file contains functions that load and prepare survey data
# Think of these as the "kitchen prep" before cooking the main analysis
#
# For beginners: Data preparation is crucial! 
# Raw data is messy - we need to clean and organize it first
# =========================================================

# Function 1: Load survey data from CSV file
# ===========================================
#' Load and prepare survey data
#' 
#' CSV (Comma-Separated Values) files are like Excel spreadsheets
#' stored as plain text. Each row is a survey response, each column
#' is a variable (question answer, demographic info, etc.)
#' 
#' @param filepath Path to the CSV file on your computer
#' @param filter_groups Keep only specific groups (NULL = keep all)
#' 
#' @return Prepared data frame ready for analysis
#' 
#' @examples
#' # Load all data
#' data <- load_survey_data("survey.csv")
#' # Load only groups A and C
#' data <- load_survey_data("survey.csv", filter_groups = c("A", "C"))
load_survey_data <- function(filepath, 
                            filter_groups = NULL) {  # Which groups to keep
  
  # Step 1: Load the CSV file into R
  # header = TRUE means first row contains column names
  # sep = "," means columns are separated by commas
  # dec = "." means decimal point is a period (not comma like in some countries)
  data <- read.csv(filepath, header = TRUE, sep = ",", dec = ".")
  
  # Step 2: Filter to specific groups if requested
  if (!is.null(filter_groups)) {
    # Keep only rows where GROUP column is in our filter list
    data <- dplyr::filter(data, GROUP %in% filter_groups)
    
    # Tell user what we did
    cat("Filtered data to groups:", paste(filter_groups, collapse = ", "), "\n")
    cat("Remaining responses:", nrow(data), "\n")
  }
  
  return(data)
}

# Function 2: Create delta (difference) variables
# ================================================
#' Create delta variables showing the gap between Q2 and Q3
#' 
#' Delta = Q2 - Q3 for each scenario
#' This shows if people think the law (Q2) is stricter or more lenient
#' than what they prefer (Q3).
#' 
#' Interpretation:
#' - Positive delta: Law is stricter than people want
#' - Zero delta: Law matches preferences
#' - Negative delta: Law is more lenient than people want
#' 
#' @param data Data frame with survey responses
#' @param var1 First variable (Q2 - what the law says)
#' @param var2 Second variable (Q3 - what people prefer)
#' @param new_var_name Name for the new delta column
#' 
#' @return Data frame with new delta column added
create_delta_variable <- function(data, 
                                 var1,  # Q2 variable (e.g., "PD2")
                                 var2,  # Q3 variable (e.g., "PD3")
                                 new_var_name) {  # New column name (e.g., "PD25")
  
  # Calculate difference and store in new column
  # data[[var]] is how we access columns by name in R
  data[[new_var_name]] <- data[[var1]] - data[[var2]]
  
  # Tell user what we created
  cat("Created delta variable:", new_var_name, 
      "=", var1, "-", var2, "\n")
  
  return(data)
}

# Function 3: Create all delta variables automatically
# ====================================================
#' Create delta variables for all scenarios at once
#' 
#' Instead of creating each delta manually, this function
#' creates all of them in one go. Saves time and reduces errors!
#' 
#' @param data Data frame
#' @param scenarios Vector of scenario codes (PD, PU, EU, etc.)
#' 
#' @return Data frame with all delta columns added
create_all_deltas <- function(data, 
                             scenarios = c("PD", "PU", "EU", "CA", "LS", "WN", "FS")) {
  
  cat("Creating delta variables for all scenarios...\n")
  
  # Loop through each scenario
  for (scenario in scenarios) {
    # Build variable names
    var1 <- paste0(scenario, "2")  # e.g., "PD2"
    var2 <- paste0(scenario, "3")  # e.g., "PD3"
    new_var <- paste0(scenario, "25")  # e.g., "PD25"
    
    # Check if both Q2 and Q3 exist for this scenario
    if (var1 %in% names(data) && var2 %in% names(data)) {
      # Create the delta variable
      data <- create_delta_variable(data, var1, var2, new_var)
    } else {
      # Warn if variables are missing
      cat("  Warning: Skipping", scenario, "- columns not found\n")
    }
  }
  
  cat("Delta variable creation complete!\n")
  return(data)
}

# Function 4: Special preparation for Educational Use data
# ========================================================
#' Prepare data for Educational Use scenario (special case)
#' 
#' The Educational Use scenario is special:
#' - It uses "B1" instead of "B" as a treatment group
#' - B1 is a variant of the B treatment specific to education
#' - We need to remove any "B" responses and keep only A, B1, and C
#' 
#' @param filepath Path to the B1 CSV file
#' 
#' @return Prepared data frame without B group
prepare_eu_data <- function(filepath) {
  
  cat("Loading Educational Use data with B1 group...\n")
  
  # Step 1: Load the data
  data <- load_survey_data(filepath)
  
  # Step 2: Remove regular B group (keep only A, B1, C)
  # The != operator means "not equal to"
  data <- dplyr::filter(data, GROUP != "B")
  
  cat("  Removed B group, keeping A, B1, and C\n")
  cat("  Remaining responses:", nrow(data), "\n")
  
  # Step 3: Create EU delta variable (EU2 - EU3)
  if ("EU2" %in% names(data) && "EU3" %in% names(data)) {
    data <- create_delta_variable(data, "EU2", "EU3", "EU25")
  }
  
  return(data)
}

# Function 5: Check data structure
# =================================
#' Validate that required columns exist in the data
#' 
#' Before running analysis, we need to make sure all required
#' columns are present. This function checks and reports any
#' missing columns.
#' 
#' @param data Data frame to check
#' @param required_cols Vector of column names that must exist
#' 
#' @return TRUE if all columns present, FALSE otherwise
validate_data_structure <- function(data, 
                                   required_cols) {
  
  # Find which required columns are missing
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    # Report missing columns
    cat("ERROR: Missing required columns:\n")
    cat("  ", paste(missing_cols, collapse = ", "), "\n")
    return(FALSE)
  } else {
    cat("✓ All required columns present\n")
    return(TRUE)
  }
}

# Function 6: Get column names for a scenario
# ============================================
#' Get all question columns for a specific scenario
#' 
#' Each scenario has multiple questions (1, 2, 3, 4, and delta).
#' This function returns all column names for a scenario.
#' 
#' @param scenario_code Two-letter code (PD, PU, etc.)
#' @param include_delta Include the delta column? (default TRUE)
#' 
#' @return Vector of column names
get_scenario_columns <- function(scenario_code, 
                                include_delta = TRUE) {
  
  # Basic questions: 1, 2, 3, 4
  cols <- paste0(scenario_code, c("1", "2", "3", "4"))
  
  # Add delta column if requested
  if (include_delta) {
    cols <- c(cols, paste0(scenario_code, "25"))
  }
  
  return(cols)
}

# Function 7: Check data quality
# ===============================
#' Check for missing values and report data quality
#' 
#' Missing values (NA in R) occur when:
#' - Respondents skip questions
#' - Data entry errors
#' - Technical problems during survey
#' 
#' This function creates a report showing how much data is missing.
#' 
#' @param data Data frame to check
#' @param columns Specific columns to check (NULL = check all)
#' 
#' @return Data frame with missing value report
check_data_quality <- function(data, 
                              columns = NULL) {
  
  # If no columns specified, check all columns
  if (is.null(columns)) {
    columns <- names(data)
  }
  
  cat("Checking data quality...\n")
  
  # Create report for each column
  missing_report <- data.frame(
    Column = columns,
    Missing_Count = sapply(columns, function(x) sum(is.na(data[[x]]))),
    Missing_Percent = sapply(columns, function(x) round(100 * mean(is.na(data[[x]])), 2)),
    stringsAsFactors = FALSE
  )
  
  # Add interpretation
  missing_report$Quality <- ifelse(
    missing_report$Missing_Percent == 0, "Perfect",
    ifelse(missing_report$Missing_Percent < 5, "Good",
           ifelse(missing_report$Missing_Percent < 10, "Acceptable", "Poor"))
  )
  
  return(missing_report)
}

# Function 8: Master data preparation function
# ============================================
#' Prepare all data with complete preprocessing
#' 
#' This is the main function that does everything:
#' 1. Loads both data files
#' 2. Creates delta variables
#' 3. Prepares special EU dataset
#' 4. Returns everything ready for analysis
#' 
#' @param main_filepath Path to main survey data
#' @param b1_filepath Path to B1 survey data (for EU scenario)
#' @param create_deltas Should we create delta variables?
#' 
#' @return List containing both datasets, fully prepared
prepare_all_data <- function(main_filepath,
                            b1_filepath = NULL,
                            create_deltas = TRUE) {
  
  cat(strrep("=", 50), "\n")
  cat("PREPARING SURVEY DATA FOR ANALYSIS\n")
  cat(strrep("=", 50), "\n\n")
  
  # Step 1: Load main survey data
  cat("Loading main survey data...\n")
  main_data <- load_survey_data(main_filepath)
  cat("  Loaded", nrow(main_data), "responses\n")
  
  # Step 2: Create delta variables if requested
  if (create_deltas) {
    cat("\nCreating delta variables...\n")
    main_data <- create_all_deltas(main_data)
  }
  
  # Step 3: Load B1 data if provided (for Educational Use)
  b1_data <- NULL
  if (!is.null(b1_filepath)) {
    cat("\nLoading B1 data for Educational Use scenario...\n")
    b1_data <- prepare_eu_data(b1_filepath)
  }
  
  cat("\n", strrep("=", 50), "\n")
  cat("DATA PREPARATION COMPLETE!\n")
  cat(strrep("=", 50), "\n")
  
  # Return both datasets in a list
  return(list(
    main = main_data,
    b1 = b1_data
  ))
}

# Function 9: Filter data by group
# =================================
#' Extract data for a specific group
#' 
#' Sometimes we need to analyze just one group at a time.
#' This function extracts data for a single group.
#' 
#' @param data Data frame
#' @param group Group to extract (A, B, or C)
#' 
#' @return Filtered data frame with only that group
filter_by_group <- function(data, group) {
  result <- dplyr::filter(data, GROUP == group)
  cat("Filtered to group", group, ":", nrow(result), "responses\n")
  return(result)
}

# Function 10: Split data into separate groups
# ============================================
#' Split data into a list of separate group datasets
#' 
#' This creates separate datasets for each group,
#' useful when you need to analyze groups independently.
#' 
#' @param data Data frame
#' @param group_var Column containing group labels
#' 
#' @return List with separate data frame for each group
split_by_groups <- function(data, group_var = "GROUP") {
  
  # Get unique group names
  groups <- unique(data[[group_var]])
  cat("Splitting data into", length(groups), "groups:", 
      paste(groups, collapse = ", "), "\n")
  
  # Create list to store results
  result <- list()
  
  # Split data for each group
  for (grp in groups) {
    result[[grp]] <- dplyr::filter(data, !!rlang::sym(group_var) == grp)
    cat("  Group", grp, ":", nrow(result[[grp]]), "responses\n")
  }
  
  return(result)
}

# Function 11: Decode race codes
# ================================
#' Decode Qualtrics race codes to human-readable labels.
#' Moved from 04-randomization.qmd inline helpers.
#'
#' @param x Vector of race codes (character or numeric).
#' @return Character vector with decoded labels.
#'   Single codes are mapped; multi-select (e.g., "1,4") pass through.

race_labels <- c(
  "1" = "White",
  "2" = "Black / African American",
  "3" = "American Indian / Alaska Native",
  "4" = "Asian",
  "5" = "Native Hawaiian / Pacific Islander",
  "6" = "Other",
  "7" = "Prefer not to say"
)

decode_race <- function(x) {
  x <- as.character(x)
  decoded <- race_labels[x]
  ifelse(is.na(decoded), x, decoded)
}


# Function 12: Decode gender codes
# ==================================
#' Decode Qualtrics gender codes to human-readable labels.
#' Moved from 04-randomization.qmd inline helpers.
#'
#' @param x Vector of gender codes (character or numeric).
#' @return Character vector with decoded labels.

gender_labels <- c("1" = "Male", "2" = "Female", "3" = "Other / Write-in",
                   "4" = "Prefer not to say")

decode_gender <- function(x) {
  x <- as.character(x)
  decoded <- gender_labels[x]
  ifelse(is.na(decoded), x, decoded)
}


# Summary: Why data preparation matters
# ======================================
# Good data preparation is essential because:
# 1. Raw data is messy and needs cleaning
# 2. We need to create calculated variables (like deltas)
# 3. Special cases (like EU with B1) need special handling
# 4. Checking data quality helps catch problems early
# 5. Well-prepared data makes analysis much easier
#
# Think of it like cooking: You can't make a good meal with
# unprepared ingredients. These functions are our "mise en place"!
