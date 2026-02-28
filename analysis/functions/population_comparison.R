# population_comparison.R — Literate Programming Version
# ============================================================
# Population Representativeness Analysis
#
# This module compares the survey sample demographics against
# American Community Survey (ACS) benchmarks. The goal is to
# assess external validity: how well does our convenience sample
# resemble the American adult population?
#
# For beginners: External validity asks "do our results generalize
# beyond the people we actually surveyed?" Comparing our sample to
# the Census helps answer this question.
#
# DEPENDENCY: tidycensus package (for fetch_acs_benchmarks only)
# All other functions work from the committed acs-benchmarks.csv.
# ============================================================

# Function 1: Fetch ACS benchmarks from the Census API
# ======================================================
#' Fetch ACS 1-year estimates for population representativeness benchmarks
#'
#' Pulls national ACS 2023 1-year estimates for five demographic variables
#' (AGE, GENDER, RACE_ETH, EDU, INCOME), recodes them into survey-aligned
#' categories, and writes a tidy CSV to disk.
#'
#' Run this once to generate the committed acs-benchmarks.csv.
#' Most readers will use load_population_benchmarks() instead.
#'
#' @param year ACS vintage year. Default 2023 (closest to March 2024 survey).
#' @param save_path File path for the output CSV.
#'   Default: data/population/acs-benchmarks.csv (via here::here()).
#'
#' @return Data frame of ACS proportions (invisibly). Writes CSV as side effect.
#'
#' @section Census API Key:
#'   Requires CENSUS_API_KEY set in ~/.Renviron.
#'   Register free at: https://api.census.gov/data/key_signup.html
#'   Set key: usethis::edit_r_environ() then add CENSUS_API_KEY=your_key_here
#'
#' @examples
#' # Run once; commits CSV to repo for offline reproducibility
#' fetch_acs_benchmarks()
fetch_acs_benchmarks <- function(
    year      = 2023,
    save_path = here::here("data", "population", "acs-benchmarks.csv")
) {

  # Step 1: Check for tidycensus and Census API key
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop(
      "tidycensus is required to fetch ACS data.\n",
      "Install it: renv::install('tidycensus'); renv::snapshot()"
    )
  }

  api_key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(api_key) == 0) {
    stop(
      "CENSUS_API_KEY not found in environment.\n",
      "Register at https://api.census.gov/data/key_signup.html\n",
      "Then add to ~/.Renviron: CENSUS_API_KEY=your_key_here\n",
      "Reload with: readRenviron('~/.Renviron')"
    )
  }

  tidycensus::census_api_key(api_key, install = FALSE, overwrite = TRUE)

  message("Fetching ACS ", year, " 1-year estimates from Census API...")

  # -----------------------------------------------------------------------
  # AGE and GENDER from B01001 (Sex by Age)
  # We pull detailed cells and aggregate into our bins.
  # -----------------------------------------------------------------------
  b01001 <- tidycensus::get_acs(
    geography = "us",
    table     = "B01001",
    year      = year,
    survey    = "acs1",
    cache_table = TRUE
  )

  # Total population (denominator for age/gender)
  total_pop <- b01001$estimate[b01001$variable == "B01001_001"]

  # Male age cells: B01001_003 to B01001_025
  # Female age cells: B01001_027 to B01001_049
  # Cell variable numbers correspond to specific age bins
  # (see acs-source-notes.md for full cell listing)

  # Helper: sum named ACS variables
  sum_acs <- function(vars, data) {
    sum(data$estimate[data$variable %in% vars], na.rm = TRUE)
  }

  # AGE 18-29 (male: 003-007, female: 027-031)
  age_1829_m <- paste0("B01001_0", sprintf("%02d", 3:7))   # 18-19,20,21,22-24,25-29
  age_1829_f <- paste0("B01001_0", sprintf("%02d", 27:31))
  age_1829 <- sum_acs(c(age_1829_m, age_1829_f), b01001)

  # AGE 30-44 (male: 008-010, female: 032-034)
  age_3044_m <- paste0("B01001_0", sprintf("%02d", 8:10))
  age_3044_f <- paste0("B01001_0", sprintf("%02d", 32:34))
  age_3044 <- sum_acs(c(age_3044_m, age_3044_f), b01001)

  # AGE 45-59 (male: 011-013, female: 035-037)
  age_4559_m <- paste0("B01001_0", sprintf("%02d", 11:13))
  age_4559_f <- paste0("B01001_0", sprintf("%02d", 35:37))
  age_4559 <- sum_acs(c(age_4559_m, age_4559_f), b01001)

  # AGE 60+ (male: 014-025, female: 038-049)
  age_60p_m <- paste0("B01001_0", sprintf("%02d", 14:25))
  age_60p_f <- paste0("B01001_0", sprintf("%02d", 38:49))
  age_60p <- sum_acs(c(age_60p_m, age_60p_f), b01001)

  # Denominator: sum of all 18+ age cells
  age_total <- age_1829 + age_3044 + age_4559 + age_60p

  age_df <- data.frame(
    variable        = "AGE",
    category_code   = 1:4,
    category_label  = c("18-29", "30-44", "45-59", "60+"),
    acs_count       = c(age_1829, age_3044, age_4559, age_60p),
    acs_proportion  = c(age_1829, age_3044, age_4559, age_60p) / age_total,
    stringsAsFactors = FALSE
  )

  # GENDER (male 18+ / female 18+ from the same B01001 cells)
  male_total   <- sum_acs(c(age_1829_m, age_3044_m, age_4559_m, age_60p_m), b01001)
  female_total <- sum_acs(c(age_1829_f, age_3044_f, age_4559_f, age_60p_f), b01001)
  gender_total <- male_total + female_total

  gender_df <- data.frame(
    variable        = "GENDER",
    category_code   = 1:2,
    category_label  = c("Male", "Female"),
    acs_count       = c(male_total, female_total),
    acs_proportion  = c(male_total, female_total) / gender_total,
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # RACE_ETH from B03002 (Hispanic/Latino Origin by Race)
  # Using B03002 avoids double-counting: Hispanics are counted once
  # under the Hispanic/Latino category regardless of race.
  # -----------------------------------------------------------------------
  b03002 <- tidycensus::get_acs(
    geography = "us",
    table     = "B03002",
    year      = year,
    survey    = "acs1",
    cache_table = TRUE
  )

  total_race   <- b03002$estimate[b03002$variable == "B03002_001"]
  hispanic     <- b03002$estimate[b03002$variable == "B03002_012"]
  nh_white     <- b03002$estimate[b03002$variable == "B03002_003"]
  nh_black     <- b03002$estimate[b03002$variable == "B03002_004"]
  nh_asian     <- b03002$estimate[b03002$variable == "B03002_006"]
  # "Other" = total minus the four named categories
  other_race   <- total_race - hispanic - nh_white - nh_black - nh_asian

  race_counts <- c(nh_white, nh_black, hispanic, nh_asian, other_race)
  race_df <- data.frame(
    variable        = "RACE_ETH",
    category_code   = 1:5,
    category_label  = c("Non-Hispanic White", "Non-Hispanic Black",
                        "Hispanic/Latino", "Asian", "Other"),
    acs_count       = race_counts,
    acs_proportion  = race_counts / total_race,
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # EDU from B15003 (Educational Attainment, population 25+)
  # -----------------------------------------------------------------------
  b15003 <- tidycensus::get_acs(
    geography = "us",
    table     = "B15003",
    year      = year,
    survey    = "acs1",
    cache_table = TRUE
  )

  total_edu <- b15003$estimate[b15003$variable == "B15003_001"]

  # HS or less: no schooling (002) through regular HS diploma (017);
  # GED equivalent (018) included in HS-or-less
  hs_or_less  <- sum_acs(paste0("B15003_0", sprintf("%02d", 2:18)), b15003)
  # Some college: some college no degree (019), <1yr college (020),
  # 1+yr college no degree (021), Associate's (021)
  # Note: B15003 variables 019-021 = some college/no degree, Associate's = 021
  some_college <- sum_acs(paste0("B15003_0", c("19", "20", "21")), b15003)
  # Bachelor's (022)
  bachelors    <- b15003$estimate[b15003$variable == "B15003_022"]
  # Graduate: Master's (023), professional (024), doctorate (025)
  graduate     <- sum_acs(paste0("B15003_0", c("23", "24", "25")), b15003)

  edu_counts <- c(hs_or_less, some_college, bachelors, graduate)
  edu_df <- data.frame(
    variable        = "EDU",
    category_code   = 1:4,
    category_label  = c("HS or less", "Some college", "Bachelor's", "Graduate"),
    acs_count       = edu_counts,
    acs_proportion  = edu_counts / total_edu,
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # INCOME from B19001 (Household Income in Past 12 Months)
  # Recoded to match survey INCOME categories 1-6
  # -----------------------------------------------------------------------
  b19001 <- tidycensus::get_acs(
    geography = "us",
    table     = "B19001",
    year      = year,
    survey    = "acs1",
    cache_table = TRUE
  )

  total_hh <- b19001$estimate[b19001$variable == "B19001_001"]

  # B19001 cells: 002=<$10k 003=$10-14.9k 004=$15-19.9k 005=$20-24.9k
  #               006=$25-29.9k 007=$30-34.9k 008=$35-39.9k 009=$40-44.9k 010=$45-49.9k
  #               011=$50-59.9k 012=$60-74.9k  013=$75-99.9k
  #               014=$100-124.9k 015=$125-149.9k  016=$150-199.9k 017=$200k+
  inc_1 <- sum_acs(paste0("B19001_00", 2:5), b19001)                          # < $25k
  inc_2 <- sum_acs(paste0("B19001_00", 6:9), b19001)                          # $25-$49.9k (partial)
  inc_2 <- inc_2 + sum_acs("B19001_010", b19001)                              # add $45-49.9k
  inc_3 <- sum_acs(paste0("B19001_0", c("11", "12")), b19001)                 # $50k-$74.9k
  inc_4 <- sum_acs("B19001_013", b19001)                                      # $75k-$99.9k
  inc_5 <- sum_acs(paste0("B19001_0", c("14", "15")), b19001)                 # $100k-$149.9k
  inc_6 <- sum_acs(paste0("B19001_0", c("16", "17")), b19001)                 # $150k+

  income_counts <- c(inc_1, inc_2, inc_3, inc_4, inc_5, inc_6)
  income_df <- data.frame(
    variable        = "INCOME",
    category_code   = 1:6,
    category_label  = c("< $25k", "$25k–$49.9k", "$50k–$74.9k",
                        "$75k–$99.9k", "$100k–$149.9k", "$150k+"),
    acs_count       = income_counts,
    acs_proportion  = income_counts / total_hh,
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # Combine all variables and write to CSV
  # -----------------------------------------------------------------------
  benchmarks <- rbind(age_df, gender_df, race_df, edu_df, income_df)

  # Validate: proportions should sum to ~1.0 within each variable
  prop_check <- tapply(benchmarks$acs_proportion, benchmarks$variable, sum)
  for (v in names(prop_check)) {
    if (abs(prop_check[v] - 1.0) > 0.01) {
      warning(sprintf(
        "Proportions for %s sum to %.4f (expected ~1.0). Check recoding.",
        v, prop_check[v]
      ))
    }
  }

  # Write CSV
  utils::write.csv(benchmarks, file = save_path, row.names = FALSE)
  message(sprintf("Wrote %d rows to %s", nrow(benchmarks), save_path))

  invisible(benchmarks)
}


# Function 2: Load committed ACS benchmarks
# ===========================================
#' Load ACS benchmark proportions from committed CSV
#'
#' Reads the pre-generated acs-benchmarks.csv and returns a named list
#' with one data frame per variable (AGE, GENDER, RACE_ETH, EDU, INCOME).
#'
#' @param path Path to acs-benchmarks.csv. Default uses here::here().
#'
#' @return Named list of data frames, one per variable.
#'   Each data frame has columns: variable, category_code, category_label,
#'   acs_count, acs_proportion.
#'
#' @examples
#' benchmarks <- load_population_benchmarks()
#' benchmarks$AGE
load_population_benchmarks <- function(
    path = here::here("data", "population", "acs-benchmarks.csv")
) {

  if (!file.exists(path)) {
    stop(
      "acs-benchmarks.csv not found at: ", path, "\n",
      "Run fetch_acs_benchmarks() to generate it ",
      "(requires Census API key; see acs-source-notes.md)."
    )
  }

  benchmarks <- utils::read.csv(path, stringsAsFactors = FALSE)

  # Return as named list split by variable
  split(benchmarks, benchmarks$variable)
}


# Function 3: Recode survey variables to ACS-aligned categories
# ==============================================================
#' Recode a survey variable to match ACS category structure
#'
#' Maps survey response codes to the ACS category labels used in
#' acs-benchmarks.csv. Necessary because survey and ACS use different
#' coding schemes for the same underlying concepts.
#'
#' @param data A data frame containing the survey variable.
#' @param variable Character. One of: "AGE", "GENDER", "RACE_ETH", "EDU", "INCOME".
#'
#' @return The input data frame with a new column named
#'   `{variable}_ACS` containing ACS-aligned category labels.
#'
#' @details
#'   Recoding maps:
#'   - AGE: numeric age → "18-29", "30-44", "45-59", "60+"
#'   - GENDER: 1=Male, 2=Female (survey "Other"/"Prefer not" coded NA for ACS comparison)
#'   - RACE_ETH: combines RACE and HISPLAT columns
#'   - EDU: 1-4 scale → ACS education categories
#'   - INCOME: 1-6 scale → ACS income brackets
recode_for_acs <- function(data, variable) {

  out <- data  # Copy to avoid modifying original

  if (variable == "AGE") {
    # Survey AGE is a numeric response in years (or age-range bins).
    # If numeric, bin directly. The survey AGE variable stores actual age.
    out$AGE_ACS <- cut(
      out$AGE,
      breaks = c(17, 29, 44, 59, Inf),
      labels = c("18-29", "30-44", "45-59", "60+"),
      right  = TRUE,
      include.lowest = FALSE
    )

  } else if (variable == "GENDER") {
    # Survey: 1=Male, 2=Female, 3+=Other/Prefer not to say
    # ACS comparison uses only Male/Female; "Other" coded NA
    out$GENDER_ACS <- dplyr::case_when(
      out$GENDER == 1 ~ "Male",
      out$GENDER == 2 ~ "Female",
      TRUE            ~ NA_character_   # Exclude "Other" from ACS comparison
    )

  } else if (variable == "HISPLAT") {
    # Hispanic/Latino origin: 1 = Hispanic, 2 = Not Hispanic (survey coding)
    # Coded NA for any missing values.
    out$HISPLAT_ACS <- dplyr::case_when(
      out$HISPLAT == 1 ~ "Hispanic/Latino",
      out$HISPLAT == 2 ~ "Not Hispanic/Latino",
      TRUE             ~ NA_character_
    )

  } else if (variable == "RACE_ONLY") {
    # Race regardless of Hispanic/Latino origin (maps to B02001 race-alone)
    # Survey RACE codes (standard Qualtrics/Census):
    #   "1"=White, "2"=Black, "3"=AIAN, "4"=Asian, "5"=NHPI, "6"=Other, "7"=Two+
    # Multi-select responses (e.g., "1,2") → Other/Multiracial
    out$RACE <- as.character(out$RACE)
    out$RACE_ONLY_ACS <- dplyr::case_when(
      out$RACE == "1"  ~ "White alone",
      out$RACE == "2"  ~ "Black or African American",
      out$RACE == "4"  ~ "Asian alone",
      !is.na(out$RACE) ~ "Other / Multiracial",
      TRUE             ~ NA_character_
    )

  } else if (variable == "RACE_ETH") {
    # Survey has separate RACE and HISPLAT (Hispanic/Latino) columns.
    # ACS uses B03002 which combines both. Priority: Hispanic/Latino first,
    # then racial category for non-Hispanics.
    #
    # HISPLAT coding in this survey: 1 = Hispanic/Latino, 2 = Not Hispanic/Latino
    # (confirmed from data: HISPLAT distributes as ~8% 1s and ~92% 2s)
    #
    # RACE is a multi-select character column (values like "1", "1,2", "2,6").
    # Standard Qualtrics/Census race codes (confirmed from data + preregistration):
    #   "1"=White, "2"=Black or African American, "3"=American Indian/Alaska Native,
    #   "4"=Asian, "5"=Native Hawaiian/Pacific Islander, "6"=Other race, "7"=Two or more
    # Multi-select responses (e.g., "1,2") → classified as "Other" (multiracial),
    # consistent with ACS "Two or more races" treatment.
    #
    # Classification priority: Hispanic/Latino first (regardless of RACE value),
    # then single-race NH categories, then "Other" for all remaining.
    out$RACE <- as.character(out$RACE)  # ensure character for string matching
    out$RACE_ETH_ACS <- dplyr::case_when(
      out$HISPLAT == 1                          ~ "Hispanic/Latino",
      out$HISPLAT == 2 & out$RACE == "1"        ~ "Non-Hispanic White",
      out$HISPLAT == 2 & out$RACE == "2"        ~ "Non-Hispanic Black",
      out$HISPLAT == 2 & out$RACE == "4"        ~ "Asian",
      !is.na(out$HISPLAT) & !is.na(out$RACE)   ~ "Other",
      TRUE                                       ~ NA_character_
    )

  } else if (variable == "EDU") {
    # Survey EDU: 1=HS or less, 2=Some college, 3=Bachelor's, 4=Graduate
    # These align directly with ACS categories
    out$EDU_ACS <- dplyr::case_when(
      out$EDU == 1 ~ "HS or less",
      out$EDU == 2 ~ "Some college",
      out$EDU == 3 ~ "Bachelor's",
      out$EDU == 4 ~ "Graduate",
      TRUE         ~ NA_character_
    )

  } else if (variable == "INCOME") {
    # Survey INCOME: 1-6 scale matching ACS brackets; 7 = Prefer not to say
    # Category 7 (n=19) is coded NA — excluded from ACS comparison.
    # (see acs-source-notes.md for bracket definitions)
    out$INCOME_ACS <- dplyr::case_when(
      out$INCOME == 1 ~ "< $25k",
      out$INCOME == 2 ~ "$25k\u2013$49.9k",
      out$INCOME == 3 ~ "$50k\u2013$74.9k",
      out$INCOME == 4 ~ "$75k\u2013$99.9k",
      out$INCOME == 5 ~ "$100k\u2013$149.9k",
      out$INCOME == 6 ~ "$150k+",
      out$INCOME == 7 ~ NA_character_,   # Prefer not to say — excluded
      TRUE            ~ NA_character_
    )

  } else if (variable == "POLPARTY") {
    # Survey POLPARTY: 1=Republican, 2=Democrat, 3=Independent,
    #                  4=Other, 5=Prefer not to say
    # Benchmark: Gallup 2024 annual averages (three-way split: R/D/I).
    # "Other" (4) and "Prefer not to say" (5) coded NA — excluded.
    out$POLPARTY_ACS <- dplyr::case_when(
      out$POLPARTY == 1 ~ "Republican",
      out$POLPARTY == 2 ~ "Democrat",
      out$POLPARTY == 3 ~ "Independent",
      out$POLPARTY %in% c(4, 5) ~ NA_character_,
      TRUE ~ NA_character_
    )

  } else if (variable == "LIBCON") {
    # Survey LIBCON: 1=Very Liberal, 2=Somewhat Liberal, 3=Moderate,
    #               4=Somewhat Conservative, 5=Very Conservative
    # Collapsed to three categories to match Gallup 2024 published breakdown.
    out$LIBCON_ACS <- dplyr::case_when(
      out$LIBCON %in% c(1, 2) ~ "Liberal",
      out$LIBCON == 3          ~ "Moderate",
      out$LIBCON %in% c(4, 5) ~ "Conservative",
      TRUE                     ~ NA_character_
    )

  } else {
    stop("Unknown variable: '", variable, "'. ",
         "Must be one of: AGE, GENDER, HISPLAT, RACE_ONLY, RACE_ETH, ",
         "EDU, INCOME, POLPARTY, LIBCON")
  }

  return(out)
}


# Function 4: Chi-square goodness-of-fit test
# ============================================
#' Chi-square goodness-of-fit: survey proportions vs. ACS benchmarks
#'
#' Tests whether the observed survey sample distribution differs from
#' the ACS reference population. Also computes Cohen's h as an effect size
#' for proportion differences.
#'
#' @param survey_data Data frame with the survey sample (Groups A, B, C only).
#' @param benchmarks Named list from load_population_benchmarks().
#' @param variable Character. One of: "AGE", "GENDER", "RACE_ETH", "EDU", "INCOME".
#'
#' @return Named list with:
#'   - `variable`: variable name
#'   - `n_survey`: number of valid survey responses
#'   - `chisq_stat`: chi-square statistic
#'   - `df`: degrees of freedom
#'   - `p_value`: p-value
#'   - `cohens_h`: Cohen's h (overall effect size, mean of pairwise |h| values)
#'   - `chisq_result`: full chisq.test output object
#'   - `comparison_table`: data frame with survey vs. ACS proportions per category
#'
#' @details
#'   Cohen's h = 2 * arcsin(sqrt(p)) is the standard effect size for
#'   comparing two proportions. Here, mean absolute h across categories
#'   is used as a summary of overall discrepancy.
run_population_chisq <- function(survey_data, benchmarks, variable) {

  # Step 1: Recode survey variable to ACS-aligned categories
  data_coded <- recode_for_acs(survey_data, variable)
  acs_col    <- paste0(variable, "_ACS")

  # Step 2: Count observed survey responses (exclude NAs)
  obs_table <- table(data_coded[[acs_col]])
  obs_table <- obs_table[obs_table > 0]   # Remove empty cells

  # Step 3: Get ACS expected proportions for matching categories
  bench_var <- benchmarks[[variable]]

  # Align ACS categories to those observed in survey
  bench_aligned <- bench_var[bench_var$category_label %in% names(obs_table), ]
  bench_aligned <- bench_aligned[match(names(obs_table),
                                       bench_aligned$category_label), ]

  # ACS proportions for matching categories (renormalize to sum to 1)
  expected_props <- bench_aligned$acs_proportion / sum(bench_aligned$acs_proportion)
  expected_n     <- expected_props * sum(obs_table)

  # Step 4: Run chi-square goodness-of-fit
  chisq_result <- tryCatch(
    chisq.test(x = as.vector(obs_table), p = expected_props),
    error = function(e) {
      warning("Chi-square test failed for ", variable, ": ", e$message)
      NULL
    }
  )

  if (is.null(chisq_result)) {
    return(list(variable = variable, n_survey = sum(obs_table),
                chisq_stat = NA, df = NA, p_value = NA, cohens_h = NA,
                chisq_result = NULL, comparison_table = NULL))
  }

  # Step 5: Cohen's h for each category pair
  survey_props <- as.vector(obs_table) / sum(obs_table)
  h_values     <- abs(2 * asin(sqrt(survey_props)) - 2 * asin(sqrt(expected_props)))
  cohens_h     <- mean(h_values, na.rm = TRUE)

  # Step 6: Per-category z-tests (survey proportion vs. ACS proportion)
  n_total <- sum(obs_table)
  z_stats <- (survey_props - expected_props) /
    sqrt(expected_props * (1 - expected_props) / n_total)
  p_cats  <- 2 * pnorm(abs(z_stats), lower.tail = FALSE)
  sig_stars <- dplyr::case_when(
    p_cats < 0.001 ~ "***",
    p_cats < 0.01  ~ "**",
    p_cats < 0.05  ~ "*",
    TRUE           ~ ""
  )

  # Step 7: Comparison table for plotting and display
  comparison_table <- data.frame(
    variable       = variable,
    category_label = names(obs_table),
    survey_n       = as.vector(obs_table),
    survey_prop    = survey_props,
    acs_prop       = expected_props,
    diff           = survey_props - expected_props,
    sig            = sig_stars,
    stringsAsFactors = FALSE
  )

  list(
    variable        = variable,
    n_survey        = sum(obs_table),
    chisq_stat      = chisq_result$statistic,
    df              = chisq_result$parameter,
    p_value         = chisq_result$p.value,
    cohens_h        = cohens_h,
    chisq_result    = chisq_result,
    comparison_table = comparison_table
  )
}


# Function 5: Side-by-side bar chart — survey vs. ACS
# =====================================================
#' Plot survey vs. ACS proportions for a single demographic variable
#'
#' Creates a horizontal side-by-side bar chart comparing the survey
#' sample distribution against ACS benchmarks. Uses the project's
#' standard theme from get_standard_theme().
#'
#' @param survey_data Data frame with the survey sample.
#' @param benchmarks Named list from load_population_benchmarks().
#' @param variable Character. One of: "AGE", "GENDER", "HISPLAT", "RACE_ONLY",
#'   "RACE_ETH", "EDU", "INCOME", "POLPARTY", "LIBCON".
#' @param variable_label Character. Human-readable label for plot title and axes.
#' @param benchmark_source Character. Label for benchmark source. Default "ACS 2023".
#'   Use "Gallup 2024" for political orientation variables.
#'
#' @return A ggplot2 object. Use print() to render, or pass to export_plot_to_tiff().
#'
#' @examples
#' p <- plot_population_comparison(survey_data, benchmarks, "AGE", "Age Group")
#' p <- plot_population_comparison(survey_data, benchmarks, "POLPARTY",
#'        "Party Affiliation", benchmark_source = "Gallup 2024")
#' print(p)
plot_population_comparison <- function(survey_data, benchmarks, variable,
                                       variable_label,
                                       show_average = FALSE,
                                       omnibus_p = NULL,
                                       benchmark_source = "ACS 2023") {

  # Derive labels from benchmark source (backward-compatible default)
  ref_label <- paste0(benchmark_source, " (reference)")
  ref_source_text <- switch(benchmark_source,
    "ACS 2023" = "Source: ACS 2023 1-Year Estimates, U.S. Census Bureau",
    "Gallup 2024" = "Source: Gallup 2024 Annual Averages (U.S. Adults)",
    paste0("Source: ", benchmark_source)
  )
  title_text <- paste0(variable_label, ": Survey vs. ", benchmark_source, " Benchmark")
  subtitle_fmt <- paste0(benchmark_source, " estimates")

  # Step 1: Compute survey proportions via recode
  data_coded <- recode_for_acs(survey_data, variable)
  acs_col    <- paste0(variable, "_ACS")

  obs_table  <- table(data_coded[[acs_col]])
  obs_table  <- obs_table[obs_table > 0]
  survey_df  <- data.frame(
    category_label = names(obs_table),
    proportion     = as.vector(obs_table) / sum(obs_table),
    source         = "Survey sample",
    stringsAsFactors = FALSE
  )

  # Step 2: Get ACS proportions for matching categories
  bench_var <- benchmarks[[variable]]
  bench_var <- bench_var[bench_var$category_label %in% names(obs_table), ]
  bench_df  <- data.frame(
    category_label = bench_var$category_label,
    proportion     = bench_var$acs_proportion / sum(bench_var$acs_proportion),
    source         = ref_label,
    stringsAsFactors = FALSE
  )

  # Step 2b: Optionally add weighted-average summary row
  if (show_average) {
    survey_props_tmp <- survey_df$proportion
    acs_props_tmp    <- bench_df$proportion
    # Weighted mean: weight each category by its ACS share
    avg_survey <- sum(acs_props_tmp * survey_props_tmp)
    avg_acs    <- sum(acs_props_tmp * acs_props_tmp)
    survey_df <- rbind(survey_df, data.frame(
      category_label = "Average", proportion = avg_survey,
      source = "Survey sample", stringsAsFactors = FALSE
    ))
    bench_df <- rbind(bench_df, data.frame(
      category_label = "Average", proportion = avg_acs,
      source = ref_label, stringsAsFactors = FALSE
    ))
  }

  # Step 3: Combine and set factor order (preserve category ordering)
  category_order <- unique(c(bench_var$category_label, names(obs_table)))
  if (show_average) category_order <- c(category_order, "Average")
  plot_data <- rbind(survey_df, bench_df)
  plot_data$category_label <- factor(
    plot_data$category_label,
    levels = rev(category_order)   # rev() so top of chart = first category
  )
  plot_data$source <- factor(
    plot_data$source,
    levels = c("Survey sample", ref_label)
  )

  # Two-color palette: survey (dark purple) vs. reference (light gray)
  source_colors <- setNames(c("#5B2C6F", "#D5D5D5"),
                            c("Survey sample", ref_label))

  # Step 4: Per-category significance (one-sample z-test of proportions)
  n_valid <- sum(obs_table)
  sig_data <- data.frame(
    category_label = names(obs_table),
    survey_prop    = as.vector(obs_table) / n_valid,
    stringsAsFactors = FALSE
  )
  acs_match <- bench_df$proportion[match(sig_data$category_label, bench_df$category_label)]
  sig_data$acs_prop <- acs_match

  # z = (p_hat - p_0) / sqrt(p_0 * (1 - p_0) / n)
  sig_data$z_stat <- (sig_data$survey_prop - sig_data$acs_prop) /
    sqrt(sig_data$acs_prop * (1 - sig_data$acs_prop) / n_valid)
  sig_data$p_val <- 2 * pnorm(abs(sig_data$z_stat), lower.tail = FALSE)
  sig_data$label <- dplyr::case_when(
    sig_data$p_val < 0.001 ~ "***",
    sig_data$p_val < 0.01  ~ "**",
    sig_data$p_val < 0.05  ~ "*",
    TRUE                   ~ "ns"
  )

  # Step 4b: Add Average row to sig_data if requested
  if (show_average) {
    avg_s <- sum(sig_data$acs_prop * sig_data$survey_prop)
    avg_a <- sum(sig_data$acs_prop * sig_data$acs_prop)
    avg_p <- if (!is.null(omnibus_p)) omnibus_p else {
      avg_se <- sqrt(sum(sig_data$acs_prop^2 *
                         sig_data$survey_prop * (1 - sig_data$survey_prop) / n_valid))
      2 * pnorm(abs((avg_s - avg_a) / avg_se), lower.tail = FALSE)
    }
    avg_label <- dplyr::case_when(
      avg_p < 0.001 ~ "***", avg_p < 0.01 ~ "**",
      avg_p < 0.05  ~ "*",   TRUE         ~ "ns"
    )
    sig_data <- rbind(sig_data, data.frame(
      category_label = "Average", survey_prop = avg_s,
      acs_prop = avg_a, z_stat = NA, p_val = avg_p, label = avg_label,
      stringsAsFactors = FALSE
    ))
  }

  # Bracket geometry — positioned past the percentage text labels
  sig_data$category_label_f <- factor(sig_data$category_label,
                                       levels = levels(plot_data$category_label))
  sig_data$y_num  <- as.numeric(sig_data$category_label_f)
  sig_data$y_top  <- sig_data$y_num + 0.225   # ACS bar (dodge top)
  sig_data$y_bot  <- sig_data$y_num - 0.225   # Survey bar (dodge bottom)

  # The percentage text ("XX.X%") is a fixed physical size (pt), but its width
  # in data coordinates scales with the axis range.  Charts with large bars
  # (race ~82%) compress the axis, so text spans more data units than on charts
  # with small bars (age ~34%).  Scale clearance proportionally.
  longer_prop <- pmax(sig_data$survey_prop, sig_data$acs_prop)
  max_prop <- max(longer_prop)
  text_clearance <- max_prop * 0.11             # scales with axis range
  sig_data$x_brack <- longer_prop + text_clearance
  # Short horizontal ticks from the bracket stem inward (just a nub, 0.008 wide)
  sig_data$x_tick_start <- sig_data$x_brack - 0.008

  # Step 5: Build plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x    = proportion,
      y    = category_label,
      fill = source
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.9),
      width    = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(proportion, accuracy = 0.1)),
      position = ggplot2::position_dodge(width = 0.9),
      hjust    = -0.1,
      size     = 5,
      color    = "black"
    ) +
    # Significance bracket — vertical stem only (no arms)
    ggplot2::geom_segment(
      data = sig_data,
      ggplot2::aes(x = x_brack, xend = x_brack, y = y_bot, yend = y_top),
      inherit.aes = FALSE, color = "grey40", linewidth = 0.4
    ) +
    # Significance label
    ggplot2::geom_text(
      data = sig_data,
      ggplot2::aes(x = x_brack + 0.005, y = y_num, label = label),
      inherit.aes = FALSE, hjust = 0, size = 5.5, fontface = 2, colour = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = source_colors,
      name   = ref_source_text,
      guide  = ggplot2::guide_legend(
        title.position = "right",
        title.theme    = ggplot2::element_text(
          size = 11, color = "grey50", face = "plain", hjust = 0
        )
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.22))
    ) +
    ggplot2::labs(
      title    = title_text,
      subtitle = paste0(
        "Survey sample (n = ", n_valid, ") vs. ", subtitle_fmt
      ),
      x        = "Proportion",
      y        = NULL,
      caption  = NULL
    ) +
    get_standard_theme(base_size = 18) +
    ggplot2::theme(
      legend.position      = "bottom",
      legend.justification = "left",
      legend.text          = ggplot2::element_text(size = 14),
      legend.margin        = ggplot2::margin(t = 2, b = 2),
      legend.background    = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key           = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background      = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background     = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.title           = ggplot2::element_text(size = 18, margin = ggplot2::margin(b = 2)),
      plot.subtitle        = ggplot2::element_text(size = 14, margin = ggplot2::margin(b = 4)),
      plot.margin          = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid.major.x   = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y   = ggplot2::element_blank()
    )

  return(p)
}


# Function 6: Summarize all comparisons in a single table
# =========================================================
#' Combine chi-square results for all variables into a summary table
#'
#' Takes the list of results from run_population_chisq() for all five
#' variables and formats them into a single kable-ready data frame.
#'
#' @param results_list Named list of results from run_population_chisq().
#'   Names should be the variable codes: "AGE", "GENDER", "RACE_ETH", "EDU", "INCOME".
#'
#' @return Data frame with columns: Variable, N, chi_sq, df, p_value,
#'   cohens_h, interpretation.
#'   Suitable for knitr::kable() with caption.
#'
#' @examples
#' results <- list(
#'   AGE    = run_population_chisq(survey_data, benchmarks, "AGE"),
#'   GENDER = run_population_chisq(survey_data, benchmarks, "GENDER"),
#'   ...
#' )
#' tbl <- summarize_population_comparisons(results)
#' knitr::kable(tbl, caption = "Population Representativeness Summary")
summarize_population_comparisons <- function(results_list) {

  # Variable display names (human-readable)
  var_labels <- c(
    AGE       = "Age",
    GENDER    = "Gender (binary)",
    HISPLAT   = "Hispanic/Latino Origin",
    RACE_ONLY = "Race (B02001)",
    RACE_ETH  = "Race/Ethnicity (Combined)",
    EDU       = "Education",
    INCOME    = "Income",
    POLPARTY  = "Party Affiliation",
    LIBCON    = "Ideology (Lib-Con)"
  )

  # Build one row per variable
  rows <- lapply(names(results_list), function(v) {
    r <- results_list[[v]]

    # Interpret Cohen's h: < 0.20 small, 0.20-0.50 medium, > 0.50 large
    interp <- dplyr::case_when(
      is.na(r$cohens_h)       ~ "—",
      r$cohens_h < 0.20       ~ "Small",
      r$cohens_h < 0.50       ~ "Medium",
      TRUE                    ~ "Large"
    )

    # Format p-value: < 0.001 if very small
    p_fmt <- dplyr::case_when(
      is.na(r$p_value)        ~ "—",
      r$p_value < 0.001       ~ "< .001",
      r$p_value < 0.05        ~ sprintf("%.3f", r$p_value),
      TRUE                    ~ sprintf("%.3f", r$p_value)
    )

    data.frame(
      Variable       = ifelse(v %in% names(var_labels), var_labels[v], v),
      N              = r$n_survey,
      "chi_sq"       = ifelse(is.na(r$chisq_stat), "—",
                              sprintf("%.2f", r$chisq_stat)),
      df             = ifelse(is.na(r$df), "—", as.character(r$df)),
      p              = p_fmt,
      "Cohens_h"     = ifelse(is.na(r$cohens_h), "—",
                              sprintf("%.3f", r$cohens_h)),
      Interpretation = interp,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}


# Function 7: Format chi-square caption for population tables
# ============================================================
#' Format a chi-square test result into a table caption string.
#' Replaces the inline sprintf() pattern repeated per-variable in 05-population.qmd.
#'
#' @param result Output of run_population_chisq() for one variable.
#' @param variable_label Human-readable label (e.g., "Age Distribution").
#'
#' @return Character string suitable for knitr::kable(caption = ...).
fmt_chisq_caption <- function(result, variable_label) {
  p_text <- ifelse(result$p_value < .001,
                   "< .001",
                   sprintf("= %.3f", result$p_value))
  sprintf(
    "%s Comparison. \u03C7\u00B2(%d) = %.2f, p %s, Cohen's h = %.3f",
    variable_label,
    result$df,
    result$chisq_stat,
    p_text,
    result$cohens_h
  )
}


# Function 8: Format a population comparison table
# ==================================================
#' Apply standard formatting to a comparison_table from run_population_chisq().
#' Adds diff_sig column and formats proportions with pad_pct().
#' Returns a display-ready data frame with columns:
#'   category_label, survey_prop, acs_prop, diff_sig
#'
#' @param result Output of run_population_chisq() for one variable.
#'
#' @return Formatted data frame ready for knitr::kable().
format_pop_table <- function(result) {
  ct <- result$comparison_table
  ct$diff_sig    <- fmt_diff_sig(ct)
  ct$survey_prop <- pad_pct(ct$survey_prop)
  ct$acs_prop    <- pad_pct(ct$acs_prop)
  ct[, c("category_label", "survey_prop", "acs_prop", "diff_sig")]
}


# Function 9: Construct a weighted-average summary row
# =====================================================
#' Build a weighted-average summary row for a population comparison table.
#' Appends to the output of format_pop_table().
#'
#' @param ct  The comparison_table from run_population_chisq() (raw, before formatting).
#' @param result The full result object (used for p_value via p_stars()).
#'
#' @return Single-row data frame matching format_pop_table() column structure.
make_weighted_avg_row <- function(ct, result) {
  avg_diff_pct <- sum(ct$acs_prop * ct$diff) * 100
  sign_char    <- ifelse(avg_diff_pct >= 0, "+", "\u2212")
  num_part     <- formatC(sprintf("%.1f", abs(avg_diff_pct)), width = 4, flag = " ")
  num_part     <- gsub(" ", "\u2007", num_part)
  stars        <- formatC(p_stars(result$p_value), width = -3, flag = " ")
  stars        <- gsub(" ", "\u2007", stars)

  data.frame(
    category_label = "**Wt. Average**",
    survey_prop    = pad_pct(sum(ct$acs_prop * ct$survey_prop)),
    acs_prop       = pad_pct(sum(ct$acs_prop * ct$acs_prop)),
    diff_sig       = paste0(sign_char, num_part, "%", stars),
    stringsAsFactors = FALSE
  )
}


# Summary: What external validity analysis adds to the study
# ===========================================================
#
# Our survey used a Qualtrics Audience panel — a convenience sample,
# not a probability sample. Convenience samples can be fast and cost-
# effective, but they raise the question: "who did we actually study?"
#
# The ACS comparison is a "representativeness check" — it doesn't
# make the sample probability-based, but it shows readers:
# 1. Which demographic groups are over- or under-represented
# 2. Whether any demographic gaps are large (Cohen's h) or trivial
# 3. The specific proportions, so replication attempts can re-weight
#
# This is standard practice for survey experiments in law/social science.
# Even non-representative samples can yield valid results for treatment
# effect estimation (internal validity), but documenting the demographic
# profile helps readers assess how findings might generalize.
