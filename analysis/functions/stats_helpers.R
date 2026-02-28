# stats_helpers.R - Literate Programming Version
# =========================================================
# Statistical Helper Functions for Survey Analysis
# 
# This file contains functions that perform statistical calculations
# These are the "behind the scenes" workers that do the math
#
# For beginners: Statistics help us understand if differences between
# groups are "real" or just due to random chance
# =========================================================

# Function 0: Validate data for statistical tests
# =================================================
#' Check if groups have sufficient data for statistical testing
#' 
#' Statistical tests like Wilcoxon and Dunn require at least 2 observations
#' per group. This function checks each group and warns about insufficient data.
#' 
#' @param data Data frame
#' @param dv Variable to analyze
#' @param group_var Grouping variable
#' @param min_n Minimum observations needed per group (default 2)
#' 
#' @return List with validation results and filtered data
validate_group_data <- function(data, 
                               dv,
                               group_var = "GROUP",
                               min_n = 2) {
  
  # Remove rows with missing values in the dependent variable
  data_clean <- data[!is.na(data[[dv]]), ]
  
  # Count observations per group
  group_counts <- table(data_clean[[group_var]])
  
  # Find groups with insufficient data
  insufficient <- names(group_counts[group_counts < min_n])
  sufficient <- names(group_counts[group_counts >= min_n])
  
  # Create validation result
  result <- list(
    is_valid = length(insufficient) == 0,
    total_n = nrow(data_clean),
    group_counts = as.data.frame(group_counts),
    insufficient_groups = insufficient,
    sufficient_groups = sufficient,
    filtered_data = data_clean[data_clean[[group_var]] %in% sufficient, ],
    message = ""
  )
  
  # Create informative message
  if (length(insufficient) > 0) {
    result$message <- paste0(
      "WARNING: Groups with insufficient data (<", min_n, " obs): ",
      paste(insufficient, collapse = ", "),
      "\nSkipping statistical tests. Available groups: ",
      paste(sufficient, collapse = ", ")
    )
  } else {
    result$message <- paste0(
      "Data validation passed. All groups have >=", min_n, " observations."
    )
  }
  
  return(result)
}

# Function 1: Calculate basic descriptive statistics
# ===================================================
#' Calculate descriptive statistics by group
#' 
#' Descriptive statistics summarize data with single numbers:
#' - Mean: The average (add all values, divide by count)
#' - SD (Standard Deviation): How spread out the data is
#' - Median: The middle value when sorted (robust to outliers)
#' 
#' @param data Data frame (table) containing survey responses
#' @param dv Dependent variable - the column we're analyzing
#' @param group_var Column that contains group labels (A, B, C)
#' @param stats Which statistics to calculate (mean, sd, median)
#' 
#' @return A neat table with statistics for each group
#' 
#' @examples
#' # Get average PD1 scores for each group
#' stats <- calculate_descriptive_stats(surveyData, "PD1", "GROUP")
calculate_descriptive_stats <- function(data, 
                                       dv,  # Variable to summarize
                                       group_var = "GROUP",  # Grouping column
                                       stats = c("mean", "sd", "median")) {  # What to calculate
  
  # Step 1: Build a list of calculations to perform
  summary_calls <- list()
  
  # Add mean calculation if requested
  if ("mean" %in% stats) {
    # rlang::expr creates an expression to evaluate later
    # This says: "calculate mean of dv, removing any missing values"
    summary_calls[[paste0("mean_", dv)]] <- rlang::expr(mean(!!rlang::sym(dv), na.rm = TRUE))
  }
  
  # Add standard deviation calculation if requested
  if ("sd" %in% stats) {
    summary_calls[[paste0("sd_", dv)]] <- rlang::expr(sd(!!rlang::sym(dv), na.rm = TRUE))
  }
  
  # Add median calculation if requested
  if ("median" %in% stats) {
    summary_calls[[paste0("median_", dv)]] <- rlang::expr(median(!!rlang::sym(dv), na.rm = TRUE))
  }
  
  # Step 2: Apply calculations to each group
  # The pipe %>% passes data from left to right
  result <- data %>%
    dplyr::group_by(!!rlang::sym(group_var)) %>%  # Split by group
    dplyr::summarise(!!!summary_calls, .groups = "drop")  # Calculate stats
  
  return(result)
}

# Function 2: Run Dunn's test for multiple group comparisons
# ===========================================================
#' Run Dunn's test wrapper
#' 
#' Dunn's test compares multiple groups when data isn't normally distributed.
#' It's like ANOVA but doesn't assume bell-curve shaped data.
#' 
#' Why use this?
#' - Survey data often isn't normally distributed
#' - We have 3 groups to compare (A, B, C)
#' - We want to know which specific pairs differ
#' 
#' @param data Data frame with survey responses
#' @param formula A formula like "PD1 ~ GROUP" (read as: PD1 depends on GROUP)
#' @param p_adjust Method to adjust p-values for multiple comparisons
#' 
#' @return Test results showing which groups differ significantly
run_dunn_test <- function(data, 
                         formula,  # Like "PD1 ~ GROUP"
                         p_adjust = "holm") {  # Adjustment method
  
  # Convert string to formula if needed
  # Formulas tell R about relationships between variables
  if (is.character(formula)) {
    formula <- as.formula(formula)  # Convert "PD1 ~ GROUP" to formula object
  }
  
  # Run the actual Dunn test
  # This compares all pairs: A vs B, A vs C, B vs C
  result <- rstatix::dunn_test(
    data = data,
    formula = formula,
    p.adjust.method = p_adjust  # Adjust for multiple comparisons
  )
  
  return(result)
}

# Function 3: Calculate effect sizes with Vargha & Delaney's A
# =============================================================
#' Calculate Vargha & Delaney's A effect size
#' 
#' Effect size tells us HOW BIG a difference is, not just IF it exists.
#' P-values tell us "is there a difference?" 
#' Effect sizes tell us "how important is this difference?"
#' 
#' Vargha & Delaney's A interpretation:
#' - 0.50 = No difference between groups
#' - 0.56 = Small effect (slight difference)
#' - 0.64 = Medium effect (noticeable difference)
#' - 0.71+ = Large effect (big difference)
#' 
#' @param data Data frame
#' @param dv Variable to analyze
#' @param group_var Grouping variable
#' 
#' @return VDA values for all group pairs
calculate_vda <- function(data, 
                         dv,  # Variable to analyze
                         group_var = "GROUP") {  # Grouping column
  
  # Calculate VDA for all pairs of groups
  # This tells us the probability that a random person from group 1
  # will have a higher score than a random person from group 2
  result <- rcompanion::multiVDA(
    x = data[[dv]],  # The values to compare
    g = data[[group_var]]  # The groups
  )
  
  return(result)
}

# Function 4: Create formatted caption with effect sizes
# ========================================================
#' Format caption showing VDA effect sizes
#' 
#' This creates the text that appears at the bottom of plots,
#' showing which groups differ and by how much.
#' 
#' @param dunn_results Results from Dunn's test
#' @param vda_results Results from VDA calculation
#' @param p_threshold Text to show for p-value significance
#' @param pairs_to_show Which pairs to include (NULL = all significant ones)
#' 
#' @return Formatted text string for plot caption
format_vda_caption <- function(dunn_results, 
                              vda_results,
                              p_threshold = "*p < 0.001",  # Significance indicator
                              pairs_to_show = NULL) {  # Which pairs to display
  
  # Step 1: Extract the VDA values for each pair
  vda_pairs <- vda_results[["pairs"]][["VDA"]]
  
  # Step 2: Decide which pairs to show
  if (is.null(pairs_to_show)) {
    # By default, show only statistically significant pairs (p < 0.05)
    pairs_to_show <- which(dunn_results$p.adj < 0.05)
  }
  
  # Step 3: Build the caption text
  caption_parts <- "    Pairwise Effect Sizes (Vargha & Delaney 2000): "
  
  # Add each significant comparison
  for (i in pairs_to_show) {
    if (i <= nrow(dunn_results)) {
      # Format: "A-B, a = 0.65*"
      caption_parts <- paste0(
        caption_parts,
        dunn_results$group1[i], "–", dunn_results$group2[i],
        ", a = ", signif(vda_pairs[i], 2), "*    "  # Round to 2 significant figures
      )
    }
  }
  
  # Step 4: Add p-value threshold note
  caption_parts <- paste0(caption_parts, "    ", p_threshold)
  
  return(caption_parts)
}

# Function 5: Remove outliers by group
# =====================================
#' Remove outliers using quantile method by group
#' 
#' Outliers are extreme values that can distort our analysis.
#' For price questions, someone might write $1,000,000 as a joke.
#' This function removes the most extreme values (top 1% by default).
#' 
#' Why remove by group?
#' - Groups might have different typical ranges
#' - We don't want one group's outliers to affect another
#' 
#' @param data Data frame
#' @param dv Variable with potential outliers (usually prices)
#' @param group_var Grouping variable
#' @param percentile Keep values below this percentile (0.99 = remove top 1%)
#' 
#' @return Data frame with outliers removed
remove_outliers_by_group <- function(data, 
                                    dv,  # Variable to clean
                                    group_var = "GROUP",
                                    percentile = 0.99) {  # Remove top 1%
  
  # Get list of unique groups (A, B, C)
  groups <- unique(data[[group_var]])
  
  # Process each group separately
  filtered_groups <- list()
  
  for (grp in groups) {
    # Step 1: Get data for just this group
    grp_data <- dplyr::filter(data, !!rlang::sym(group_var) == grp)
    
    # Step 2: Find the cutoff value (99th percentile)
    # This is the value where 99% of responses are below it
    upper_bound <- quantile(grp_data[[dv]], percentile, na.rm = TRUE)
    
    # Step 3: Keep only values below the cutoff
    grp_filtered <- grp_data[grp_data[[dv]] <= upper_bound, ]
    
    # Step 4: Store the filtered group data
    filtered_groups[[grp]] <- grp_filtered
  }
  
  # Step 5: Combine all groups back together
  result <- dplyr::bind_rows(filtered_groups)
  
  return(result)
}

# Function 6: Test if values differ from a specific number
# =========================================================
#' Test if each group's values differ from a specific number
#' 
#' For delta questions, we want to know if the difference between
#' Q2 and Q3 is significantly different from zero.
#' If it's not different from zero, people's preferences match the law!
#' 
#' @param data Data frame
#' @param dv Variable to test (usually a delta variable)
#' @param group_var Grouping variable
#' @param mu The value to test against (usually 0)
#' 
#' @return List of test results for each group
test_delta_nonzero <- function(data, 
                              dv,  # Variable to test
                              group_var = "GROUP",
                              mu = 0) {  # Test if different from zero
  
  # Get list of groups
  groups <- unique(data[[group_var]])
  results <- list()
  
  # Test each group separately
  for (grp in groups) {
    # Get data for this group
    grp_data <- dplyr::filter(data, !!rlang::sym(group_var) == grp)
    
    # Run Wilcoxon signed-rank test
    # This tests if the median is different from mu (usually 0)
    # It's like a t-test but doesn't assume normal distribution
    results[[grp]] <- wilcox.test(
      grp_data[[dv]],  # Values to test
      conf.int = TRUE,  # Get confidence interval
      mu = mu  # Value to test against
    )
  }
  
  return(results)
}

# Function 7: Chi-square test for categorical variables
# ======================================================
#' Create contingency table and run chi-square test
#' 
#' Chi-square tests whether categorical variables are related.
#' For example: Is race distribution the same across groups?
#' 
#' How it works:
#' 1. Creates a table showing counts for each combination
#' 2. Calculates what we'd expect if there's no relationship
#' 3. Compares actual counts to expected counts
#' 4. Large differences = variables are related
#' 
#' @param data Data frame
#' @param var1 First categorical variable (e.g., "RACE")
#' @param var2 Second categorical variable (e.g., "GROUP")
#' 
#' @return Chi-square test results and contingency table
run_chisq_test <- function(data, var1, var2) {
  
  # Step 1: Create contingency table (cross-tabulation)
  # This shows how many people are in each combination
  # Example: How many White people in Group A, Black in Group B, etc.
  cont_table <- table(data[[var1]], data[[var2]])
  
  # Step 2: Run chi-square test on the table
  # This tests if the distribution is independent
  result <- chisq.test(cont_table)
  
  # Return both the table and test results
  return(list(
    table = cont_table,  # The actual counts
    test = result  # The statistical test
  ))
}

# Function 8: Compare groups using ANOVA or Kruskal-Wallis
# =========================================================
#' Compare groups using appropriate statistical test
#' 
#' Two main tests for comparing multiple groups:
#' 1. ANOVA: For normally distributed data (bell curve)
#' 2. Kruskal-Wallis: For any distribution (doesn't assume bell curve)
#' 
#' @param data Data frame
#' @param dv Variable to compare across groups
#' @param group_var Grouping variable
#' @param test_type "parametric" (ANOVA) or "np" (Kruskal-Wallis)
#' 
#' @return Test results
compare_groups <- function(data,
                          dv,  # Variable to compare
                          group_var = "GROUP",
                          test_type = "np") {  # Default to non-parametric
  
  if (test_type == "parametric") {
    # ANOVA (Analysis of Variance)
    # Tests if means differ across groups
    # Assumes normal distribution and equal variances
    formula <- as.formula(paste(dv, "~", group_var))
    result <- aov(formula, data = data)  # aov = analysis of variance
    return(summary(result))
    
  } else {
    # Kruskal-Wallis test
    # Tests if distributions differ across groups
    # Doesn't assume normal distribution (safer for survey data)
    formula <- as.formula(paste(dv, "~", group_var))
    result <- kruskal.test(formula, data = data)
    return(result)
  }
}

# =========================================================
# Cross-Scenario Summary Table Functions
# =========================================================
# These functions compute consolidated summary statistics
# and VDA effect sizes across all seven scenarios, producing
# the cross-scenario comparison tables in §7 (Scenario
# Analyses Overview).
# =========================================================

#' Compute one summary row for a scenario × question combination
#'
#' Returns a one-row data frame with Mean (SD) for each group.
#' Treatment groups additionally show [% difference from control].
#' For delta (suffix "25"), all groups show [Δ/Q2 × 100] — the
#' fraction of perceived consequences that exceed preferences.
#'
#' @param data Data frame (survey_data or survey_data_eu)
#' @param scenario_code Two-letter code (PD, PU, EU, CA, LS, WN, FS)
#' @param question_suffix Column suffix: "1", "2", "3", or "25" (delta)
#' @param control_group Control group label (default "C")
#'
#' @return One-row data frame: Scenario | Control (C) | Short (A) | Full+Primer (B)
compute_scenario_summary_row <- function(data, scenario_code, question_suffix,
                                         control_group = "C") {
  col_name <- paste0(scenario_code, question_suffix)

  if (!col_name %in% names(data)) {
    warning(paste("Column", col_name, "not found in data"))
    return(NULL)
  }

  groups <- sort(unique(data$GROUP))
  treat_b <- if ("B1" %in% groups) "B1" else "B"
  is_eu <- "B1" %in% groups

  # Compute mean and SD per group
  group_stats <- list()
  for (grp in groups) {
    vals <- data[[col_name]][data$GROUP == grp]
    vals <- vals[!is.na(vals)]
    group_stats[[grp]] <- list(mean = mean(vals), sd = sd(vals), n = length(vals))
  }

  control_mean <- group_stats[[control_group]]$mean

  # Run Dunn's test (Holm-adjusted) for significance bolding
  data_clean <- data[!is.na(data[[col_name]]), ]
  formula <- as.formula(paste(col_name, "~ GROUP"))
  dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

  # Map each treatment group to its p-value vs control
  p_ac <- dunn$p.adj[dunn$group1 == "A" & dunn$group2 == "C"]
  p_bc <- dunn$p.adj[dunn$group1 == treat_b & dunn$group2 == "C"]
  if (length(p_ac) == 0) p_ac <- 1
  if (length(p_bc) == 0) p_bc <- 1

  sig_map <- list()
  sig_map[["A"]] <- p_ac < 0.05
  sig_map[[treat_b]] <- p_bc < 0.05
  sig_map[["C"]] <- FALSE

  # Format a single cell: "Mean (SD)" or "Mean (SD) [+X.X%]"
  # Bracket is bolded when Dunn's test shows significant diff from control
  format_cell <- function(grp) {
    s <- group_stats[[grp]]
    is_sig <- isTRUE(sig_map[[grp]])

    if (question_suffix == "25") {
      # Delta: all groups show delta/Q2 percentage
      # Use rounded values so displayed % is consistent with displayed means
      q2_col <- paste0(scenario_code, "2")
      q2_mean <- mean(data[[q2_col]][data$GROUP == grp], na.rm = TRUE)
      r_delta <- round(s$mean, 2)
      r_q2    <- round(q2_mean, 2)
      pct <- if (abs(r_q2) > 0.001) r_delta / r_q2 * 100 else NA
      if (!is.na(pct)) {
        pct_str <- sprintf("[%+.1f%%]", pct)
        if (is_sig) pct_str <- paste0("**", pct_str, "**")
        return(sprintf("%.2f (%.2f) %s", s$mean, s$sd, pct_str))
      }
    } else if (grp != control_group) {
      # Q1/Q2/Q3: treatment groups show % diff from control
      # Use rounded values so displayed % is consistent with displayed means
      r_treat <- round(s$mean, 2)
      r_ctrl  <- round(control_mean, 2)
      pct <- if (abs(r_ctrl) > 0.001) {
        (r_treat - r_ctrl) / r_ctrl * 100
      } else {
        NA
      }
      if (!is.na(pct)) {
        pct_str <- sprintf("[%+.1f%%]", pct)
        if (is_sig) pct_str <- paste0("**", pct_str, "**")
        return(sprintf("%.2f (%.2f) %s", s$mean, s$sd, pct_str))
      }
    }

    sprintf("%.2f (%.2f)", s$mean, s$sd)
  }

  # Scenario label (with dagger for EU)
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )
  scenario_label <- unname(scenario_names[scenario_code])
  if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

  data.frame(
    Scenario = scenario_label,
    `Control (C)` = format_cell("C"),
    `Short (A)` = format_cell("A"),
    `Full+Primer (B)` = format_cell(treat_b),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Compute one VDA effect-size row for a scenario × question
#'
#' Runs Dunn's test (Holm correction) and multiVDA for one
#' scenario/question. Returns VDA values with significance stars
#' and effect size markers.
#'
#' Significance: * p < .05, ** p < .01, *** p < .001
#' Effect thresholds (Vargha & Delaney 2000):
#'   (S)mall |VDA - 0.5| >= 0.06
#'   (M)edium >= 0.14
#'   (L)arge >= 0.21
#'
#' @param data Data frame (survey_data or survey_data_eu)
#' @param scenario_code Two-letter code
#' @param question_suffix Column suffix: "1", "2", "3", or "25"
#'
#' @return One-row data frame with 7 columns: Scenario, cs_vda, cs_sig, cfp_vda, cfp_sig, sfp_vda, sfp_sig
compute_scenario_vda_row <- function(data, scenario_code, question_suffix) {
  col_name <- paste0(scenario_code, question_suffix)

  if (!col_name %in% names(data)) {
    warning(paste("Column", col_name, "not found in data"))
    return(NULL)
  }

  data_clean <- data[!is.na(data[[col_name]]), ]
  is_eu <- "B1" %in% unique(data_clean$GROUP)

  # Run Dunn's test (pairwise post-hoc)
  formula <- as.formula(paste(col_name, "~ GROUP"))
  dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

  # Run VDA effect sizes
  vda <- rcompanion::multiVDA(x = data_clean[[col_name]], g = data_clean$GROUP)

  # Split VDA into separate value and annotation columns for decimal alignment
  # Asterisks escaped for markdown (kable pipe tables interpret raw * as bold/italic)
  format_vda_split <- function(vda_val, p_val) {
    stars <- if (p_val < 0.001) "\\*\\*\\*"
             else if (p_val < 0.01) "\\*\\*"
             else if (p_val < 0.05) "\\*"
             else "n.s."
    vda_dist <- abs(vda_val - 0.5)
    effect <- if (vda_dist >= 0.21) " (L)"
              else if (vda_dist >= 0.14) " (M)"
              else if (vda_dist >= 0.06) " (S)"
              else ""
    list(vda = sprintf("%.3f", vda_val), anno = paste0(stars, effect))
  }

  # Scenario label (unname strips R vector names that leak into row names)
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )
  scenario_label <- unname(scenario_names[scenario_code])
  if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

  # EU marker for cells involving B/B1 (placed on annotation side)
  eu_marker <- if (is_eu) "\u2020" else ""

  # multiVDA pair indices (alphabetical): 1 = A-B, 2 = A-C, 3 = B-C
  # Desired order: C-A, C-B, A-B → flip pairs 2 & 3, keep pair 1
  p1 <- format_vda_split(1 - vda$pairs$VDA[2], dunn$p.adj[2])  # C-A
  p2 <- format_vda_split(1 - vda$pairs$VDA[3], dunn$p.adj[3])  # C-B
  p3 <- format_vda_split(vda$pairs$VDA[1], dunn$p.adj[1])       # A-B

  result <- data.frame(
    Scenario = scenario_label,
    cs_vda  = p1$vda,
    cs_sig  = p1$anno,
    cfp_vda = p2$vda,
    cfp_sig = paste0(p2$anno, eu_marker),
    sfp_vda = p3$vda,
    sfp_sig = paste0(p3$anno, eu_marker),
    stringsAsFactors = FALSE
  )
  result
}

#' Build a cross-scenario summary table for one question type
#'
#' Loops over all scenarios, switching to the EU dataset for
#' Educational Use, and returns a combined data frame.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param question_suffix "1", "2", "3", or "25" (delta)
#' @param scenarios Character vector of scenario codes (in display order)
#'
#' @return Data frame with one row per scenario
build_summary_table <- function(data_main, data_eu, question_suffix,
                                scenarios = c("PD", "PU", "EU", "CA",
                                              "LS", "WN", "FS")) {
  rows <- lapply(scenarios, function(sc) {
    data_use <- if (sc == "EU") data_eu else data_main
    compute_scenario_summary_row(data_use, sc, question_suffix)
  })
  dplyr::bind_rows(rows)
}

#' Build a cross-scenario VDA effect-size table for one question type
#'
#' Same loop pattern as build_summary_table(), calling
#' compute_scenario_vda_row() for each scenario.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param question_suffix "1", "2", "3", or "25" (delta)
#' @param scenarios Character vector of scenario codes (in display order)
#'
#' @return Data frame with one row per scenario
build_vda_table <- function(data_main, data_eu, question_suffix,
                            scenarios = c("PD", "PU", "EU", "CA",
                                          "LS", "WN", "FS")) {
  rows <- lapply(scenarios, function(sc) {
    data_use <- if (sc == "EU") data_eu else data_main
    compute_scenario_vda_row(data_use, sc, question_suffix)
  })
  dplyr::bind_rows(rows)
}

# =========================================================
# Cross-Scenario Price & Wilcoxon Summary Table Functions
# =========================================================
# These functions compute consolidated price statistics
# (Q4) and Wilcoxon signed-rank test summaries (Delta)
# across all seven scenarios, for use in §8 (Question
# Summaries).
# =========================================================

#' Compute one price summary row for a scenario
#'
#' Like compute_scenario_summary_row() but tailored for Q4 (price):
#' - Applies 99th-percentile outlier trimming per group before stats
#' - Reports Median (SD) instead of Mean (SD)
#' - Treatment groups show [% difference from control median]
#'
#' @param data Data frame (survey_data or survey_data_eu)
#' @param scenario_code Two-letter code (PD, PU, EU, CA, LS, WN, FS)
#' @param control_group Control group label (default "C")
#'
#' @return One-row data frame: Scenario | Control (C) | Short (A) | Full+Primer (B)
compute_scenario_price_row <- function(data, scenario_code,
                                       control_group = "C") {
  col_name <- paste0(scenario_code, "4")

  if (!col_name %in% names(data)) {
    warning(paste("Column", col_name, "not found in data"))
    return(NULL)
  }

  # Trim outliers at 99th percentile per group
  data_trimmed <- remove_outliers_by_group(data, col_name, "GROUP", 0.99)

  groups <- sort(unique(data_trimmed$GROUP))
  treat_b <- if ("B1" %in% groups) "B1" else "B"
  is_eu <- "B1" %in% groups

  # Compute median and SD per group
  group_stats <- list()
  for (grp in groups) {
    vals <- data_trimmed[[col_name]][data_trimmed$GROUP == grp]
    vals <- vals[!is.na(vals)]
    group_stats[[grp]] <- list(median = median(vals), sd = sd(vals), n = length(vals))
  }

  control_median <- group_stats[[control_group]]$median

  # Run Dunn's test on trimmed data for significance bolding
  data_clean <- data_trimmed[!is.na(data_trimmed[[col_name]]), ]
  formula <- as.formula(paste(col_name, "~ GROUP"))
  dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

  p_ac <- dunn$p.adj[dunn$group1 == "A" & dunn$group2 == "C"]
  p_bc <- dunn$p.adj[dunn$group1 == treat_b & dunn$group2 == "C"]
  if (length(p_ac) == 0) p_ac <- 1
  if (length(p_bc) == 0) p_bc <- 1

  sig_map <- list()
  sig_map[["A"]] <- p_ac < 0.05
  sig_map[[treat_b]] <- p_bc < 0.05
  sig_map[["C"]] <- FALSE

  # Format cell: "Median (SD)" or "Median (SD) [+X.X%]"
  # Bracket bolded when Dunn's test vs control is significant
  format_cell <- function(grp) {
    s <- group_stats[[grp]]
    is_sig <- isTRUE(sig_map[[grp]])
    if (grp != control_group) {
      # Use rounded values so displayed % is consistent with displayed medians
      r_treat <- round(s$median, 2)
      r_ctrl  <- round(control_median, 2)
      pct <- if (abs(r_ctrl) > 0.001) {
        (r_treat - r_ctrl) / r_ctrl * 100
      } else {
        NA
      }
      if (!is.na(pct)) {
        pct_str <- sprintf("[%+.1f%%]", pct)
        if (is_sig) pct_str <- paste0("**", pct_str, "**")
        return(sprintf("%.2f (%.2f) %s", s$median, s$sd, pct_str))
      }
    }
    sprintf("%.2f (%.2f)", s$median, s$sd)
  }

  # Scenario label (with dagger for EU)
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )
  scenario_label <- unname(scenario_names[scenario_code])
  if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

  data.frame(
    Scenario = scenario_label,
    `Control (C)` = format_cell("C"),
    `Short (A)` = format_cell("A"),
    `Full+Primer (B)` = format_cell(treat_b),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build a cross-scenario price summary table
#'
#' Loops over all scenarios, switching to the EU dataset for
#' Educational Use. Reports Median (SD) after 99th-percentile
#' outlier trimming.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param scenarios Character vector of scenario codes (in display order)
#'
#' @return Data frame with one row per scenario
build_price_summary_table <- function(data_main, data_eu,
                                      scenarios = c("PD", "PU", "EU", "CA",
                                                    "LS", "WN", "FS")) {
  rows <- lapply(scenarios, function(sc) {
    data_use <- if (sc == "EU") data_eu else data_main
    compute_scenario_price_row(data_use, sc)
  })
  dplyr::bind_rows(rows)
}

#' Compute one price VDA effect-size row for a scenario
#'
#' Like compute_scenario_vda_row() but applies 99th-percentile
#' outlier trimming before running Dunn's test and multiVDA.
#'
#' @param data Data frame (survey_data or survey_data_eu)
#' @param scenario_code Two-letter code
#'
#' @return One-row data frame with 7 columns: Scenario, cs_vda, cs_sig, cfp_vda, cfp_sig, sfp_vda, sfp_sig
compute_scenario_price_vda_row <- function(data, scenario_code) {
  col_name <- paste0(scenario_code, "4")

  if (!col_name %in% names(data)) {
    warning(paste("Column", col_name, "not found in data"))
    return(NULL)
  }

  # Trim outliers at 99th percentile per group before tests
  data_trimmed <- remove_outliers_by_group(data, col_name, "GROUP", 0.99)
  data_clean <- data_trimmed[!is.na(data_trimmed[[col_name]]), ]
  is_eu <- "B1" %in% unique(data_clean$GROUP)

  # Run Dunn's test (pairwise post-hoc)
  formula <- as.formula(paste(col_name, "~ GROUP"))
  dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

  # Run VDA effect sizes
  vda <- rcompanion::multiVDA(x = data_clean[[col_name]], g = data_clean$GROUP)

  # Split VDA into separate value and annotation columns for decimal alignment
  # Asterisks escaped for markdown (kable pipe tables interpret raw * as bold/italic)
  format_vda_split <- function(vda_val, p_val) {
    stars <- if (p_val < 0.001) "\\*\\*\\*"
             else if (p_val < 0.01) "\\*\\*"
             else if (p_val < 0.05) "\\*"
             else "n.s."
    vda_dist <- abs(vda_val - 0.5)
    effect <- if (vda_dist >= 0.21) " (L)"
              else if (vda_dist >= 0.14) " (M)"
              else if (vda_dist >= 0.06) " (S)"
              else ""
    list(vda = sprintf("%.3f", vda_val), anno = paste0(stars, effect))
  }

  # Scenario label (unname strips R vector names)
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )
  scenario_label <- unname(scenario_names[scenario_code])
  if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

  # EU marker for cells involving B/B1 (placed on annotation side)
  eu_marker <- if (is_eu) "\u2020" else ""

  # multiVDA pair indices (alphabetical): 1 = A-B, 2 = A-C, 3 = B-C
  # Desired order: C-A, C-B, A-B → flip pairs 2 & 3, keep pair 1
  p1 <- format_vda_split(1 - vda$pairs$VDA[2], dunn$p.adj[2])  # C-A
  p2 <- format_vda_split(1 - vda$pairs$VDA[3], dunn$p.adj[3])  # C-B
  p3 <- format_vda_split(vda$pairs$VDA[1], dunn$p.adj[1])       # A-B

  result <- data.frame(
    Scenario = scenario_label,
    cs_vda  = p1$vda,
    cs_sig  = p1$anno,
    cfp_vda = p2$vda,
    cfp_sig = paste0(p2$anno, eu_marker),
    sfp_vda = p3$vda,
    sfp_sig = paste0(p3$anno, eu_marker),
    stringsAsFactors = FALSE
  )
  result
}

#' Build a cross-scenario price VDA effect-size table
#'
#' Loops over all scenarios, switching to the EU dataset for
#' Educational Use. Applies 99th-percentile outlier trimming
#' before running Dunn's test and multiVDA.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param scenarios Character vector of scenario codes (in display order)
#'
#' @return Data frame with one row per scenario
build_price_vda_table <- function(data_main, data_eu,
                                  scenarios = c("PD", "PU", "EU", "CA",
                                                "LS", "WN", "FS")) {
  rows <- lapply(scenarios, function(sc) {
    data_use <- if (sc == "EU") data_eu else data_main
    compute_scenario_price_vda_row(data_use, sc)
  })
  dplyr::bind_rows(rows)
}

#' Build a cross-scenario Wilcoxon signed-rank test table (Delta)
#'
#' For each scenario × group, runs wilcox.test(delta, mu = 0,
#' conf.int = TRUE). Returns a compact table with test
#' statistics, significance level, and conclusion.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param scenarios Character vector of scenario codes (in display order)
#'
#' @return Data frame: Scenario | Group | n | V | Non-Zero | Dir. | Sig.
build_wilcoxon_table <- function(data_main, data_eu,
                                 scenarios = c("PD", "PU", "EU", "CA",
                                               "LS", "WN", "FS")) {
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )

  rows <- list()
  for (sc in scenarios) {
    data_use <- if (sc == "EU") data_eu else data_main
    col_name <- paste0(sc, "25")
    is_eu <- sc == "EU"

    if (!col_name %in% names(data_use)) next

    groups <- sort(unique(data_use$GROUP))
    scenario_label <- unname(scenario_names[sc])
    if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

    for (grp in groups) {
      vals <- data_use[[col_name]][data_use$GROUP == grp]
      vals <- vals[!is.na(vals)]

      if (length(vals) < 2) next

      tr <- wilcox.test(vals, mu = 0, conf.int = TRUE)

      # Group label: canonical format with dagger for B1
      grp_labels <- c(C = "Control (C)", A = "Short (A)",
                       B = "Full + Primer (B)",
                       B1 = "Full + Primer (B1)\u2020")
      grp_label <- unname(grp_labels[grp])
      if (is.na(grp_label)) grp_label <- grp

      # Asterisk notation (escaped for markdown pipe tables)
      sig_label <- if (tr$p.value < 0.001) "\\*\\*\\*"
                   else if (tr$p.value < 0.01) "\\*\\*"
                   else if (tr$p.value < 0.05) "\\*"
                   else "n.s."

      rows[[length(rows) + 1]] <- data.frame(
        Scenario     = scenario_label,
        Group        = grp_label,
        n            = length(vals),
        V            = unname(tr$statistic),
        `Non-Zero`   = ifelse(tr$p.value < 0.05, "Yes", "No"),
        Dir.         = ifelse(unname(tr$estimate) >= 0, "+", "\u2212"),
        Sig.         = sig_label,
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

# =========================================================
# Long-Format Cross-Scenario Summary Tables (Chapter 8)
# =========================================================
# These functions produce one row per scenario x group,
# matching the per-scenario table format from Ch. 7.X with
# separate columns for each statistic.
# =========================================================

#' Build a long-format cross-scenario summary table for Q1/Q2/Q3/Delta
#'
#' Produces one row per scenario x group with separate columns for
#' Mean, SD, and Median. Treatment groups show [% difference from control]
#' in the Mean column; bold brackets indicate Dunn's test significance.
#' For Delta (suffix "25"), all groups show [delta/Q2 x 100].
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param question_suffix "1", "2", "3", or "25" (delta)
#' @param scenarios Character vector of scenario codes
#'
#' @return Data frame: Scenario | Group | Mean | SD | Median
build_summary_table_long <- function(data_main, data_eu, question_suffix,
                                     scenarios = c("PD", "PU", "EU", "CA",
                                                   "LS", "WN", "FS")) {
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )

  group_labels <- c(
    C = "Control (C)", A = "Short (A)",
    B = "Full + Primer (B)", B1 = "Full + Primer (B1)\u2020"
  )

  rows <- list()
  for (sc in scenarios) {
    data_use <- if (sc == "EU") data_eu else data_main
    col_name <- paste0(sc, question_suffix)
    is_eu <- sc == "EU"

    if (!col_name %in% names(data_use)) next

    groups <- sort(unique(data_use$GROUP))
    treat_b <- if ("B1" %in% groups) "B1" else "B"

    # Stats per group
    group_stats <- list()
    for (grp in groups) {
      vals <- data_use[[col_name]][data_use$GROUP == grp]
      vals <- vals[!is.na(vals)]
      group_stats[[grp]] <- list(
        mean = mean(vals), sd = sd(vals),
        median = median(vals), n = length(vals)
      )
    }

    control_mean <- group_stats[["C"]]$mean

    # Dunn's test for significance
    data_clean <- data_use[!is.na(data_use[[col_name]]), ]
    formula <- as.formula(paste(col_name, "~ GROUP"))
    dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

    p_ac <- dunn$p.adj[dunn$group1 == "A" & dunn$group2 == "C"]
    p_bc <- dunn$p.adj[dunn$group1 == treat_b & dunn$group2 == "C"]
    if (length(p_ac) == 0) p_ac <- 1
    if (length(p_bc) == 0) p_bc <- 1

    scenario_label <- unname(scenario_names[sc])
    if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

    grp_order <- intersect(c("C", "A", treat_b), groups)

    for (i in seq_along(grp_order)) {
      grp <- grp_order[i]
      s <- group_stats[[grp]]

      # Significance stars for treatment groups
      p_val <- if (grp == "C") 1
               else if (grp == "A") p_ac
               else p_bc
      stars <- if (p_val < 0.001) "\\*\\*\\*"
               else if (p_val < 0.01) "\\*\\*"
               else if (p_val < 0.05) "\\*"
               else ""

      # Determine percentage string
      if (question_suffix == "25") {
        # Delta: all groups show delta/Q2 percentage
        q2_col <- paste0(sc, "2")
        q2_mean <- mean(data_use[[q2_col]][data_use$GROUP == grp], na.rm = TRUE)
        r_delta <- round(s$mean, 2)
        r_q2 <- round(q2_mean, 2)
        pct <- if (abs(r_q2) > 0.001) r_delta / r_q2 * 100 else NA

        if (!is.na(pct)) {
          pct_str <- paste0(sprintf("[%+.1f%%]", pct), stars)
        } else {
          pct_str <- ""
        }

        mean_str <- if (nchar(pct_str) > 0) {
          sprintf("%.2f %s", s$mean, pct_str)
        } else {
          sprintf("%.2f", s$mean)
        }
      } else if (grp != "C") {
        # Q1/Q2/Q3: treatment groups show % diff from control
        r_treat <- round(s$mean, 2)
        r_ctrl <- round(control_mean, 2)
        pct <- if (abs(r_ctrl) > 0.001) (r_treat - r_ctrl) / abs(r_ctrl) * 100 else NA

        if (!is.na(pct)) {
          pct_str <- paste0(sprintf("[%+.1f%%]", pct), stars)
        } else {
          pct_str <- ""
        }

        mean_str <- if (nchar(pct_str) > 0) {
          sprintf("%.2f %s", s$mean, pct_str)
        } else {
          sprintf("%.2f", s$mean)
        }
      } else {
        # Control: just the mean
        mean_str <- sprintf("%.2f", s$mean)
      }

      rows[[length(rows) + 1]] <- data.frame(
        Scenario = scenario_label,
        Group    = unname(group_labels[grp]),
        Mean     = mean_str,
        SD       = sprintf("%.2f", s$sd),
        Median   = sprintf("%.0f", s$median),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Build a long-format cross-scenario price summary table
#'
#' Same structure as build_summary_table_long() but for Q4 (price):
#' - Applies 99th-percentile outlier trimming per group
#' - Reports Median (not Mean) as primary statistic
#' - Treatment groups show [% difference from control median]
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param scenarios Character vector of scenario codes
#'
#' @return Data frame: Scenario | Group | Median | SD
build_price_summary_table_long <- function(data_main, data_eu,
                                           scenarios = c("PD", "PU", "EU", "CA",
                                                         "LS", "WN", "FS")) {
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )

  group_labels <- c(
    C = "Control (C)", A = "Short (A)",
    B = "Full + Primer (B)", B1 = "Full + Primer (B1)\u2020"
  )

  rows <- list()
  for (sc in scenarios) {
    data_use <- if (sc == "EU") data_eu else data_main
    col_name <- paste0(sc, "4")
    is_eu <- sc == "EU"

    if (!col_name %in% names(data_use)) next

    # Trim outliers at 99th percentile per group
    data_trimmed <- remove_outliers_by_group(data_use, col_name, "GROUP", 0.99)

    groups <- sort(unique(data_trimmed$GROUP))
    treat_b <- if ("B1" %in% groups) "B1" else "B"

    # Stats per group
    group_stats <- list()
    for (grp in groups) {
      vals <- data_trimmed[[col_name]][data_trimmed$GROUP == grp]
      vals <- vals[!is.na(vals)]
      group_stats[[grp]] <- list(median = median(vals), sd = sd(vals), n = length(vals))
    }

    control_median <- group_stats[["C"]]$median

    # Dunn's test on trimmed data
    data_clean <- data_trimmed[!is.na(data_trimmed[[col_name]]), ]
    formula <- as.formula(paste(col_name, "~ GROUP"))
    dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")

    p_ac <- dunn$p.adj[dunn$group1 == "A" & dunn$group2 == "C"]
    p_bc <- dunn$p.adj[dunn$group1 == treat_b & dunn$group2 == "C"]
    if (length(p_ac) == 0) p_ac <- 1
    if (length(p_bc) == 0) p_bc <- 1

    scenario_label <- unname(scenario_names[sc])
    if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

    grp_order <- intersect(c("C", "A", treat_b), groups)

    for (i in seq_along(grp_order)) {
      grp <- grp_order[i]
      s <- group_stats[[grp]]

      median_base <- sprintf("$%s", formatC(s$median, format = "f",
                                            big.mark = ",", digits = 0))

      if (grp != "C" && abs(control_median) > 0.001) {
        r_treat <- round(s$median, 0)
        r_ctrl  <- round(control_median, 0)
        pct <- (r_treat - r_ctrl) / abs(r_ctrl) * 100

        p_val <- if (grp == "A") p_ac else p_bc
        stars <- if (p_val < 0.001) "\\*\\*\\*"
                 else if (p_val < 0.01) "\\*\\*"
                 else if (p_val < 0.05) "\\*"
                 else ""

        pct_str <- paste0(sprintf("[%+.1f%%]", pct), stars)

        median_str <- paste0(median_base, " ", pct_str)
      } else {
        median_str <- median_base
      }

      rows[[length(rows) + 1]] <- data.frame(
        Scenario = scenario_label,
        Group    = unname(group_labels[grp]),
        Median   = median_str,
        SD       = sprintf("$%s", formatC(s$sd, format = "f",
                                          big.mark = ",", digits = 0)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Build a long-format cross-scenario VDA table for Q1/Q2/Q3/Delta
#'
#' Matches the per-scenario VDA format from Chapters 7.X: one row per
#' scenario x comparison with probability percentages, labeled effect
#' sizes, and significance stars.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param question_suffix "1", "2", "3", or "25" (delta)
#' @param scenarios Character vector of scenario codes
#'
#' @return Data frame: Scenario | Comparison | P(1st > 2nd) | Effect Size | Sig.
build_vda_table_long <- function(data_main, data_eu, question_suffix,
                                 scenarios = c("PD", "PU", "EU", "CA",
                                               "LS", "WN", "FS")) {
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )

  rows <- list()
  for (sc in scenarios) {
    data_use <- if (sc == "EU") data_eu else data_main
    col_name <- paste0(sc, question_suffix)
    is_eu <- sc == "EU"

    if (!col_name %in% names(data_use)) next

    data_clean <- data_use[!is.na(data_use[[col_name]]), ]
    treat_b <- if ("B1" %in% unique(data_clean$GROUP)) "B1" else "B"

    formula <- as.formula(paste(col_name, "~ GROUP"))
    dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")
    vda  <- rcompanion::multiVDA(x = data_clean[[col_name]], g = data_clean$GROUP)

    scenario_label <- unname(scenario_names[sc])
    if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

    # multiVDA pair indices (alphabetical): 1 = A-B/B1, 2 = A-C, 3 = B/B1-C
    comparisons <- list(
      list(label = "C > A",                dunn_idx = 2, flip = TRUE),
      list(label = paste0("C > ", treat_b), dunn_idx = 3, flip = TRUE),
      list(label = paste0("A > ", treat_b), dunn_idx = 1, flip = FALSE)
    )

    for (comp in comparisons) {
      idx <- comp$dunn_idx
      if (idx > nrow(dunn)) next

      vda_val     <- vda$pairs$VDA[idx]
      p_val       <- dunn$p.adj[idx]
      display_vda <- if (comp$flip) 1 - vda_val else vda_val

      rows[[length(rows) + 1]] <- data.frame(
        Scenario       = scenario_label,
        Comparison     = comp$label,
        `P(1st > 2nd)` = pad_pct(display_vda),
        `Effect Size`  = classify_vda_effect(vda_val),
        Sig.           = p_stars(p_val),
        stringsAsFactors = FALSE,
        check.names    = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Build a long-format cross-scenario VDA table for Q4 (price)
#'
#' Same as build_vda_table_long() but applies 99th-percentile outlier
#' trimming per group before computing Dunn's test and VDA.
#'
#' @param data_main Main dataset (Groups A, B, C)
#' @param data_eu EU dataset (Groups A, B1, C)
#' @param scenarios Character vector of scenario codes
#'
#' @return Data frame: Scenario | Comparison | P(1st > 2nd) | Effect Size | Sig.
build_price_vda_table_long <- function(data_main, data_eu,
                                       scenarios = c("PD", "PU", "EU", "CA",
                                                     "LS", "WN", "FS")) {
  scenario_names <- c(
    PD = "Public Domain", PU = "Personal Use", EU = "Educational Use",
    CA = "Commercial Advertising", LS = "Large Scale Copying",
    WN = "Wikipedia Image Search", FS = "Filesharing"
  )

  rows <- list()
  for (sc in scenarios) {
    data_use <- if (sc == "EU") data_eu else data_main
    col_name <- paste0(sc, "4")
    is_eu <- sc == "EU"

    if (!col_name %in% names(data_use)) next

    data_trimmed <- remove_outliers_by_group(data_use, col_name, "GROUP", 0.99)
    data_clean   <- data_trimmed[!is.na(data_trimmed[[col_name]]), ]
    treat_b <- if ("B1" %in% unique(data_clean$GROUP)) "B1" else "B"

    formula <- as.formula(paste(col_name, "~ GROUP"))
    dunn <- rstatix::dunn_test(data_clean, formula, p.adjust.method = "holm")
    vda  <- rcompanion::multiVDA(x = data_clean[[col_name]], g = data_clean$GROUP)

    scenario_label <- unname(scenario_names[sc])
    if (is_eu) scenario_label <- paste0(scenario_label, "\u2020")

    comparisons <- list(
      list(label = "C > A",                dunn_idx = 2, flip = TRUE),
      list(label = paste0("C > ", treat_b), dunn_idx = 3, flip = TRUE),
      list(label = paste0("A > ", treat_b), dunn_idx = 1, flip = FALSE)
    )

    for (comp in comparisons) {
      idx <- comp$dunn_idx
      if (idx > nrow(dunn)) next

      vda_val     <- vda$pairs$VDA[idx]
      p_val       <- dunn$p.adj[idx]
      display_vda <- if (comp$flip) 1 - vda_val else vda_val

      rows[[length(rows) + 1]] <- data.frame(
        Scenario       = scenario_label,
        Comparison     = comp$label,
        `P(1st > 2nd)` = pad_pct(display_vda),
        `Effect Size`  = classify_vda_effect(vda_val),
        Sig.           = p_stars(p_val),
        stringsAsFactors = FALSE,
        check.names    = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

# =========================================================
# VDA Effect Size Helpers (for scenario plot brackets)
# =========================================================

#' Classify VDA effect size per Vargha & Delaney (2000) thresholds
#'
#' Maps a VDA value to a human-readable effect size category.
#' Distance from 0.5: >= 0.21 large, >= 0.14 moderate, >= 0.06 small.
#'
#' @param vda_value Numeric VDA value (0 to 1)
#' @return Character: "negligible", "small", "moderate", or "large"
classify_vda_effect <- function(vda_value) {
  dist <- abs(vda_value - 0.5)
  if (dist >= 0.21) return("large")
  if (dist >= 0.14) return("moderate")
  if (dist >= 0.06) return("small")
  return("negligible")
}

#' Format a kable-ready VDA pairwise comparison table
#'
#' Takes Dunn's test and multiVDA results and produces a data frame
#' showing the probability that a randomly selected observation from
#' the higher-scoring group exceeds one from the lower-scoring group.
#' VDA values are displayed as percentages for readability.
#'
#' @param dunn Data frame from rstatix::dunn_test()
#' @param vda Result from rcompanion::multiVDA()
#'
#' @return Data frame suitable for knitr::kable()
format_vda_table <- function(dunn, vda) {
  n_pairs <- nrow(dunn)

  # Identify B/B1 group from the Dunn results
  all_groups <- unique(c(dunn$group1, dunn$group2))
  treat_b <- if ("B1" %in% all_groups) "B1" else "B"

  # Fixed comparison order: C > A, C > B/B1, A > B/B1
  # Dunn's test returns alphabetical pairs:
  #   Row 1: A - B/B1  (VDA pair 1)
  #   Row 2: A - C     (VDA pair 2)
  #   Row 3: B/B1 - C  (VDA pair 3)
  comparisons <- list(
    list(label = "C > A",                dunn_idx = 2, flip = TRUE),
    list(label = paste0("C > ", treat_b), dunn_idx = 3, flip = TRUE),
    list(label = paste0("A > ", treat_b), dunn_idx = 1, flip = FALSE)
  )

  rows <- lapply(comparisons, function(comp) {
    i <- comp$dunn_idx
    if (i > n_pairs) return(NULL)

    vda_val <- vda$pairs$VDA[i]
    p_val   <- dunn$p.adj[i]

    # Fixed direction: flip VDA when comparison reverses alphabetical order
    display_vda <- if (comp$flip) 1 - vda_val else vda_val

    data.frame(
      Comparison      = comp$label,
      `P(1st > 2nd)`  = pad_pct(display_vda),
      `Effect Size`   = classify_vda_effect(vda_val),
      Sig.            = p_stars(p_val),
      check.names     = FALSE,
      stringsAsFactors = FALSE
    )
  })
  result <- dplyr::bind_rows(rows)
  result
}

# Summary: Why these functions matter
# ====================================
# These statistical helpers do the "heavy lifting" of analysis:
# - They calculate the numbers that tell us what's happening
# - They test if differences are "real" or just random noise
# - They help us understand both IF groups differ and HOW MUCH they differ
#
# Without these functions, we'd have to:
# - Calculate everything by hand (error-prone)
# - Write the same code over and over (time-consuming)
# - Risk making mistakes in our statistical procedures
