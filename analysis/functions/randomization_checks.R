# randomization_checks_literate.R - Literate Programming Version
# ================================================================
# Functions for Checking Covariate Balance Across Groups
# 
# This file contains functions that check if our random assignment worked
# Think of this as "quality control" for the experiment
#
# For beginners: In experiments, we randomly assign people to groups.
# If randomization works well, groups should be similar in demographics.
# If one group has all young people and another has all old people,
# our results might be biased!
# ================================================================

# Function 1: Check continuous variables across groups
# =====================================================
#' Check if continuous variables are balanced across groups
#' 
#' Continuous variables are things we measure on a scale:
#' - Age (18, 25, 65...)
#' - Income ($30,000, $50,000...)
#' - Education years (12, 16, 20...)
#' 
#' We test if the average values are similar across groups.
#' If they're very different, we might have a problem!
#' 
#' @param data Survey data frame
#' @param covariate The variable to check (e.g., "AGE")
#' @param group_var Column with group labels
#' @param test_type "parametric" (assumes normal) or "np" (any distribution)
#' @param create_plot Should we make a visual comparison?
#' @param base_family Font for the plot
#' 
#' @return List with test results and optional plot
#' 
#' @examples
#' # Check if age is balanced across groups
#' age_check <- check_continuous_covariate(data, "AGE", test_type = "parametric")
check_continuous_covariate <- function(data,
                                      covariate,  # Variable to check
                                      group_var = "GROUP",  # Group column
                                      test_type = "parametric",  # Test type
                                      create_plot = TRUE,  # Make plot?
                                      base_family = PLOT_FONT) {  # Font
  
  # Step 1: Determine which statistical test to use
  # Parametric = assumes normal distribution (bell curve)
  # Non-parametric = works with any distribution
  gg_test_type <- ifelse(test_type == "parametric", "parametric", "np")
  
  # Step 2: Create plot if requested
  plot <- NULL  # Start with no plot
  if (create_plot) {
    
    # Create friendly labels for common variables
    covariate_labels <- list(
      AGE = "Age",
      INCOME = "Income",
      EDU = "Educational Attainment",  # Ordinal (1-7); NOT the NETSKILLS battery
      NETSKILLS = "Internet Skills"
    )
    
    # Get the friendly label or use variable name
    cov_label <- ifelse(
      covariate %in% names(covariate_labels),
      covariate_labels[[covariate]],  # Use friendly label
      covariate  # Use variable name as-is
    )
    
    # Create the comparison plot
    plot <- ggstatsplot::ggbetweenstats(
      data = data,
      x = !!rlang::sym(group_var),  # Groups on x-axis
      y = !!rlang::sym(covariate),  # Variable on y-axis
      type = gg_test_type,  # Statistical test type
      pairwise.display = "none",  # Don't show pairwise comparisons
      p.adjust.method = "holm",  # Correction method
      effsize.type = "unbiased",  # Effect size calculation
      title = paste("Analysis of", cov_label, "by Group"),  # Plot title
      ggtheme = ggthemes::theme_tufte(base_family = base_family, base_size = 15),  # Clean theme
      point.args = list(alpha = 0.0),  # Hide individual points for clarity
      centrality.point.args = list(size = 5, color = "#FFD700"),  # Gold mean indicator
      centrality.label.args = list(size = 3, color = "#4A1486", fill = "white", alpha = 0.9)
    ) +
      ggplot2::labs(caption = NULL)  # Remove bottom-right sample info
    
    # The plot shows:
    # - Box plots for each group's distribution
    # - Mean or median values
    # - Statistical test result at top
    # - p > 0.05 means groups are similar (good!)
  }
  
  # Step 3: Run the appropriate statistical test
  if (test_type == "parametric") {
    # ANOVA (Analysis of Variance)
    # Tests if means are different across groups
    # Assumes data is normally distributed
    formula <- as.formula(paste(covariate, "~", group_var))
    test_result <- aov(formula, data = data)
    test_summary <- summary(test_result)
    
    message(paste("ANOVA for", covariate, ": F-statistic =", 
                 round(test_summary[[1]]$`F value`[1], 3)))
    
  } else {
    # Kruskal-Wallis test
    # Tests if distributions are different across groups
    # Doesn't assume normal distribution (safer for surveys)
    formula <- as.formula(paste(covariate, "~", group_var))
    test_result <- kruskal.test(formula, data = data)
    test_summary <- test_result
    
    message(paste("Kruskal-Wallis for", covariate, ": H =", 
                 round(test_result$statistic, 3)))
  }
  
  # Step 4: Interpret the results
  p_value <- ifelse(test_type == "parametric",
                   test_summary[[1]]$`Pr(>F)`[1],
                   test_result$p.value)
  
  if (p_value > 0.05) {
    message(paste("  ✓ Groups are balanced for", covariate, 
                 "(p =", round(p_value, 3), ")"))
  } else {
    message(paste("  ⚠ Groups differ for", covariate, 
                 "(p =", round(p_value, 3), ")"))
  }
  
  # Return everything
  return(list(
    plot = plot,  # Visual comparison
    test = test_result,  # Full test object
    summary = test_summary,  # Test summary
    covariate = covariate,  # Which variable
    test_type = test_type,  # Which test used
    balanced = p_value > 0.05  # Are groups balanced?
  ))
}

# Function 2: Check categorical variables across groups
# ======================================================
#' Check if categorical variables are balanced across groups
#' 
#' Categorical variables have distinct categories:
#' - Gender (Male, Female, Other)
#' - Race (White, Black, Asian, etc.)
#' - Education level (High School, College, Graduate)
#' 
#' We use chi-square test to see if the distribution of
#' categories is similar across groups.
#' 
#' @param data Survey data frame
#' @param covariate Categorical variable to check
#' @param group_var Column with group labels
#' @param create_table Should we create a contingency table?
#' 
#' @return Chi-square test results and optional table
check_categorical_covariate <- function(data,
                                       covariate,  # Variable to check
                                       group_var = "GROUP",  # Group column
                                       create_table = TRUE) {  # Make table?
  
  # Step 1: Create contingency table
  # This shows how many people in each category for each group
  # Example:
  #         Group A  Group B  Group C
  # Male        45       48       43
  # Female      55       52       57
  
  cont_table <- table(data[[covariate]], data[[group_var]])
  
  # Step 2: Run chi-square test
  # This tests if the distribution is independent of group
  # Null hypothesis: Distribution is the same across groups
  test_result <- chisq.test(cont_table)
  
  # Step 3: Interpret results
  if (test_result$p.value > 0.05) {
    message(paste("✓", covariate, "is balanced across groups",
                 "(χ² =", round(test_result$statistic, 2),
                 ", p =", round(test_result$p.value, 3), ")"))
  } else {
    message(paste("⚠", covariate, "differs across groups",
                 "(χ² =", round(test_result$statistic, 2),
                 ", p =", round(test_result$p.value, 3), ")"))
  }
  
  # Step 4: Prepare results
  result <- list(
    test = test_result,  # Chi-square test
    covariate = covariate,  # Variable name
    balanced = test_result$p.value > 0.05  # Is it balanced?
  )
  
  # Step 5: Add tables if requested
  if (create_table) {
    # Raw counts
    result$table <- cont_table
    
    # Proportions within each group (more interpretable)
    # margin = 2 means calculate proportions by column (group)
    result$proportions <- prop.table(cont_table, margin = 2)
    
    message("Proportions by group:")
    print(round(result$proportions, 3))
  }
  
  return(result)
}

# Function 3: Run all randomization checks at once
# =================================================
#' Run complete randomization check for all variables
#' 
#' This master function checks all your demographic variables
#' at once and creates a comprehensive report.
#' 
#' @param data Survey data frame
#' @param continuous_vars Vector of continuous variable names
#' @param categorical_vars Vector of categorical variable names
#' @param group_var Column with group labels
#' @param create_plots Should we create plots for continuous variables?
#' 
#' @return List with all test results organized by type
run_all_randomization_checks <- function(data,
                                        continuous_vars = c("AGE", "INCOME", "EDU"),
                                        categorical_vars = c("RACE", "GENDER"),
                                        group_var = "GROUP",
                                        create_plots = TRUE) {
  
  message(strrep("=", 50))
  message("RANDOMIZATION CHECK REPORT")
  message(strrep("=", 50), "\n")
  
  # Initialize results storage
  results <- list(
    continuous = list(),  # For continuous variables
    categorical = list()  # For categorical variables
  )
  
  # Step 1: Check continuous variables
  message("Checking Continuous Variables:")
  message(strrep("-", 30))
  
  for (var in continuous_vars) {
    if (var %in% names(data)) {
      # Determine appropriate test
      # Use parametric for age/income (often normal)
      # Use non-parametric for scales (often not normal)
      test_type <- ifelse(var %in% c("AGE", "INCOME"), "parametric", "np")
      
      # Run the check
      results$continuous[[var]] <- check_continuous_covariate(
        data = data,
        covariate = var,
        group_var = group_var,
        test_type = test_type,
        create_plot = create_plots
      )
    } else {
      warning(paste("Variable", var, "not found in data"))
    }
  }
  
  # Step 2: Check categorical variables
  message("\nChecking Categorical Variables:")
  message(strrep("-", 30))
  
  for (var in categorical_vars) {
    if (var %in% names(data)) {
      results$categorical[[var]] <- check_categorical_covariate(
        data = data,
        covariate = var,
        group_var = group_var
      )
    } else {
      warning(paste("Variable", var, "not found in data"))
    }
  }
  
  # Step 3: Overall summary
  message("\n", strrep("=", 50))
  message("RANDOMIZATION SUMMARY")
  message(strrep("=", 50))
  
  # Count balanced variables
  continuous_balanced <- sum(sapply(results$continuous, function(x) x$balanced))
  categorical_balanced <- sum(sapply(results$categorical, function(x) x$balanced))
  
  total_checked <- length(results$continuous) + length(results$categorical)
  total_balanced <- continuous_balanced + categorical_balanced
  
  message(paste("Variables checked:", total_checked))
  message(paste("Variables balanced:", total_balanced))
  message(paste("Balance rate:", round(100 * total_balanced/total_checked, 1), "%"))
  
  if (total_balanced == total_checked) {
    message("\n✓ EXCELLENT: All variables are balanced!")
    message("  Randomization appears successful.")
  } else {
    message("\n⚠ NOTE: Some variables show imbalance.")
    message("  Consider controlling for these in analysis.")
  }
  
  return(results)
}

# Function 4: Create summary table of all checks
# ===============================================
#' Create summary table of randomization check results
#' 
#' This function takes all the test results and creates
#' a neat summary table that's easy to read and share.
#' 
#' @param check_results Results from run_all_randomization_checks
#' 
#' @return Data frame with summary of all tests
summarize_randomization_checks <- function(check_results) {
  
  # Initialize list to store summary rows
  summary_rows <- list()
  
  # Step 1: Summarize continuous variables
  for (var in names(check_results$continuous)) {
    result <- check_results$continuous[[var]]
    
    # Extract p-value based on test type
    if (result$test_type == "parametric") {
      # ANOVA p-value is in the summary
      p_value <- result$summary[[1]][["Pr(>F)"]][1]
      test_name <- "ANOVA"
    } else {
      # Kruskal-Wallis p-value is in the test object
      p_value <- result$test$p.value
      test_name <- "Kruskal-Wallis"
    }
    
    # Create summary row
    summary_rows[[var]] <- data.frame(
      Variable = var,
      Type = "Continuous",
      Test = test_name,
      P_Value = round(p_value, 4),
      Significant = p_value < 0.05,
      Interpretation = ifelse(p_value < 0.05, 
                             "Groups differ", 
                             "Groups similar"),
      stringsAsFactors = FALSE
    )
  }
  
  # Step 2: Summarize categorical variables
  for (var in names(check_results$categorical)) {
    result <- check_results$categorical[[var]]
    
    # Create summary row
    summary_rows[[paste0(var, "_cat")]] <- data.frame(
      Variable = var,
      Type = "Categorical",
      Test = "Chi-square",
      P_Value = round(result$test$p.value, 4),
      Significant = result$test$p.value < 0.05,
      Interpretation = ifelse(result$test$p.value < 0.05,
                             "Groups differ",
                             "Groups similar"),
      stringsAsFactors = FALSE
    )
  }
  
  # Step 3: Combine all rows into one table
  summary_df <- dplyr::bind_rows(summary_rows)
  
  # Add visual indicators for quick reading
  summary_df$Status <- ifelse(
    summary_df$Significant,
    "⚠ Imbalanced",  # Warning symbol
    "✓ Balanced"  # Check mark
  )
  
  return(summary_df)
}

# Function 5: Create grid of plots
# =================================
#' Create grid layout of all randomization plots
#' 
#' This function arranges all the randomization check plots
#' into a single grid for easy comparison and reporting.
#' 
#' @param check_results Results from run_all_randomization_checks
#' @param ncol Number of columns in the grid
#' 
#' @return Combined plot or list of plots
plot_randomization_grid <- function(check_results, 
                                   ncol = 2) {  # 2 columns by default
  
  # Step 1: Extract all plots
  plots <- list()
  
  for (var in names(check_results$continuous)) {
    if (!is.null(check_results$continuous[[var]]$plot)) {
      plots[[var]] <- check_results$continuous[[var]]$plot
    }
  }
  
  # Check if we have any plots
  if (length(plots) == 0) {
    warning("No plots to display")
    return(NULL)
  }
  
  message(paste("Arranging", length(plots), "plots in grid..."))
  
  # Step 2: Try to combine plots
  # We need either patchwork or gridExtra package
  if (requireNamespace("patchwork", quietly = TRUE)) {
    # Use patchwork (preferred - better layout control)
    combined <- patchwork::wrap_plots(plots, ncol = ncol)
    message("Created grid using patchwork package")
    
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    # Use gridExtra as fallback
    combined <- gridExtra::grid.arrange(grobs = plots, ncol = ncol)
    message("Created grid using gridExtra package")
    
  } else {
    # No grid package available
    warning("Install 'patchwork' or 'gridExtra' package for grid plots")
    message("Returning list of individual plots instead")
    return(plots)
  }
  
  return(combined)
}

# Function 6: Check brand recognition
# ====================================
#' Analyze brand recognition patterns
#' 
#' Brand recognition helps us understand if people know
#' Creative Commons and can distinguish real from fake brands.
#' 
#' We included "Byteyield" as a fake brand to check attention.
#' If it's recognized as much as Microsoft, something's wrong!
#' 
#' @param data Survey data frame
#' @param brand_cols Vector of brand recognition column names
#' @param create_plot Should we create a likert scale plot?
#' 
#' @return Analysis results with summary and optional plot
check_brand_recognition <- function(data,
                                   brand_cols = c("BRANDSKILLS_1", "BRANDSKILLS_2", 
                                                 "BRANDSKILLS_3", "BRANDSKILLS_4"),
                                   create_plot = TRUE) {
  
  message("Analyzing brand recognition patterns...")
  
  # Step 1: Define what each column represents
  brand_labels <- c(
    "BRANDSKILLS_1" = "Creative Commons",  # The brand we're studying
    "BRANDSKILLS_2" = "Byteyield",  # Fake brand (attention check)
    "BRANDSKILLS_3" = "Microsoft",  # Well-known brand (baseline)
    "BRANDSKILLS_4" = "Mozilla"  # Real but less known brand
  )
  
  # Step 2: Calculate summary statistics for each brand
  summary_stats <- list()
  
  for (col in brand_cols) {
    if (col %in% names(data)) {
      # Calculate statistics
      summary_stats[[brand_labels[col]]] <- data.frame(
        Brand = brand_labels[col],
        Mean = mean(data[[col]], na.rm = TRUE),
        SD = sd(data[[col]], na.rm = TRUE),
        Median = median(data[[col]], na.rm = TRUE),
        Min = min(data[[col]], na.rm = TRUE),
        Max = max(data[[col]], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Interpret recognition level
      mean_recognition <- mean(data[[col]], na.rm = TRUE)
      if (mean_recognition < 2) {
        recognition_level <- "Very Low"
      } else if (mean_recognition < 3) {
        recognition_level <- "Low"
      } else if (mean_recognition < 4) {
        recognition_level <- "Moderate"
      } else {
        recognition_level <- "High"
      }
      
      summary_stats[[brand_labels[col]]]$Recognition_Level <- recognition_level
    }
  }
  
  # Step 3: Combine into summary table
  summary_df <- dplyr::bind_rows(summary_stats)
  
  # Step 4: Check for attention (fake brand should be low)
  byteyield_mean <- summary_df$Mean[summary_df$Brand == "Byteyield"]
  if (byteyield_mean > 2.5) {
    message("⚠ WARNING: Fake brand (Byteyield) has high recognition.")
    message("  This might indicate inattentive responses.")
  } else {
    message("✓ Good: Fake brand has appropriately low recognition.")
  }
  
  # Step 5: Create plot if requested
  plot <- NULL
  if (create_plot && requireNamespace("ggstats", quietly = TRUE)) {
    # Prepare data for likert plot
    brand_data <- data
    
    # Apply labels for better plot
    for (col in names(brand_labels)) {
      if (col %in% names(brand_data)) {
        attr(brand_data[[col]], "label") <- brand_labels[[col]]
      }
    }
    
    # Create likert plot
    plot <- ggstats::gglikert(
      brand_data,
      include = all_of(brand_cols)
    )
    
    message("Created brand recognition likert plot")
  }
  
  # Return results
  return(list(
    summary = summary_df,  # Summary statistics
    plot = plot,  # Likert plot (if created)
    attention_check = byteyield_mean < 2.5  # Did attention check pass?
  ))
}

# Function 7: Extract p-value from a randomization check result
# ==============================================================
#' Extract p-value from either ANOVA or Kruskal-Wallis result.
#' Moved from 04-randomization.qmd inline helpers.
#'
#' @param result Output of check_continuous_covariate().
#' @return Numeric p-value.
rand_p <- function(result) {
  if (result$test_type == "parametric") {
    summary(result$test)[[1]][["Pr(>F)"]][1]
  } else {
    result$test$p.value
  }
}

#' Extract test statistic from a randomization check result.
#' @param result Output of check_continuous_covariate().
#' @return Numeric test statistic (F or H).
rand_stat <- function(result) {
  if (result$test_type == "parametric") {
    summary(result$test)[[1]][["F value"]][1]
  } else {
    as.numeric(result$test$statistic)
  }
}

#' Extract degrees of freedom from a randomization check result.
#' @param result Output of check_continuous_covariate().
#' @return Numeric df.
rand_df <- function(result) {
  if (result$test_type == "parametric") {
    summary(result$test)[[1]][["Df"]][1]
  } else {
    result$test$parameter
  }
}

#' Return human-readable test name for a randomization check result.
#' @param result Output of check_continuous_covariate().
#' @return Character: "ANOVA" or "Kruskal-Wallis".
rand_test_label <- function(result) {
  if (result$test_type == "parametric") "ANOVA" else "Kruskal-Wallis"
}


# Summary: Why randomization checks matter
# =========================================
# 
# Good randomization is the foundation of valid experiments!
# 
# If groups aren't balanced:
# - Differences might be due to demographics, not treatment
# - Results could be biased
# - Conclusions might be wrong
# 
# These functions help you:
# 1. Check if randomization worked
# 2. Identify problematic variables
# 3. Document balance for publications
# 4. Decide if statistical controls are needed
# 
# Remember: Perfect balance is rare, but major imbalances
# need to be addressed in your analysis!
