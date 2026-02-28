# master_analysis.R
# ===========================================================
# High-Level Wrapper Functions for Complete Scenario Analysis
# 
# This file contains the "master" functions that orchestrate everything.
# They coordinate all the other functions to create a complete analysis —
# instead of running 20 functions separately, one master function does
# everything.
#
# NOTE: This file does NOT source dependencies itself. The _common.R
# setup file at the project root handles all library and function loading.
# If you need to use master_analysis.R standalone, source the other
# function files first:
#   source(here::here("analysis", "functions", "data_prep.R"))
#   source(here::here("analysis", "functions", "stats_helpers.R"))
#   source(here::here("analysis", "functions", "plotting_themes.R"))
#   source(here::here("analysis", "functions", "analysis_functions.R"))
#   source(here::here("analysis", "functions", "export_helpers.R"))
# ===========================================================

# Function 1: Analyze all questions for one scenario
# ===================================================
#' Run complete analysis for all questions in a scenario
#' 
#' A "scenario" is one situation we tested (like Public Domain).
#' Each scenario has 5 questions (Q1, Q2, Q3, Delta, Price).
#' This function analyzes all of them at once!
#' 
#' Think of this as: "Analyze everything about Public Domain"
#' 
#' @param data Survey data frame
#' @param scenario_code Two-letter code (PD, PU, EU, CA, LS, WN, FS)
#' @param scenario_name Full name like "Public Domain"
#' @param questions Which questions to analyze (default = all)
#' @param export_tiffs Save plots as TIFF files?
#' @param data_b1 Special dataset for EU scenario (has B1 group)
#' 
#' @return List with all plots, statistics, and test results
#' 
#' @examples
#' # Analyze everything about Public Domain
#' pd_results <- analyze_full_scenario(data, "PD", "Public Domain")
analyze_full_scenario <- function(data,
                                 scenario_code,  # e.g., "PD"
                                 scenario_name,  # e.g., "Public Domain"
                                 questions = c("q1", "q2", "q3", "delta", "price"),
                                 export_tiffs = TRUE,
                                 data_b1 = NULL) {
  
  # Special handling for Educational Use scenario
  # EU uses a different dataset with B1 instead of B group
  if (scenario_code == "EU" && !is.null(data_b1)) {
    message("Using B1 dataset for Educational Use scenario")
    data <- data_b1
  }
  
  # Tell user what we're doing
  message(strrep("=", 50))
  message(paste("ANALYZING", scenario_name, "SCENARIO"))
  message(strrep("=", 50))
  
  # Initialize storage for results
  # Think of this as setting up folders to organize everything
  results <- list(
    plots = list(),       # Store all plots
    statistics = list(),  # Store all statistics tables
    tests = list(),       # Store all test results
    vda = list(),         # Store VDA effect sizes per question
    dunn = list(),        # Store Dunn's test results per question
    scenario_code = scenario_code,
    scenario_name = scenario_name
  )
  
  # Define how variable names map to question types
  # Each scenario has variables like PD1, PD2, PD3, PD4, PD25
  question_map <- list(
    q1 = paste0(scenario_code, "1"),     # e.g., "PD1"
    q2 = paste0(scenario_code, "2"),     # e.g., "PD2"
    q3 = paste0(scenario_code, "3"),     # e.g., "PD3"
    delta = paste0(scenario_code, "25"), # e.g., "PD25"
    price = paste0(scenario_code, "4")   # e.g., "PD4"
  )
  
  # Define question numbers for axis labels
  question_nums <- list(
    q1 = "2.1",     # Survey question 2.1
    q2 = "2.2",     # Survey question 2.2
    q3 = "2.3",     # Survey question 2.3
    delta = NULL,   # Delta doesn't have a question number
    price = "2.4"   # Survey question 2.4
  )
  
  # Analyze each question type
  for (q_type in questions) {
    
    message("\n", strrep("-", 30))
    message(paste("Analyzing", toupper(q_type), "..."))
    
    # Get the column name for this question
    col_name <- question_map[[q_type]]
    
    # Check if this column exists in the data
    if (!col_name %in% names(data)) {
      warning(paste("Column", col_name, "not found. Skipping", q_type))
      next  # Skip to next question
    }
    
    # Run the analysis for this question
    analysis <- analyze_scenario_question(
      data = data,
      dv = col_name,                        # Variable to analyze
      question_type = q_type,                # Type of question
      scenario_name = scenario_name,         # For plot title
      question_num = question_nums[[q_type]], # For axis label
      export_tiff = export_tiffs              # Save to file?
    )
    
    # Store the results
    results$plots[[q_type]] <- analysis$plot
    results$statistics[[q_type]] <- analysis$statistics
    results$vda[[q_type]]  <- analysis$vda
    results$dunn[[q_type]] <- analysis$dunn

    # Store additional results (wilcox tests for delta, filtered data for price)
    if (!is.null(analysis$additional_results)) {
      results$tests[[q_type]] <- analysis$additional_results
    }
    
    message(paste("✓", toupper(q_type), "analysis complete"))
  }
  
  # Create an overall summary
  message("\nCreating scenario summary...")
  results$summary <- create_scenario_summary(results$statistics)
  
  message("\n", strrep("=", 50))
  message(paste(scenario_name, "ANALYSIS COMPLETE!"))
  message(strrep("=", 50))
  
  return(results)
}

# Function 2: Analyze all scenarios in the survey
# ================================================
#' Analyze all seven scenarios at once
#' 
#' Instead of analyzing each scenario separately, this function
#' analyzes all of them in one go. It's like pressing a button
#' that runs the entire analysis!
#' 
#' @param main_data Main survey data
#' @param b1_data Special data for EU scenario
#' @param scenarios Which scenarios to analyze (default = all)
#' @param export_tiffs Save plots as TIFF files?
#' 
#' @return List with results for all scenarios
analyze_all_scenarios <- function(main_data,
                                 b1_data = NULL,
                                 scenarios = c("PD", "PU", "EU", "CA", "LS", "WN", "FS"),
                                 export_tiffs = TRUE) {
  
  # Define full names for each scenario code
  scenario_names <- list(
    PD = "Public Domain",           # No copyright
    PU = "Personal Use",            # Personal, non-commercial
    EU = "Educational Use",         # Teaching and learning
    CA = "Commercial Advertising",  # Business use
    LS = "Large Scale Copying",     # Mass distribution
    WN = "Wikipedia Image Search",  # Online images
    FS = "Filesharing"             # Peer-to-peer sharing
  )
  
  message("\n", strrep("#", 60))
  message("STARTING COMPLETE SURVEY ANALYSIS")
  message("Scenarios to analyze:", paste(scenarios, collapse = ", "))
  message(strrep("#", 60), "\n")
  
  # Initialize storage for all results
  all_results <- list()
  
  # Process each scenario
  for (scenario in scenarios) {
    
    message("\n", strrep("~", 50))
    message(paste("Scenario", which(scenarios == scenario), "of", length(scenarios)))
    
    # Determine which dataset to use
    # EU scenario needs the special B1 dataset
    if (scenario == "EU" && !is.null(b1_data)) {
      data_to_use <- b1_data
    } else {
      data_to_use <- main_data
    }
    
    # Run the complete analysis for this scenario
    all_results[[scenario]] <- analyze_full_scenario(
      data = data_to_use,
      scenario_code = scenario,
      scenario_name = scenario_names[[scenario]],
      export_tiffs = export_tiffs
    )
  }
  
  # Add cross-scenario comparisons
  message("\n", strrep("~", 50))
  message("Creating cross-scenario comparisons...")
  all_results$comparisons <- compare_across_scenarios(all_results)
  
  message("\n", strrep("#", 60))
  message("ALL SCENARIOS ANALYZED SUCCESSFULLY!")
  message(strrep("#", 60))
  
  return(all_results)
}

# Function 3: Create summary of scenario statistics
# ==================================================
#' Combine statistics from all questions into one summary
#' 
#' This function takes the statistics from each question
#' and combines them into one easy-to-read table.
#' 
#' @param stats_list List of statistics for each question
#' 
#' @return Combined summary data frame
create_scenario_summary <- function(stats_list) {
  
  # Combine all statistics with question labels
  summary_rows <- list()
  
  for (q_type in names(stats_list)) {
    stats <- stats_list[[q_type]]
    
    if (!is.null(stats)) {
      # Add a column showing which question
      stats$Question <- q_type
      
      # Store in our list
      summary_rows[[q_type]] <- stats
    }
  }
  
  # Combine all rows if we have any
  if (length(summary_rows) > 0) {
    summary_df <- dplyr::bind_rows(summary_rows)
    
    # Reorder columns for better readability
    # Put Question first, then GROUP, then statistics
    col_order <- c("Question", "GROUP", 
                  setdiff(names(summary_df), c("Question", "GROUP")))
    summary_df <- summary_df[, col_order]
    
    return(summary_df)
  } else {
    return(NULL)
  }
}

# Function 4: Compare results across scenarios
# =============================================
#' Compare how responses differ across all scenarios
#' 
#' This function creates a comparison table showing how
#' each group responded to Q1 across all scenarios.
#' Helps identify which scenarios are seen as most/least risky.
#' 
#' @param results_list List of results from all scenarios
#' 
#' @return Comparison data frame
compare_across_scenarios <- function(results_list) {
  
  message("Creating cross-scenario comparison table...")
  
  # Extract key statistics for comparison
  comparison_data <- list()
  
  for (scenario in names(results_list)) {
    # Skip the comparisons entry itself
    if (scenario != "comparisons") {
      scenario_results <- results_list[[scenario]]
      
      # Extract Q1 statistics (likelihood of legal consequences)
      if ("q1" %in% names(scenario_results$statistics)) {
        q1_stats <- scenario_results$statistics$q1
        
        # Find the column with mean values
        # It will be named like mean_PD1, mean_PU1, etc.
        mean_col <- names(q1_stats)[grep("^mean_", names(q1_stats))][1]
        
        if (!is.null(mean_col) && length(mean_col) > 0) {
          # Extract means for each group
          mean_a <- q1_stats[[mean_col]][q1_stats$GROUP == "A"]
          mean_b <- q1_stats[[mean_col]][q1_stats$GROUP == "B"]
          
          # For EU scenario, might be B1 instead of B
          if (length(mean_b) == 0) {
            mean_b <- q1_stats[[mean_col]][q1_stats$GROUP == "B1"]
          }
          
          mean_c <- q1_stats[[mean_col]][q1_stats$GROUP == "C"]
          
          # Only create comparison if we found the groups
          if (length(mean_a) > 0 && length(mean_c) > 0) {
            comparison_data[[scenario]] <- data.frame(
              Scenario = scenario,
              Question = "Q1 (Legal Risk)",
              Mean_A = round(mean_a, 2),
              Mean_B = ifelse(length(mean_b) > 0, round(mean_b, 2), NA),
              Mean_C = round(mean_c, 2),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  
  # Combine all comparisons if we have any
  if (length(comparison_data) > 0) {
    comparison_df <- dplyr::bind_rows(comparison_data)
    
    # Add interpretation column
    comparison_df$Interpretation <- apply(comparison_df[, c("Mean_A", "Mean_B", "Mean_C")], 
                                         1, function(x) {
      means <- na.omit(x)
      if (max(means) > 5) {
        "High perceived risk"
      } else if (max(means) > 3) {
        "Moderate perceived risk"
      } else {
        "Low perceived risk"
      }
    })
    
    message("Comparison table created with", nrow(comparison_df), "scenarios")
    return(comparison_df)
  } else {
    message("No comparison data available")
    return(NULL)
  }
}

# Function 5: Generate complete analysis report
# ==============================================
#' Run the entire analysis pipeline and save everything
#' 
#' This is the "do everything" function. It:
#' 1. Loads data
#' 2. Checks randomization
#' 3. Analyzes all scenarios
#' 4. Saves all results
#' 
#' @param data_paths List with paths to data files
#' @param output_dir Directory for saving results
#' @param scenarios Which scenarios to analyze
#' 
#' @return Complete analysis results
generate_full_report <- function(data_paths,
                                output_dir = "output",
                                scenarios = c("PD", "PU", "EU", "CA", "LS", "WN", "FS")) {
  
  message("\n", strrep("★", 60))
  message("COMPREHENSIVE ANALYSIS PIPELINE")
  message(strrep("★", 60), "\n")
  
  # Step 1: Create output directory structure
  message("Step 1: Setting up output directories...")
  create_output_structure(output_dir)
  
  # Step 2: Load and prepare data
  message("\nStep 2: Loading and preparing data...")
  data <- prepare_all_data(
    main_filepath = data_paths$main,
    b1_filepath = data_paths$b1,
    create_deltas = TRUE
  )
  
  # Step 3: Run randomization checks
  message("\nStep 3: Checking randomization...")
  randomization_results <- run_all_randomization_checks(
    data = data$main,
    create_plots = TRUE
  )
  
  # Step 4: Analyze all scenarios
  message("\nStep 4: Analyzing all scenarios...")
  scenario_results <- analyze_all_scenarios(
    main_data = data$main,
    b1_data = data$b1,
    scenarios = scenarios,
    export_tiffs = TRUE
  )
  
  # Step 5: Compile full results
  message("\nStep 5: Compiling results...")
  full_results <- list(
    data = data,
    randomization = randomization_results,
    scenarios = scenario_results,
    timestamp = Sys.time(),
    session_info = sessionInfo()
  )
  
  # Step 6: Save everything
  message("\nStep 6: Saving results...")
  save_analysis_results(
    full_results,
    filename = "complete_analysis_results.rds",
    path = output_dir
  )
  
  # Also export summary statistics
  export_scenario_summaries(scenario_results, output_dir)
  
  message("\n", strrep("★", 60))
  message("ANALYSIS COMPLETE!")
  message("Results saved to:", output_dir)
  message(strrep("★", 60))
  
  return(full_results)
}

# Function 6: Export summary statistics for all scenarios
# ========================================================
#' Export all scenario statistics to CSV files
#' 
#' This function takes the statistics from all scenarios
#' and saves them as CSV files that can be opened in Excel.
#' 
#' @param scenario_results Results from analyze_all_scenarios
#' @param output_dir Where to save the files
#' 
#' @return NULL (creates files)
export_scenario_summaries <- function(scenario_results, 
                                    output_dir) {
  
  message("Exporting scenario summaries...")
  
  # Collect all statistics
  all_stats <- list()
  
  for (scenario in names(scenario_results)) {
    if (scenario != "comparisons" && !is.null(scenario_results[[scenario]]$summary)) {
      all_stats[[scenario]] <- scenario_results[[scenario]]$summary
    }
  }
  
  # Export to CSV if we have statistics
  if (length(all_stats) > 0) {
    export_stats_to_csv(
      all_stats,
      filename = "all_scenario_statistics.csv",
      path = file.path(output_dir, "tables")
    )
    
    message("✓ Statistics exported to CSV")
  }
  
  invisible(NULL)
}

# Function 7: Quick analysis for testing
# =======================================
#' Quickly analyze one question from one scenario
#' 
#' This function is perfect for testing or when you just
#' need to analyze one specific thing quickly.
#' 
#' @param data Data frame
#' @param scenario_code Scenario code (PD, PU, etc.)
#' @param question Question type (q1, q2, q3, delta, price)
#' 
#' @return Analysis results
#' 
#' @examples
#' # Quick analysis of Public Domain Q1
#' result <- quick_analyze(data, "PD", "q1")
quick_analyze <- function(data, 
                         scenario_code, 
                         question = "q1") {
  
  # Scenario names for titles
  scenario_names <- list(
    PD = "Public Domain",
    PU = "Personal Use",
    EU = "Educational Use",
    CA = "Commercial Advertising",
    LS = "Large Scale Copying",
    WN = "Wikipedia Image Search",
    FS = "Filesharing"
  )
  
  # Map question to column name
  question_map <- list(
    q1 = paste0(scenario_code, "1"),
    q2 = paste0(scenario_code, "2"),
    q3 = paste0(scenario_code, "3"),
    delta = paste0(scenario_code, "25"),
    price = paste0(scenario_code, "4")
  )
  
  message(paste("Quick analysis:", scenario_code, "-", question))
  
  # Run the analysis
  result <- analyze_scenario_question(
    data = data,
    dv = question_map[[question]],
    question_type = question,
    scenario_name = scenario_names[[scenario_code]],
    question_num = ifelse(question %in% c("q1", "q2", "q3"), 
                         gsub("q", "2.", question), NULL),
    export_tiff = FALSE  # Don't save file for quick analysis
  )
  
  message("Quick analysis complete!")
  
  return(result)
}

# Summary: How these master functions work together
# ==================================================
# 
# The hierarchy of functions:
# 
# 1. generate_full_report()       [Runs everything]
#    ↓
# 2. analyze_all_scenarios()      [All scenarios]
#    ↓
# 3. analyze_full_scenario()      [One scenario]
#    ↓
# 4. analyze_scenario_question()  [One question]
#    ↓
# 5. Helper functions             [Individual tasks]
#
# You can enter at any level depending on what you need:
# - Full analysis? Use generate_full_report()
# - All scenarios? Use analyze_all_scenarios()
# - One scenario? Use analyze_full_scenario()
# - One question? Use quick_analyze()
#
# This modular design makes the code:
# - Flexible (use what you need)
# - Maintainable (fix issues in one place)
# - Understandable (each function has one job)
# - Reusable (functions can be used in other projects)
