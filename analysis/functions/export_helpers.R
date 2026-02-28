# export_helpers_literate.R - Literate Programming Version
# ==========================================================
# Export Utilities for Plots and Tables
# 
# This file contains functions that save your analysis results
# Think of these as the "save" and "export" functions that preserve your work
#
# For beginners: Saving your work properly is crucial!
# - You need high-quality images for publications
# - Tables need to be formatted for reports
# - Results should be saved for future use
# ==========================================================

# Function 1: Export plot to TIFF file
# =====================================
#' Export plot to TIFF file for publication
#' 
#' TIFF (Tagged Image File Format) is preferred for publications because:
#' - It's uncompressed (no quality loss)
#' - It supports high resolution (300+ DPI)
#' - It's accepted by most journals
#' - It preserves all details
#' 
#' DPI = Dots Per Inch (resolution)
#' - Screen: 72-96 DPI
#' - Print: 300+ DPI
#' 
#' @param plot The ggplot object to save
#' @param filename Name for the output file
#' @param width Width in inches (journals often specify this)
#' @param height Height in inches
#' @param res Resolution in DPI (300 is standard for print)
#' @param path Directory to save in (NULL = current directory)
#' 
#' @return NULL (creates file as side effect)
#' 
#' @examples
#' # Save a plot for publication
#' export_plot_to_tiff(my_plot, "figure1.tiff", width=6.5, height=4)
export_plot_to_tiff <- function(plot,  # The plot to save
                               filename,  # What to call the file
                               width = 4.6,  # Width in inches (CAELJ body column)
                               height = 3.5,  # Height in inches
                               res = 300,  # Resolution (DPI)
                               path = NULL) {  # Where to save (NULL = data/figures/v2/)
  
  # Step 1: Construct the full file path
  # Default output directory: data/figures/v2/ (relative to project root)
  if (is.null(path) && requireNamespace("here", quietly = TRUE)) {
    path <- here::here("data", "figures", "v2")
  }
  
  if (!is.null(path)) {
    # Ensure the output directory exists
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message(paste("Created output directory:", path))
    }
    # Combine path and filename
    filepath <- file.path(path, filename)
  } else {
    # Fallback: save in current directory
    filepath <- filename
  }
  
  # Step 2: Open TIFF graphics device
  # This tells R to send graphics to a file instead of screen
  tiff(
    filepath,  # Where to save
    units = "in",  # Specify dimensions in inches
    width = width,  # Plot width
    height = height,  # Plot height
    res = res,  # Resolution in DPI
    type = "cairo"  # Cairo renderer: heavier strokes, better anti-aliasing
  )
  
  # Step 3: Print the plot to the file
  # print() is needed for ggplot objects
  print(plot)
  
  # Step 4: Close the graphics device
  # IMPORTANT: Always close the device or file may be corrupted
  dev.off()
  
  # Step 5: Confirm the file was created
  message(paste("Plot exported to:", filepath))
  message(paste("  Size:", width, "x", height, "inches"))
  message(paste("  Resolution:", res, "DPI"))
  
  # Return nothing (invisible prevents NULL from printing)
  invisible(NULL)
}

# Function 2: Export results table
# =================================
#' Export results table using kable for nice formatting
#' 
#' kable (from the knitr package) creates nicely formatted tables
#' that can be used in documents, reports, and presentations.
#' 
#' Format options:
#' - "markdown": For RMarkdown documents
#' - "html": For web pages
#' - "latex": For academic papers
#' - "pipe": For plain text
#' 
#' @param stats_df Data frame with statistics to export
#' @param format Output format for the table
#' @param caption Title/caption for the table
#' @param digits Number of decimal places to show
#' 
#' @return Formatted table object
export_results_table <- function(stats_df,  # Data frame to format
                                format = "markdown",  # Output format
                                caption = NULL,  # Table title
                                digits = 3) {  # Decimal places
  
  # Step 1: Round numeric columns to specified digits
  # This makes tables cleaner and easier to read
  numeric_cols <- sapply(stats_df, is.numeric)  # Find numeric columns
  stats_df[numeric_cols] <- round(stats_df[numeric_cols], digits)  # Round them
  
  # Step 2: Create formatted table using kable
  table <- knitr::kable(
    stats_df,  # The data
    format = format,  # Output format
    caption = caption,  # Table title
    digits = digits  # Decimal places
  )
  
  # Tell user what we created
  message(paste("Created", format, "table with", nrow(stats_df), "rows"))
  
  return(table)
}

# Function 3: Export multiple plots at once
# ==========================================
#' Export multiple plots to TIFF files in batch
#' 
#' When you have many plots to save, this function saves them all
#' at once with consistent settings. Saves time and ensures consistency.
#' 
#' @param plots Named list of plots (names become filenames)
#' @param prefix Text to add before each filename
#' @param width Width for all plots
#' @param height Height for all plots
#' @param res Resolution for all plots
#' 
#' @return NULL (creates multiple files)
#' 
#' @examples
#' # Save all plots from a scenario
#' plots <- list(q1 = plot1, q2 = plot2, q3 = plot3)
#' export_plots_batch(plots, prefix = "PD_")
#' # Creates: PD_q1.tiff, PD_q2.tiff, PD_q3.tiff
export_plots_batch <- function(plots,  # List of plots
                              prefix = "",  # Filename prefix
                              width = 6.5,  # Width for all
                              height = 4,  # Height for all
                              res = 300) {  # Resolution for all
  
  # Tell user we're starting batch export
  message(paste("Exporting", length(plots), "plots..."))
  
  # Loop through each plot in the list
  for (name in names(plots)) {
    # Create filename: prefix + name + .tiff
    filename <- paste0(prefix, name, ".tiff")
    
    # Export this plot
    export_plot_to_tiff(
      plots[[name]],  # The plot
      filename,  # Its filename
      width,  # Dimensions
      height,
      res
    )
  }
  
  message("Batch export complete!")
  
  invisible(NULL)
}

# Function 4: Save analysis results to RDS file
# ==============================================
#' Save analysis results to RDS file for later use
#' 
#' RDS (R Data Serialization) files preserve R objects exactly:
#' - Keeps all data types intact
#' - Preserves structure and attributes
#' - Can be loaded back into R later
#' - More efficient than CSV for complex objects
#' 
#' Use RDS when you want to:
#' - Save your work and continue later
#' - Share exact results with colleagues
#' - Create reproducible analyses
#' 
#' @param results Any R object(s) to save
#' @param filename Name for the RDS file
#' @param path Directory to save in
#' 
#' @return NULL (creates file)
save_analysis_results <- function(results,  # What to save
                                 filename,  # Filename
                                 path = NULL) {  # Where to save
  
  # Construct full path if directory provided
  if (!is.null(path)) {
    filepath <- file.path(path, filename)
  } else {
    filepath <- filename
  }
  
  # Save the object to RDS file
  # RDS preserves everything about the object
  saveRDS(results, filepath)
  
  # Report what we saved
  message(paste("Results saved to:", filepath))
  message(paste("  File size:", 
                round(file.size(filepath) / 1024, 1), "KB"))
  
  # To load later, use:
  message("To reload: results <- readRDS('", filepath, "')")
  
  invisible(NULL)
}

# Function 5: Export statistics to CSV
# =====================================
#' Export summary statistics to CSV file
#' 
#' CSV (Comma-Separated Values) files are useful because:
#' - They can be opened in Excel
#' - They're human-readable
#' - They're universal (any program can read them)
#' - They're good for sharing with non-R users
#' 
#' @param stats_list List of statistics data frames
#' @param filename Output filename
#' @param path Output directory
#' 
#' @return NULL (creates CSV file)
export_stats_to_csv <- function(stats_list,  # List of statistics
                               filename = "summary_statistics.csv",
                               path = NULL) {
  
  # Step 1: Combine all statistics with scenario labels
  # We'll stack all the statistics tables together
  all_stats <- list()
  
  message("Combining statistics from", length(stats_list), "scenarios...")
  
  for (scenario in names(stats_list)) {
    # Get statistics for this scenario
    stats_df <- stats_list[[scenario]]
    
    # Add a column showing which scenario
    stats_df$Scenario <- scenario
    
    # Store in our list
    all_stats[[scenario]] <- stats_df
  }
  
  # Step 2: Combine all into one big table
  combined_stats <- dplyr::bind_rows(all_stats)
  
  # Step 3: Construct file path
  if (!is.null(path)) {
    filepath <- file.path(path, filename)
  } else {
    filepath <- filename
  }
  
  # Step 4: Write to CSV file
  # row.names = FALSE prevents R from adding row numbers
  write.csv(combined_stats, filepath, row.names = FALSE)
  
  # Report what we created
  message(paste("Statistics exported to:", filepath))
  message(paste("  Total rows:", nrow(combined_stats)))
  message(paste("  Total columns:", ncol(combined_stats)))
  
  invisible(NULL)
}

# Function 6: Create output directory structure
# ==============================================
#' Create organized folder structure for outputs
#' 
#' Good organization prevents losing files and makes sharing easier.
#' This function creates a standard folder structure for your results.
#' 
#' Typical structure:
#' output/
#' ├── plots/     (for figures)
#' ├── tables/    (for statistics)
#' └── data/      (for processed data)
#' 
#' @param base_path Main output directory
#' @param subdirs Vector of subdirectory names to create
#' 
#' @return NULL (creates directories)
create_output_structure <- function(base_path,  # Main folder
                                   subdirs = c("plots", "tables", "data")) {
  
  # Step 1: Create base directory if it doesn't exist
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
    message(paste("Created base directory:", base_path))
  } else {
    message(paste("Base directory exists:", base_path))
  }
  
  # Step 2: Create each subdirectory
  for (subdir in subdirs) {
    # Combine base path with subdirectory name
    subdir_path <- file.path(base_path, subdir)
    
    # Create if it doesn't exist
    if (!dir.exists(subdir_path)) {
      dir.create(subdir_path)
      message(paste("  Created subdirectory:", subdir))
    } else {
      message(paste("  Subdirectory exists:", subdir))
    }
  }
  
  message("Output structure ready!")
  
  invisible(NULL)
}

# Function 7: Generate standardized filename
# ===========================================
#' Generate filename based on scenario and question
#' 
#' Consistent naming helps you find files later.
#' This function creates standardized names automatically.
#' 
#' Naming convention:
#' [SCENARIO][QUESTION][WIDTH].tiff
#' Example: PD165.tiff = Public Domain Q1, 6.5 inches wide
#' 
#' @param scenario_code Two-letter scenario code
#' @param question Question identifier
#' @param width Plot width (used in filename)
#' @param extension File extension
#' 
#' @return Generated filename string
generate_filename <- function(scenario_code,  # e.g., "PD"
                            question,  # e.g., "1" or "25"
                            width = 6.5,  # Plot width
                            extension = ".tiff") {  # File type
  
  # Step 1: Create base name from scenario and question
  # Remove any non-numeric characters from question
  base_name <- toupper(paste0(
    scenario_code,  # Scenario code
    gsub("\\D", "", question)  # Question number only
  ))
  
  # Step 2: Add width indicator
  # Convert width to integer (6.5 becomes 65)
  width_indicator <- as.integer(width * 10)
  
  # Step 3: Construct complete filename
  filename <- paste0(base_name, width_indicator, extension)
  
  return(filename)
}

# Function 8: Export analysis report to HTML
# ===========================================
#' Export complete analysis report as HTML
#' 
#' HTML reports are great for sharing because:
#' - They can be viewed in any web browser
#' - They're self-contained (include plots)
#' - They're interactive (can have clickable elements)
#' - They look professional
#' 
#' @param results Complete analysis results object
#' @param template_file Path to RMarkdown template (optional)
#' @param output_file Output filename
#' 
#' @return NULL (creates HTML file)
export_html_report <- function(results,  # Analysis results
                              template_file = NULL,  # RMD template
                              output_file = "analysis_report.html") {
  
  # Check if template provided
  if (is.null(template_file)) {
    # No template - just save results as RDS instead
    warning("HTML export requires a template file.")
    warning("Saving results as RDS instead...")
    
    # Save as RDS with similar name
    saveRDS(results, gsub("\\.html$", ".rds", output_file))
    
    message("Results saved as RDS file")
    message("To create HTML report, provide an RMarkdown template")
    
    return(invisible(NULL))
  }
  
  # If template provided, render it
  message("Rendering HTML report...")
  message("This may take a few minutes...")
  
  # Render the RMarkdown document to HTML
  rmarkdown::render(
    template_file,  # The RMD template
    output_file = output_file,  # Output name
    params = list(results = results)  # Pass results to template
  )
  
  message(paste("Report exported to:", output_file))
  message("Open in any web browser to view")
  
  invisible(NULL)
}

# Function 9: Create a summary export of all results
# ===================================================
#' Create a comprehensive export of all analysis results
#' 
#' This function exports everything in organized folders:
#' - All plots as TIFF files
#' - All statistics as CSV files
#' - Complete results as RDS
#' - Summary report as HTML (if template provided)
#' 
#' @param results Complete analysis results
#' @param output_dir Base directory for all exports
#' @param export_plots Should plots be exported?
#' @param export_tables Should tables be exported?
#' 
#' @return NULL (creates multiple files)
export_all_results <- function(results,  # All analysis results
                              output_dir = "output",  # Where to save
                              export_plots = TRUE,  # Export plots?
                              export_tables = TRUE) {  # Export tables?
  
  message("="*50)
  message("COMPREHENSIVE EXPORT STARTING")
  message("="*50)
  
  # Step 1: Create directory structure
  create_output_structure(output_dir)
  
  # Step 2: Export plots if requested
  if (export_plots && "plots" %in% names(results)) {
    message("\nExporting plots...")
    plot_dir <- file.path(output_dir, "plots")
    
    for (scenario in names(results$plots)) {
      export_plots_batch(
        results$plots[[scenario]],
        prefix = paste0(scenario, "_"),
        width = 6.5,
        height = 4
      )
    }
  }
  
  # Step 3: Export tables if requested
  if (export_tables && "statistics" %in% names(results)) {
    message("\nExporting statistics...")
    table_dir <- file.path(output_dir, "tables")
    
    export_stats_to_csv(
      results$statistics,
      filename = "all_statistics.csv",
      path = table_dir
    )
  }
  
  # Step 4: Save complete results as RDS
  message("\nSaving complete results...")
  save_analysis_results(
    results,
    filename = "complete_results.rds",
    path = file.path(output_dir, "data")
  )
  
  message("\n", "="*50)
  message("EXPORT COMPLETE!")
  message("Results saved in:", output_dir)
  message("="*50)
  
  invisible(NULL)
}

# Summary: Why proper export matters
# ===================================
# 
# Good export practices ensure:
# 1. Your work is preserved and reproducible
# 2. Results can be shared with others
# 3. Figures meet publication standards
# 4. Data is organized and findable
# 5. Analysis can be continued later
#
# File format guide:
# - TIFF: Publication-quality images
# - CSV: Universal data format
# - RDS: Preserve R objects exactly
# - HTML: Shareable reports
#
# These functions handle all the technical details
# so you can focus on your analysis!
