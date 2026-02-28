# plotting_themes_literate.R - Literate Programming Version
# ============================================================
# Plotting Themes and Visual Settings for Survey Analysis
# 
# This file contains functions that make our plots look professional
# and consistent. Think of this as the "style guide" for visualizations.
#
# For beginners: Good visualization is crucial for communication!
# These functions ensure all our plots have the same clean, professional look.
# ============================================================

# Style presets for scenario plots
# ==========================================
# Web (Quarto Book): lavender/purple, larger fonts for screen
# Print (CAELJ TIFF): grayscale, compact fonts for 4.6" galley width
PLOT_STYLES <- list(
  web = list(
    violin_fill    = "#E8E0F0",   # Lavender
    violin_outline = "#4A1486",   # Dark purple
    mean_dot_color = "#4A1486",   # Dark purple
    midpoint_color = "#4A1486",   # Dark purple
    arrow_color    = "#4A1486",   # Dark purple (VDA brackets)
    boxplot_fill   = "#F0F0F0",   # Light gray
    base_size = 18, title_size = 22, subtitle_size = 16, caption_size = 17,
    axis_text_x = 18, axis_text_y = 16,
    mean_dot_size = 5, label_size = 5.5, bracket_text = 5.5
  ),
  print = list(
    violin_fill    = "#D0D0D0",   # Light gray
    violin_outline = "#333333",   # Near-black
    mean_dot_color = "#333333",   # Near-black
    midpoint_color = "#333333",   # Near-black
    arrow_color    = "#333333",   # Near-black
    boxplot_fill   = "#F0F0F0",   # Light gray
    base_size = 10, title_size = 12, subtitle_size = 10, caption_size = 10,
    axis_text_x = 10, axis_text_y = 10,
    mean_dot_size = 3.5, label_size = 3.5, bracket_text = 3.5
  )
)

#' Get a named style preset
#'
#' @param style One of "web" or "print"
#' @return Named list of color and sizing parameters
get_plot_style <- function(style = "web") {
  if (!style %in% names(PLOT_STYLES)) {
    warning(sprintf("Unknown style '%s', falling back to 'web'.", style))
    style <- "web"
  }
  PLOT_STYLES[[style]]
}

# Backward-compatible globals (derived from web preset)
# 04-randomization.qmd and create_rand_plot() reference these directly
VIOLIN_FILL    <- PLOT_STYLES$web$violin_fill
VIOLIN_OUTLINE <- PLOT_STYLES$web$violin_outline
MEAN_DOT_COLOR <- PLOT_STYLES$web$mean_dot_color
MIDPOINT_COLOR <- PLOT_STYLES$web$midpoint_color

# Function 1: Get standardized theme for plots
# ==============================================
#' Get standardized ggplot theme
#' 
#' A "theme" in ggplot2 is like a template that controls how plots look:
#' - Font styles and sizes
#' - Background colors
#' - Grid lines
#' - Legend appearance
#' 
#' We use the "Tufte" theme, named after Edward Tufte, who wrote about
#' data visualization best practices. It's clean and minimalist.
#' 
#' @param base_family The font family to use (like Arial, Times, etc.)
#' @param base_size The base font size (all other sizes scale from this)
#' 
#' @return A theme object that can be applied to any ggplot
#' 
#' @examples
#' # Apply theme to a plot
#' my_plot + get_standard_theme()
get_standard_theme <- function(base_family = PLOT_FONT,  # Default font
                              base_size = 7) {  # Default size (points)
  
  # Create the Tufte theme
  # This theme removes unnecessary elements (less ink = clearer message)
  theme <- ggthemes::theme_tufte(
    base_family = base_family,  # Font for all text
    base_size = base_size  # Base size (other elements scale from this)
  ) +
  # Override Tufte defaults: force black axis text and transparent backgrounds.
  # theme_tufte() is a complete theme that overrides global theme_update()
  # settings, so transparent backgrounds must be set here explicitly.
  ggplot2::theme(
    axis.text           = ggplot2::element_text(color = "black"),
    plot.title.position = "plot",
    plot.background     = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background    = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key          = ggplot2::element_rect(fill = "transparent", colour = NA)
  )

  return(theme)
}

# Function 2: Get standard arguments for statistical plots
# =========================================================
#' Get standard argument list for ggbetweenstats plots
#' 
#' ggbetweenstats creates complex statistical plots. To keep them
#' consistent, we use the same settings every time. This function
#' stores all those settings in one place.
#' 
#' Think of this as a "recipe" for making plots look consistent.
#' 
#' @param show_points Should individual data points be visible?
#' @param base_family Font family for text
#' 
#' @return A list of arguments to pass to ggbetweenstats
get_standard_plot_args <- function(show_points = FALSE,  # Show individual responses?
                                  base_family = PLOT_FONT) {  # Font family
  
  # Step 1: Decide how to show individual data points
  if (show_points) {
    # Show points with some transparency so they don't overlap too much
    point_args <- list(
      alpha = 0.3,  # 30% opacity (70% transparent)
      position = "jitter"  # Spread points horizontally to avoid overlap
    )
  } else {
    # Make points invisible (0% opacity = 100% transparent)
    point_args <- list(alpha = 0.0)
  }
  
  # Step 2: Create the complete list of arguments
  # Each setting controls a different aspect of the plot
  args <- list(
    # Statistical settings
    type = "np",  # Non-parametric tests (don't assume normal distribution)
    mean.ci = TRUE,  # Show confidence intervals around means
    pairwise.display = "significant",  # Only show significant comparisons
    digits = 2,  # Round numbers to 2 decimal places
    p.adjust.method = "holm",  # Adjust p-values for multiple comparisons
    effsize.type = "unbiased",  # Use unbiased effect size estimates
    
    # Visual settings
    point.args = point_args,  # How to display individual points
    centrality.type = "parametric",  # Show means (not medians)
    
    # Theme and styling
    ggtheme = get_standard_theme(base_family),  # Apply our standard theme
    
    # Significance brackets (the lines connecting groups)
    ggsignif.args = list(
      textsize = 1.5,  # Size of p-value text on brackets
      family = base_family  # Font for bracket text
    ),

    # Central tendency markers (the dots showing means)
    centrality.point.args = list(
      size = 2.5,  # Size of mean dots
      color = MEAN_DOT_COLOR  # Mean dot color
    ),

    # Labels for mean values
    centrality.label.args = list(
      family = base_family,  # Font for mean labels
      size = 1.6,  # Text size for mean values
      nudge_x = 0.4,  # Move labels to the right of dots
      segment.linetype = 4,  # Dotted line to connect label to dot
      min.segment.length = 0  # Always show connecting line
    ),
    
    # Axis label
    xlab = "Treatment Groups (A & B) & Control (C)"  # Standard x-axis label
  )
  
  return(args)
}

# Function 3: Get arguments specifically for price plots
# =======================================================
#' Get standard arguments for price estimate plots
#' 
#' Price plots are special because:
#' - They often have outliers (extreme values)
#' - We show medians instead of means (more robust)
#' - We want to see individual estimates as dots
#' 
#' @param base_family Font family for text
#' 
#' @return List of arguments tailored for price plots
get_price_plot_args <- function(base_family = PLOT_FONT) {
  
  args <- list(
    # Statistical settings
    type = "np",  # Non-parametric (prices often skewed)
    bf.message = FALSE,  # Don't show Bayes Factor (too technical)
    pairwise.display = "significant",  # Show significant differences
    digits = 2,  # Round to 2 decimal places
    p.adjust.method = "holm",  # Multiple comparison correction
    effsize.type = "unbiased",  # Unbiased effect sizes
    
    # Point display - always show for prices
    point.args = list(
      alpha = 0.3,  # Semi-transparent dots
      position = "jitter"  # Spread out to see all estimates
    ),
    
    # Central tendency - use median for prices
    centrality.type = "p",  # "p" = show median (50th percentile)
    
    # Theme and styling
    ggtheme = get_standard_theme(base_family),
    
    # Bracket styling
    ggsignif.args = list(
      textsize = 1.5,
      family = base_family
    ),

    # Median markers
    centrality.point.args = list(
      size = 2.5,
      color = MEAN_DOT_COLOR  # Mean dot color
    ),

    # Median value labels
    centrality.label.args = list(
      family = base_family,
      size = 1.4,
      nudge_x = 0.4,
      segment.linetype = 4,
      min.segment.length = 0
    ),
    
    # Standard x-axis label
    xlab = "Treatment Groups (A & B) & Control (C)"
  )
  
  return(args)
}

# Function 4: Apply formatting to existing plots
# ===============================================
#' Apply standard formatting to an existing plot
#' 
#' Sometimes we create a plot and then want to adjust its appearance.
#' This function applies our standard formatting after the fact.
#' 
#' @param plot An existing ggplot object
#' @param center_caption Should the caption be centered?
#' 
#' @return The plot with formatting applied
apply_standard_formatting <- function(plot,  # The plot to modify
                                     center_caption = TRUE) {  # Center bottom text?
  
  # Center the caption if requested
  # Captions often contain statistical results that look better centered
  if (center_caption) {
    plot <- plot + 
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0.5)  # hjust = 0.5 means center
      )
  }
  
  return(plot)
}

# Function 5: Create color palette for groups
# ============================================
#' Create color palette for experimental groups
#' 
#' Colors help distinguish groups visually. We use colorblind-friendly
#' palettes so everyone can distinguish the groups.
#' 
#' Why colorblind-friendly matters:
#' - ~8% of men have some color vision deficiency
#' - Red-green colorblindness is most common
#' - Good design is accessible design
#' 
#' @param n_groups Number of groups to color
#' 
#' @return Vector of color codes
get_group_colors <- function(n_groups = 3) {  # Usually 3 groups (A, B, C)

  # Uniform dark purple for all groups — group identity is conveyed
  # by axis labels and legends, not by color differentiation
  colors <- rep("#333333", n_groups)

  return(colors)
}

# Function 6: Set global plotting options
# ========================================
#' Set global plotting options for the entire session
#' 
#' Instead of setting options for each plot individually,
#' this function sets them once for your entire R session.
#' Like setting preferences that apply to everything.
#' 
#' @param base_family Font family to use globally
#' 
#' @return NULL (changes are made to global settings)
set_global_plot_options <- function(base_family = PLOT_FONT) {
  
  # Set the default theme for all ggplot2 plots
  ggplot2::theme_set(get_standard_theme(base_family))
  
  # Set the default color palette
  options(ggplot2.discrete.colour = get_group_colors())
  
  # invisible(NULL) returns nothing but doesn't print NULL
  invisible(NULL)
}

# Function 7: Create custom axis labels
# ======================================
#' Create appropriate axis labels for different question types
#' 
#' Different questions need different axis labels.
#' This function generates the right labels automatically.
#' 
#' @param scenario_code Two-letter code (PD, PU, etc.)
#' @param question_num Question number (2.1, 2.2, etc.)
#' @param question_type Type of question (q1, q2, delta, price)
#' 
#' @return List with x and y axis labels
get_axis_labels <- function(scenario_code,  # Which scenario
                           question_num = NULL,  # Question number
                           question_type = "standard") {  # Type of question
  
  # X-axis is always the same (groups)
  x_label <- "Treatment Groups (A & B) & Control (C)"
  
  # Y-axis depends on question type
  if (question_type == "delta") {
    # Delta shows difference between questions
    y_label <- "Δ SQs 2.2-2.3"  # Δ (delta) symbol for difference
    
  } else if (question_type == "price") {
    # Price questions show dollar amounts
    y_label <- "SQ 2.4 - License Price Estimate (USD)"
    
  } else if (!is.null(question_num)) {
    # Regular questions show the question number and scale
    y_label <- paste0("SQ ", question_num, " - 7-pt Scale")
    
  } else {
    # Default if no specific type
    y_label <- "7-pt Scale"
  }
  
  # Return both labels as a list
  return(list(x = x_label, y = y_label))
}

# Function 8: Format plot titles
# ===============================
#' Format plot title based on scenario and question type
#' 
#' Titles should be informative and consistent. This function
#' creates standardized titles that clearly describe what's shown.
#' 
#' @param scenario_name Full name like "Public Domain"
#' @param question_type Type of question being analyzed
#' 
#' @return Formatted title string
format_plot_title <- function(scenario_name,  # e.g., "Public Domain"
                             question_type) {  # e.g., "q1", "delta", "price"
  
  # Each question type gets a specific title prefix
  title_templates <- list(
    q1 = "Likelihood of Legal Consequences",  # How likely to face legal issues
    q2 = "Estimate of Legal Consequences, Current Law",  # What law says
    q3 = "Estimate of Legal Consequences, Respondent Preference",  # What they prefer
    delta = "Delta of Current Law–Respondent Preference",  # The gap
    price = "Non-Exclusive License Price Estimate for Scenario Use"  # Cost estimate
  )
  
  # Get the appropriate prefix
  title_prefix <- title_templates[[question_type]]
  
  # If we don't recognize the type, use a generic prefix
  if (is.null(title_prefix)) {
    title_prefix <- "Analysis of"
  }
  
  # Combine prefix with scenario name
  # The \n creates a line break for better readability
  full_title <- paste0(
    title_prefix, 
    "\n",  # New line
    scenario_name, 
    " Scenario by Group"
  )
  
  return(full_title)
}

# Function 9: Randomization violin + box plot
# =============================================
#' Create a violin + boxplot for covariate balance checks.
#' Matches the Chapter 7 scenario style: violin fill, mean dots, omnibus caption.
#' Moved from 04-randomization.qmd inline code to shared module.
#'
#' @param data     Survey data frame (already filtered to Groups A/B/C).
#' @param dv       Dependent variable column name (e.g., "AGE").
#' @param group_var Group column name. Default "GROUP".
#' @param title    Markdown-formatted plot title.
#' @param test_type "parametric" for ANOVA or "np" for Kruskal-Wallis.
#' @param base_family Font family. Default PLOT_FONT.
#'
#' @return A ggplot2 object.
create_rand_plot <- function(data, dv, group_var = "GROUP",
                             title = NULL, test_type = "parametric",
                             base_family = PLOT_FONT) {
  # Reorder groups: C, A, B
  group_label_map <- c("C" = "Control (C)", "A" = "Short (A)",
                        "B" = "Full + Primer (B)")
  data[[group_var]] <- factor(data[[group_var]],
                              levels = c("C", "A", "B"),
                              labels = group_label_map[c("C", "A", "B")])

  # Group means
  group_stats <- data %>%
    dplyr::group_by(!!rlang::sym(group_var)) %>%
    dplyr::summarise(mean_val = mean(!!rlang::sym(dv), na.rm = TRUE),
                     .groups = "drop")

  # Omnibus test caption
  formula_obj <- as.formula(paste(dv, "~", group_var))
  if (test_type == "parametric") {
    test_result <- aov(formula_obj, data = data)
    s <- summary(test_result)
    f_val <- s[[1]][["F value"]][1]
    p_val <- s[[1]][["Pr(>F)"]][1]
    df1 <- s[[1]][["Df"]][1]; df2 <- s[[1]][["Df"]][2]
    n_total <- nrow(data)
    p_text <- if (p_val < 0.001) "< .001" else sprintf("= %.3f", p_val)
    caption_md <- sprintf(
      "ANOVA *F*(%d, %d) = %.2f, *p* %s, *n* = %d",
      df1, df2, f_val, p_text, n_total)
  } else {
    kw <- kruskal.test(formula_obj, data = data)
    kw_eff <- rstatix::kruskal_effsize(data, formula_obj)
    n_total <- sum(!is.na(data[[dv]]))
    p_text <- if (kw$p.value < 0.001) "< .001" else sprintf("= %.3f", kw$p.value)
    caption_md <- sprintf(
      "Kruskal-Wallis *H*(%d) = %.2f, *p* %s, *e*<sup>2</sup> = %.2f, *n* = %d",
      kw$parameter, kw$statistic, p_text, kw_eff$effsize, n_total)
  }

  # Build plot
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = !!rlang::sym(group_var), y = !!rlang::sym(dv)
  )) +
    ggplot2::geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#F0F0F0") +
    ggplot2::geom_violin(width = 1.4, fill = VIOLIN_FILL, color = VIOLIN_OUTLINE,
                         alpha = 0.4, linewidth = 0.5) +
    ggplot2::geom_point(data = group_stats,
      ggplot2::aes(x = !!rlang::sym(group_var), y = mean_val),
      size = 5, color = MEAN_DOT_COLOR, inherit.aes = FALSE) +
    ggplot2::geom_label(data = group_stats,
      ggplot2::aes(x = !!rlang::sym(group_var), y = mean_val,
                   label = sprintf("%.2f", mean_val)),
      size = 5.5, family = base_family, nudge_x = 0.20,
      label.size = 0.15, label.padding = ggplot2::unit(0.12, "lines"),
      fill = "white", inherit.aes = FALSE) +
    ggthemes::theme_tufte(base_family = base_family, base_size = 18) +
    ggplot2::theme(
      plot.title          = ggtext::element_markdown(size = 22,
                              margin = ggplot2::margin(b = 2)),
      plot.title.position = "plot",
      plot.caption        = ggtext::element_markdown(size = 17, hjust = 0.5),
      axis.text.x         = ggplot2::element_text(size = 18, color = "black",
                              margin = ggplot2::margin(t = -10)),
      axis.text.y         = ggplot2::element_text(size = 16, color = "black",
                              margin = ggplot2::margin(r = 0)),
      axis.title.y        = ggplot2::element_blank(),
      axis.ticks.y        = ggplot2::element_blank(),
      axis.ticks.x        = ggplot2::element_blank(),
      plot.background     = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background    = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin         = ggplot2::margin(t = 5, r = 0, b = 5, l = 10)
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.0, 0.0))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, x = NULL, caption = caption_md)

  p
}


# Summary: Why consistent plotting matters
# =========================================
# 
# These functions ensure that:
# 1. All plots look professional and consistent
# 2. Colors are accessible to color-blind readers
# 3. Text is readable and appropriately sized
# 4. Statistical information is clearly presented
# 5. The focus is on the data, not decorations
#
# Good visualization principles:
# - Remove unnecessary elements (data-ink ratio)
# - Use color purposefully, not decoratively
# - Make text large enough to read easily
# - Be consistent across all figures
# - Consider accessibility for all readers
#
# These functions implement these principles automatically!
