# _common.R
# =============================================================
# Shared setup for the CC Survey Online Appendix (Quarto Book)
# Source this file in the setup chunk of every chapter:
#   source(here::here("_common.R"))
#
# DATA ARCHITECTURE (single-file):
#   One canonical CSV: rousse-cc-survey-2024.csv (1,319 observations)
#   Contains Groups A (400), B (414), B1 (97), C (408).
#   B1 participants completed only the EU scenario.
#
#   Two analysis views are derived by filtering:
#     survey_data    — Groups A, B, C (n=1,222) — all scenarios except EU
#     survey_data_eu — Groups A, B1, C (n=905)  — EU scenario only
# =============================================================

# 1. Reproducibility seed
set.seed(08281944)

# 2. Core libraries
suppressPackageStartupMessages({
  library(here)         # Portable project-relative paths
  library(knitr)        # Table rendering and chunk options
  library(ggplot2)      # Core plotting
  library(ggstatsplot)  # Annotated statistical plots (scenario analyses)
  library(ggthemes)     # Professional plot themes
  library(ggstats)      # Likert plots (brand recognition chapter)
  library(ggtext)       # Markdown text rendering in plot elements
  library(ggsignif)     # Significance brackets for group comparisons
  library(rstatix)      # Pipe-friendly stats: Dunn's test, Kruskal-Wallis
  library(rcompanion)   # Vargha & Delaney's A effect sizes
  library(dplyr)        # Data manipulation
  library(rlang)        # Tidy evaluation / programming utilities
  library(systemfonts)  # Font resolution (ggplot2 dependency, always available)
})

# 3. Font resolution
#    Pick the first available font from the fallback sequence.
#    systemfonts queries the OS font registry directly — no import step needed.
resolve_font <- function(candidates) {
  available <- unique(systemfonts::system_fonts()$family)
  for (font in candidates) {
    if (font %in% available) return(font)
  }
  "sans"
}

PLOT_FONT <- resolve_font(c(
  "Equity A",
  "Equity OT",
  "Century Schoolbook",
  "Century",
  "Times New Roman",
  "serif"
))

# 3b. Small-caps variant for bracket labels (OpenType smcp feature)
#     Falls back to NULL if the font lacks small-caps tables.
PLOT_FONT_SC <- tryCatch({
  sc_name <- paste0(PLOT_FONT, " SC")
  systemfonts::register_variant(
    name     = sc_name,
    family   = PLOT_FONT,
    features = systemfonts::font_feature(letters = "small_caps")
  )
  sc_name
}, error = function(e) {
  message("Small-caps variant unavailable; bracket labels will use UPPERCASE fallback.")
  NULL
})

# 4. Global knitr chunk options
knitr::opts_chunk$set(
  cache      = FALSE,
  echo       = FALSE,
  warning    = FALSE,
  message    = FALSE,
  fig.width  = 8,
  fig.height = 7,
  fig.align  = "center",
  out.width  = "100%",
  dev        = "png",
  dev.args   = list(bg = "transparent")
)

# 5. Global ggplot theme: transparent backgrounds, restrained title sizes
ggplot2::theme_update(
  plot.background     = ggplot2::element_rect(fill = "transparent", colour = NA),
  panel.background    = ggplot2::element_rect(fill = "transparent", colour = NA),
  legend.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
  legend.key          = ggplot2::element_rect(fill = "transparent", colour = NA),
  plot.title          = ggplot2::element_text(size = 11),
  plot.title.position = "plot",
  plot.subtitle       = ggplot2::element_text(size = 9),
  plot.caption        = ggplot2::element_text(size = 8)
)

# 4b. Shared formatting helpers for inline R in scenario chapters
# ------------------------------------------------------------------
# These are used by the extraction chunks in each scenario .qmd file.
# .g() extracts a value from a statistics data frame by group and stat.
# fmt() / fmtp() / fmtd() format numbers for inline display.

.g <- function(df, grp, stat = "mean") {
  col <- grep(paste0("^", stat, "_"), names(df), value = TRUE)[1]
  if (is.null(col) || is.na(col)) return(NA_real_)
  df[[col]][df$GROUP == grp]
}

fmt  <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)

fmtp <- function(p) {
  if (p < .001) "< .001"
  else sprintf("%.3f", p)
}

# Format difference with significance stars for population tables
# Outputs percentage with explicit +/− sign (e.g., "+3.4%***", "−12.0%*")
# Pads numeric portion and stars for decimal-aligned tables (U+2007 figure spaces).
fmt_diff_sig <- function(ct, digits = 1) {
  pct <- ct$diff * 100
  sign_char <- ifelse(pct >= 0, "+", "\u2212")
  # Pad numeric portion to consistent width
  num_part <- formatC(sprintf(paste0("%.", digits, "f"), abs(pct)),
                      width = 4, flag = " ")  # "3.1" → " 3.1", "12.0" → "12.0"
  num_part <- gsub(" ", "\u2007", num_part)
  # Pad stars to fixed 3-char width so they don't push numbers around
  stars <- formatC(ct$sig, width = -3, flag = " ")  # left-align, pad right
  stars <- gsub(" ", "\u2007", stars)
  paste0(sign_char, num_part, "%", stars)
}

# Pad a percentage string to fixed width using Unicode figure spaces (U+2007).
# With tabular-nums, figure spaces are exactly digit-width → decimal alignment.
pad_pct <- function(x, digits = 1, width = 5) {
  # x is a proportion (0–1); convert to percentage string
  raw <- sprintf(paste0("%.", digits, "f"), x * 100)
  padded <- formatC(raw, width = width, flag = " ")
  paste0(gsub(" ", "\u2007", padded), "%")
}

# Significance stars from a p-value
p_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "n.s."
  )
}

fmtd <- function(x) {
  sprintf("$%s", formatC(x, format = "f", big.mark = ",", digits = 0))
}

# Extract all inline variables for a scenario chapter.
# Returns a named list that can be injected via list2env().
# Replaces the ~65-line manual extraction chunk in each scenario .qmd.
#
# @param results  Output of analyze_full_scenario()
# @param sc_code  Scenario code, e.g. "PD", "EU"
# @param data     Analysis data frame (survey_data or survey_data_eu)
# @param treat_b  Treatment group B code: "B" (default) or "B1" for EU
# @param prefix   Variable prefix; defaults to tolower(sc_code)
extract_scenario_inline <- function(results, sc_code, data,
                                    treat_b = "B", prefix = NULL) {
  if (is.null(prefix)) prefix <- tolower(sc_code)
  res <- results
  b_label <- paste0("Full + Primer (", treat_b, ")")

  out <- list()

  # ── Q1, Q2, Q3: means, SDs, Kruskal-Wallis ──
  for (q in c("q1", "q2", "q3")) {
    qn <- gsub("q", "", q)  # "1", "2", "3"
    stats <- res$statistics[[q]]
    out[[paste0(prefix, "_", q, "_c")]]    <- fmt(.g(stats, "Control (C)"))
    out[[paste0(prefix, "_", q, "_a")]]    <- fmt(.g(stats, "Short (A)"))
    out[[paste0(prefix, "_", q, "_b")]]    <- fmt(.g(stats, b_label))
    out[[paste0(prefix, "_", q, "_sd_c")]] <- fmt(.g(stats, "Control (C)", "sd"))
    out[[paste0(prefix, "_", q, "_sd_a")]] <- fmt(.g(stats, "Short (A)", "sd"))
    out[[paste0(prefix, "_", q, "_sd_b")]] <- fmt(.g(stats, b_label, "sd"))
    out[[paste0(prefix, "_kw_", q)]]       <- kruskal.test(
      as.formula(paste0(sc_code, qn, " ~ GROUP")), data = data
    )
  }

  # ── Delta: means, SDs, Kruskal-Wallis ──
  out[[paste0(prefix, "_delta_c")]]    <- fmt(.g(res$statistics$delta, "Control (C)"))
  out[[paste0(prefix, "_delta_a")]]    <- fmt(.g(res$statistics$delta, "Short (A)"))
  out[[paste0(prefix, "_delta_b")]]    <- fmt(.g(res$statistics$delta, b_label))
  out[[paste0(prefix, "_delta_sd_c")]] <- fmt(.g(res$statistics$delta, "Control (C)", "sd"))
  out[[paste0(prefix, "_delta_sd_a")]] <- fmt(.g(res$statistics$delta, "Short (A)", "sd"))
  out[[paste0(prefix, "_delta_sd_b")]] <- fmt(.g(res$statistics$delta, b_label, "sd"))
  out[[paste0(prefix, "_kw_delta")]]   <- kruskal.test(
    as.formula(paste0(sc_code, "25 ~ GROUP")), data = data
  )

  # ── Wilcoxon signed-rank (per-group delta tests) ──
  wilcox_ps  <- sapply(res$tests$delta, function(t) t$p.value)
  out[[paste0(prefix, "_wilcox_ps")]]      <- wilcox_ps
  out[[paste0(prefix, "_wilcox_all_sig")]] <- all(wilcox_ps < 0.05)
  out[[paste0(prefix, "_wilcox_any_sig")]] <- any(wilcox_ps < 0.05)

  # ── Delta direction helpers (standardized across all scenarios) ──
  delta_vals <- c(.g(res$statistics$delta, "Short (A)"),
                  .g(res$statistics$delta, b_label),
                  .g(res$statistics$delta, "Control (C)"))
  out[[paste0(prefix, "_delta_vals")]]    <- delta_vals
  out[[paste0(prefix, "_delta_all_pos")]] <- all(delta_vals > 0)

  # ── Q4 (Price): medians, means, Kruskal-Wallis ──
  out[[paste0(prefix, "_price_md_c")]] <- fmtd(.g(res$statistics$price, "Control (C)", "median"))
  out[[paste0(prefix, "_price_md_a")]] <- fmtd(.g(res$statistics$price, "Short (A)", "median"))
  out[[paste0(prefix, "_price_md_b")]] <- fmtd(.g(res$statistics$price, b_label, "median"))
  out[[paste0(prefix, "_price_m_c")]]  <- fmtd(.g(res$statistics$price, "Control (C)"))
  out[[paste0(prefix, "_price_m_a")]]  <- fmtd(.g(res$statistics$price, "Short (A)"))
  out[[paste0(prefix, "_price_m_b")]]  <- fmtd(.g(res$statistics$price, b_label))
  out[[paste0(prefix, "_kw_price")]]   <- kruskal.test(
    as.formula(paste0(sc_code, "4 ~ GROUP")), data = res$tests$price
  )

  # ── VDA pairwise tables ──
  out[[paste0(prefix, "_vda_q1")]]    <- format_vda_table(res$dunn$q1,    res$vda$q1)
  out[[paste0(prefix, "_vda_q2")]]    <- format_vda_table(res$dunn$q2,    res$vda$q2)
  out[[paste0(prefix, "_vda_q3")]]    <- format_vda_table(res$dunn$q3,    res$vda$q3)
  out[[paste0(prefix, "_vda_delta")]] <- format_vda_table(res$dunn$delta, res$vda$delta)
  out[[paste0(prefix, "_vda_price")]] <- format_vda_table(res$dunn$price, res$vda$price)

  out
}

# Format a Wilcoxon signed-rank results table from a scenario results object.
# Returns a data.frame ready for knitr::kable().
# Replaces the ~17-line block copy-pasted in each scenario's delta section.
#
# @param results  Output of analyze_full_scenario()
format_wilcoxon_table <- function(results) {
  if (is.null(results$tests$delta)) return(NULL)
  grp_order <- intersect(
    c("Control (C)", "Short (A)", "Full + Primer (B)", "Full + Primer (B1)"),
    names(results$tests$delta)
  )
  wilcox_rows <- lapply(grp_order, function(grp) {
    tr <- results$tests$delta[[grp]]
    data.frame(
      Group      = grp,
      V          = unname(tr$statistic),
      `Non-Zero` = ifelse(tr$p.value < 0.05, "Yes", "No"),
      Dir.       = ifelse(tr$estimate >= 0, "+", "\u2212"),
      Sig.       = p_stars(tr$p.value),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, wilcox_rows)
}

# Format a statistics table with % difference from Control in the Mean column.
# Treatment groups show "Mean (±X.X%)***" with significance stars from Dunn's test.
# Control row shows the bare mean. Works for both mean and median stats tables.
#
# @param dunn  Optional data frame from rstatix::dunn_test(). When provided,
#              treatment groups get 1--3 significance stars instead of a generic
#              footnote marker. Group codes extracted from GROUP labels (e.g.,
#              "Short (A)" -> "A") are matched against dunn$group1/group2.
fmt_stats_table <- function(stats_df, digits = 2, use_median = FALSE, dunn = NULL) {
  stat_prefix <- if (use_median) "median_" else "mean_"
  mean_col <- grep(paste0("^", stat_prefix), names(stats_df), value = TRUE)[1]
  sd_col   <- grep("^sd_",     names(stats_df), value = TRUE)[1]
  med_col  <- grep("^median_", names(stats_df), value = TRUE)[1]

  # Identify Control row by matching "Control" in the GROUP label
  is_control <- grepl("Control", stats_df$GROUP, ignore.case = TRUE)
  control_val <- stats_df[[mean_col]][is_control][1]

  # Build p-value lookup: treatment group code -> p.adj vs control
  p_lookup <- list()
  if (!is.null(dunn)) {
    for (i in seq_len(nrow(dunn))) {
      g1 <- dunn$group1[i]; g2 <- dunn$group2[i]
      if ("C" %in% c(g1, g2)) {
        treat <- if (g1 == "C") g2 else g1
        p_lookup[[treat]] <- dunn$p.adj[i]
      }
    }
  }

  mean_fmt <- sapply(seq_len(nrow(stats_df)), function(i) {
    val <- stats_df[[mean_col]][i]
    val_str <- sprintf(paste0("%.", digits, "f"), val)
    if (!is_control[i] && !is.na(control_val) && control_val != 0) {
      # Use rounded values so displayed % is consistent with displayed means
      r_val  <- round(val, digits)
      r_ctrl <- round(control_val, digits)
      pct <- (r_val - r_ctrl) / abs(r_ctrl) * 100
      sign_str <- ifelse(pct >= 0, "+", "\u2212")
      pct_base <- sprintf("(%s%.1f%%)", sign_str, abs(pct))

      # Add significance stars from Dunn's test
      if (length(p_lookup) > 0) {
        grp_code <- sub(".*\\(([^)]+)\\).*", "\\1", stats_df$GROUP[i])
        p_val <- p_lookup[[grp_code]]
        if (!is.null(p_val) && !is.na(p_val)) {
          stars <- if (p_val < 0.001) "\\*\\*\\*"
                   else if (p_val < 0.01) "\\*\\*"
                   else if (p_val < 0.05) "\\*"
                   else ""
          pct_base <- paste0(pct_base, stars)
        }
      }

      paste0(val_str, " ", pct_base)
    } else {
      val_str
    }
  })

  data.frame(
    Group       = stats_df$GROUP,
    Mean        = mean_fmt,
    `Std. Dev.` = sprintf(paste0("%.", digits, "f"), stats_df[[sd_col]]),
    Median      = sprintf("%.0f", stats_df[[med_col]]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# 5. Data path
#    Single canonical file. here::here() resolves from the project root.
data_path       <- here::here("data", "survey-data", "rousse-cc-survey-2024.csv")
figures_v2_path <- here::here("data", "figures", "v2")

# 6. Source function modules (order matters: dependencies first)
source(here::here("analysis", "functions", "data_prep.R"))
source(here::here("analysis", "functions", "stats_helpers.R"))
source(here::here("analysis", "functions", "plotting_themes.R"))
source(here::here("analysis", "functions", "analysis_functions.R"))
source(here::here("analysis", "functions", "export_helpers.R"))
source(here::here("analysis", "functions", "randomization_checks.R"))
source(here::here("analysis", "functions", "master_analysis.R"))
source(here::here("analysis", "functions", "population_comparison.R"))

# 7. Load and prepare data (single-file architecture)
#    Step 1: Load the full dataset
survey_all <- load_survey_data(data_path)

#    Step 2: Create delta variables (Q2 − Q3) for all scenarios.
#    B1 rows get NA for non-EU deltas (PD, PU, CA, LS, WN, FS columns are empty).
#    EU delta is computed for all rows that have EU2 and EU3.
survey_all <- create_all_deltas(survey_all)

#    Step 3: Derive analysis views by group filter.
#    Non-EU scenarios: Groups A, B, C only (original sample, n=1,222)
survey_data <- dplyr::filter(survey_all, GROUP %in% c("A", "B", "C"))

#    EU scenario: Groups A, B1, C (B replaced by supplemental B1 wave, n=905)
survey_data_eu <- dplyr::filter(survey_all, GROUP %in% c("A", "B1", "C"))

# 8. Data file checksum (for reproducibility chapter)
md5_data <- tryCatch(
  tools::md5sum(data_path),
  error = function(e) "unavailable"
)

# _common.R loaded successfully.
cat(sprintf(
  "Data loaded: %d total → %d (A/B/C) + %d (A/B1/C for EU)\n",
  nrow(survey_all), nrow(survey_data), nrow(survey_data_eu)
))
