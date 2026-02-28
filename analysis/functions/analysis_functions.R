# analysis_functions.R — Scenario Plot Redesign (v2)
# =========================================================
# Core functions for analyzing survey scenario questions.
# Replaces ggstatsplot::ggbetweenstats() with custom ggplot2 +
# ggtext composition for cleaner, more controllable
# visualizations. Significance shown via horizontal dominance
# arrows (non-dominant → dominant group) with VDA effect labels.
#
# Previous version archived at:
#   analysis/functions/prev/analysis_functions_pre-redesign.R
#
# Author: Thomas H. Rousse
# =========================================================

# ─── Helper: add horizontal dominance arrows ─────────────────
#' Add dominance arrows with VDA effect-size labels
#'
#' For each significant pairwise comparison (p < 0.05), draws a
#' horizontal arrow from the non-dominant group (lower mean) toward
#' the dominant group (higher mean). The arrowhead indicates which
#' group scored higher. An effect-size label sits at the midpoint.
#' Line thickness scales with effect magnitude.
#'
#' Arrow stacking order: narrowest span at bottom, widest on top
#' (A-B < C-A < C-B).
#'
#' @param p A ggplot object to add arrows to
#' @param dunn Data frame from rstatix::dunn_test()
#' @param vda Result from rcompanion::multiVDA()
#' @param y_start Starting y position for the lowest arrow
#' @param y_step Vertical spacing between stacked arrows
#' @param base_family Font family for label text
#'
#' @return The ggplot object with arrow + label annotation layers
add_vda_brackets <- function(p, dunn, vda, y_start, y_step,
                             base_family = PLOT_FONT,
                             style = get_plot_style("web")) {
  sig_idx <- which(dunn$p.adj < 0.05)

  if (length(sig_idx) == 0) return(p)

  # ── Build comparison pairs, effect-size labels, and raw VDA values ──
  comparisons <- lapply(sig_idx, function(i) {
    c(as.character(dunn$group1[i]), as.character(dunn$group2[i]))
  })

  annotations <- vapply(sig_idx, function(i) {
    classify_vda_effect(vda$pairs$VDA[i])
  }, character(1))

  vda_values <- vapply(sig_idx, function(i) {
    vda$pairs$VDA[i]
  }, numeric(1))

  # ── Reorder: narrowest at bottom, widest (C-B) on top ──
  canonical_order <- list(
    c("Short (A)", "Full + Primer (B)"),
    c("Short (A)", "Full + Primer (B1)"),
    c("Control (C)", "Short (A)"),
    c("Control (C)", "Full + Primer (B1)"),
    c("Control (C)", "Full + Primer (B)")
  )

  pair_key <- function(comp) paste(sort(comp), collapse = "|")
  canon_keys <- vapply(canonical_order, pair_key, character(1))
  comp_keys  <- vapply(comparisons, pair_key, character(1))

  matched   <- na.omit(match(canon_keys, comp_keys))
  unmatched <- setdiff(seq_along(comparisons), matched)
  ordered   <- c(matched, unmatched)

  comparisons <- comparisons[ordered]
  annotations <- annotations[ordered]
  vda_values  <- vda_values[ordered]

  y_positions <- seq(y_start, by = y_step, length.out = length(comparisons))

  # ── "Effect Size" label to the left of arrow region ──
  if (length(comparisons) > 0) {
    label_y <- mean(range(y_positions))
    p <- p + ggplot2::annotate(
      "text",
      x     = 0.65,
      y     = label_y,
      label = "Effect Size",
      family = base_family,
      fontface = "italic",
      size   = style$bracket_text,
      color  = "black",
      hjust  = 0.5
    )
  }

  # ── Map effect size to arrow line thickness ──
  effect_lwd <- vapply(annotations, function(eff) {
    switch(eff,
      "large"    = 0.5,
      "moderate" = 0.4,
      0.3  # small / negligible
    )
  }, numeric(1))

  # ── Map effect size to arrowhead size ──
  arrow_size <- vapply(annotations, function(eff) {
    switch(eff,
      "large"    = 0.08,
      "moderate" = 0.06,
      0.04  # small / negligible
    )
  }, numeric(1))

  # ── Compute group means from plot data for dominance direction ──
  x_var    <- rlang::as_name(p$mapping$x)
  y_var    <- rlang::as_name(p$mapping$y)
  x_levels <- levels(p$data[[x_var]])
  group_means <- tapply(p$data[[y_var]], p$data[[x_var]], mean, na.rm = TRUE)

  for (i in seq_along(comparisons)) {
    g1 <- comparisons[[i]][1]
    g2 <- comparisons[[i]][2]
    x1  <- match(g1, x_levels)
    x2  <- match(g2, x_levels)
    x_mid <- (x1 + x2) / 2

    # Dominant group = higher mean score. Arrow points FROM dominant TOWARD non-dominant.
    dominant_x    <- if (group_means[g1] > group_means[g2]) x1 else x2
    nondominant_x <- if (dominant_x == x1) x2 else x1

    # Horizontal arrow: dominant → non-dominant
    p <- p + ggplot2::annotate(
      "segment",
      x        = dominant_x,
      xend     = nondominant_x,
      y        = y_positions[i],
      yend     = y_positions[i],
      arrow    = grid::arrow(length = grid::unit(arrow_size[i], "inches"),
                             angle  = 25,
                             type   = "closed"),
      color    = style$arrow_color,
      linewidth = effect_lwd[i]
    )

    # Effect-size label at midpoint (white box occludes arrow line)
    p <- p + ggplot2::annotate(
      "label",
      x             = x_mid,
      y             = y_positions[i],
      label         = annotations[i],
      family        = base_family,
      fontface      = "plain",
      size          = style$bracket_text,
      fill          = "white",
      label.size    = 0.3,
      label.padding = ggplot2::unit(0.15, "lines"),
      vjust         = 0.5
    )
  }

  p
}

# ─── Helper: Kruskal-Wallis markdown subtitle ────────────────
#' Build a markdown-formatted subtitle with KW test results
#'
#' @param data Data frame
#' @param dv Dependent variable name
#' @param group_var Grouping variable name
#'
#' @return Character string with markdown italics for p and n
build_kw_subtitle <- function(data, dv, group_var) {
  formula_obj <- as.formula(paste(dv, "~", group_var))
  kw     <- kruskal.test(formula_obj, data = data)
  kw_eff <- rstatix::kruskal_effsize(data, formula_obj)

  n_total <- sum(!is.na(data[[dv]]))
  p_text  <- if (kw$p.value < 0.001) "< .001" else sprintf("= %.3f", kw$p.value)

  sprintf(
    "Kruskal-Wallis *H*(%d) = %.2f, *p* %s, *e*<sup>2</sup> = %.2f, *n* = %d",
    kw$parameter, kw$statistic, p_text, kw_eff$effsize, n_total
  )
}


# ─── Function 1: create_scenario_plot (Q1, Q2, Q3) ──────────
#' Create simplified violin plot for ordinal survey questions
#'
#' Replaces ggstatsplot::ggbetweenstats() with a custom ggplot2
#' composition: violin + boxplot + mean dots + VDA brackets.
#'
#' @param data Survey data frame
#' @param dv Dependent variable column name (e.g., "PD1")
#' @param group_var Grouping column (default "GROUP")
#' @param title Plot title
#' @param show_points Show jittered individual points?
#' @param base_family Font family
#'
#' @return List with plot, statistics, vda, dunn
create_scenario_plot <- function(data,
                                dv,
                                group_var = "GROUP",
                                title = NULL,
                                subtitle = NULL,
                                show_points = FALSE,
                                base_family = PLOT_FONT,
                                style = get_plot_style("web")) {

  # ── Reorder groups: C, A, B (Control first) ──
  actual_groups <- unique(data[[group_var]][!is.na(data[[group_var]])])
  desired_order <- c("C", "A", "B", "B1")
  factor_levels <- desired_order[desired_order %in% actual_groups]
  group_label_map <- c(
    "C" = "Control (C)", "A" = "Short (A)",
    "B" = "Full + Primer (B)", "B1" = "Full + Primer (B1)"
  )
  data[[group_var]] <- factor(
    data[[group_var]],
    levels = factor_levels,
    labels = group_label_map[factor_levels]
  )

  # ── Validate ──
  validation <- validate_group_data(data, dv, group_var)
  message(validation$message)

  if (!validation$is_valid || nrow(validation$filtered_data) < 3) {
    warning("Insufficient data for statistical testing.")
    stats <- calculate_descriptive_stats(data, dv, group_var)
    return(list(
      plot = NULL, statistics = stats, vda = NULL, dunn = NULL,
      validation_failed = TRUE, message = validation$message
    ))
  }

  data <- validation$filtered_data

  # ── Statistical tests ──
  formula_obj <- as.formula(paste(dv, "~", group_var))
  dunn <- rstatix::dunn_test(data, formula_obj, p.adjust.method = "holm")
  vda  <- rcompanion::multiVDA(x = data[[dv]], g = data[[group_var]])

  # ── Group means ──
  group_stats <- data %>%
    dplyr::group_by(!!rlang::sym(group_var)) %>%
    dplyr::summarise(
      mean_val = mean(!!rlang::sym(dv), na.rm = TRUE),
      .groups = "drop"
    )
  # ── Caption (stats only — no centrality legend) ──
  caption_md <- build_kw_subtitle(data, dv, group_var)

  # ── Build plot ──
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = !!rlang::sym(group_var),
    y = !!rlang::sym(dv)
  )) +
    ggplot2::geom_hline(
      yintercept = 4, linetype = "dashed",
      color = "#CCCCCC"
    ) +
    ggplot2::geom_boxplot(
      width = 0.15, outlier.shape = NA, fill = style$boxplot_fill
    ) +
    ggplot2::geom_violin(
      width = 1.4,
      fill = style$violin_fill, color = style$violin_outline,
      alpha = 0.4, linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = group_stats,
      ggplot2::aes(x = !!rlang::sym(group_var), y = mean_val),
      size = style$mean_dot_size, color = style$mean_dot_color, inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = group_stats,
      ggplot2::aes(
        x = !!rlang::sym(group_var), y = mean_val,
        label = sprintf("%.2f", mean_val)
      ),
      size = style$label_size, family = base_family,
      nudge_x = 0.20,
      label.size = 0.15, label.padding = ggplot2::unit(0.12, "lines"),
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(1, 3, 5, 7),
      expand = ggplot2::expansion(add = c(1.0, 0.3))
    ) +
    ggthemes::theme_tufte(base_family = base_family, base_size = style$base_size) +
    ggplot2::theme(
      plot.title          = ggtext::element_markdown(size = style$title_size, margin = ggplot2::margin(b = 2)),
      plot.title.position = "plot",
      plot.subtitle       = ggplot2::element_text(size = style$subtitle_size, margin = ggplot2::margin(t = 2, b = 2)),
      plot.caption        = ggtext::element_markdown(size = style$caption_size, hjust = 0.5),
      axis.text.x         = ggplot2::element_text(size = style$axis_text_x, color = "black",
                              margin = ggplot2::margin(t = -5)),
      axis.text.y         = ggplot2::element_text(size = style$axis_text_y, color = "black",
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
    ggplot2::labs(
      title = title, subtitle = subtitle, x = NULL,
      caption = caption_md
    )

  # Add jittered points if requested
  if (show_points) {
    p <- p + ggplot2::geom_jitter(width = 0.1, alpha = 0.3, size = 0.8)
  }

  # ── VDA brackets (below plot, between data and group labels) ──
  y_min <- min(data[[dv]], na.rm = TRUE)
  p <- add_vda_brackets(p, dunn, vda,
                        y_start = y_min - 0.50, y_step = -0.42,
                        base_family = base_family, style = style)

  # ── Descriptive statistics ──
  stats <- calculate_descriptive_stats(data, dv, group_var)

  return(list(plot = p, statistics = stats, vda = vda, dunn = dunn))
}


# ─── Function 2: create_delta_plot ───────────────────────────
#' Create violin plot for delta questions (Q2 - Q3)
#'
#' Same structure as create_scenario_plot but with midpoint at 0
#' and per-group Wilcoxon signed-rank tests against zero.
#'
#' @param data Survey data frame
#' @param dv Delta variable (e.g., "PD25")
#' @param title Plot title
#' @param test_by_group Run Wilcoxon tests per group?
#' @param base_family Font family
#'
#' @return List with plot, wilcox_tests, statistics, vda, dunn
create_delta_plot <- function(data,
                              dv,
                              title = NULL,
                              subtitle = NULL,
                              test_by_group = TRUE,
                              base_family = PLOT_FONT,
                              style = get_plot_style("web")) {

  # ── Reorder groups ──
  actual_groups <- unique(data$GROUP[!is.na(data$GROUP)])
  desired_order <- c("C", "A", "B", "B1")
  factor_levels <- desired_order[desired_order %in% actual_groups]
  group_label_map <- c(
    "C" = "Control (C)", "A" = "Short (A)",
    "B" = "Full + Primer (B)", "B1" = "Full + Primer (B1)"
  )
  data$GROUP <- factor(data$GROUP, levels = factor_levels,
                       labels = group_label_map[factor_levels])

  # ── Validate ──
  validation <- validate_group_data(data, dv, "GROUP")
  message(validation$message)

  if (!validation$is_valid || nrow(validation$filtered_data) < 3) {
    warning("Insufficient data for delta testing.")
    stats <- calculate_descriptive_stats(data, dv, "GROUP")
    return(list(
      plot = NULL, wilcox_tests = NULL, statistics = stats,
      vda = NULL, dunn = NULL, validation_failed = TRUE
    ))
  }

  data <- validation$filtered_data

  # ── Statistical tests ──
  formula_obj <- as.formula(paste(dv, "~ GROUP"))
  dunn <- rstatix::dunn_test(data, formula_obj, p.adjust.method = "holm")
  vda  <- rcompanion::multiVDA(x = data[[dv]], g = data[["GROUP"]])

  # ── Group means ──
  group_stats <- data %>%
    dplyr::group_by(GROUP) %>%
    dplyr::summarise(
      mean_val = mean(!!rlang::sym(dv), na.rm = TRUE),
      .groups = "drop"
    )
  # ── Caption (stats only — no centrality legend) ──
  caption_md <- build_kw_subtitle(data, dv, "GROUP")

  # ── Build plot — midpoint at 0 ──
  p <- ggplot2::ggplot(data, ggplot2::aes(x = GROUP, y = !!rlang::sym(dv))) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed",
      color = "#CCCCCC"
    ) +
    ggplot2::geom_boxplot(
      width = 0.15, outlier.shape = NA, fill = style$boxplot_fill
    ) +
    ggplot2::geom_violin(
      width = 1.4,
      fill = style$violin_fill, color = style$violin_outline,
      alpha = 0.4, linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = group_stats,
      ggplot2::aes(x = GROUP, y = mean_val),
      size = style$mean_dot_size, color = style$mean_dot_color, inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = group_stats,
      ggplot2::aes(
        x = GROUP, y = mean_val,
        label = sprintf("%.2f", mean_val)
      ),
      size = style$label_size, family = base_family,
      nudge_x = 0.20,
      label.size = 0.15, label.padding = ggplot2::unit(0.12, "lines"),
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = c(1.0, 0.3))
    ) +
    ggthemes::theme_tufte(base_family = base_family, base_size = style$base_size) +
    ggplot2::theme(
      plot.title          = ggtext::element_markdown(size = style$title_size, margin = ggplot2::margin(b = 2)),
      plot.title.position = "plot",
      plot.subtitle       = ggplot2::element_text(size = style$subtitle_size, margin = ggplot2::margin(t = 2, b = 2)),
      plot.caption        = ggtext::element_markdown(size = style$caption_size, hjust = 0.5),
      axis.text.x         = ggplot2::element_text(size = style$axis_text_x, color = "black",
                              margin = ggplot2::margin(t = -5)),
      axis.text.y         = ggplot2::element_text(size = style$axis_text_y, color = "black",
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
    ggplot2::labs(
      title = title, subtitle = subtitle, x = NULL,
      caption = caption_md
    )

  # ── VDA brackets (below plot, between data and group labels) ──
  y_min <- min(data[[dv]], na.rm = TRUE)
  p <- add_vda_brackets(p, dunn, vda,
                        y_start = y_min - 0.50, y_step = -0.38,
                        base_family = base_family, style = style)

  # ── Wilcoxon signed-rank tests per group ──
  wilcox_results <- NULL
  if (test_by_group) {
    groups <- unique(data$GROUP)
    wilcox_results <- list()
    for (grp in groups) {
      grp_data <- dplyr::filter(data, GROUP == grp)
      if (nrow(grp_data) > 0 && sum(!is.na(grp_data[[dv]])) > 0) {
        wilcox_results[[as.character(grp)]] <- wilcox.test(
          grp_data[[dv]], conf.int = TRUE, mu = 0
        )
      }
    }
  }

  # ── Descriptive statistics ──
  stats <- calculate_descriptive_stats(data, dv, "GROUP")

  return(list(
    plot = p, wilcox_tests = wilcox_results, statistics = stats,
    vda = vda, dunn = dunn
  ))
}


# ─── Function 3: create_price_plot ───────────────────────────
#' Create violin plot for price questions (Q4)
#'
#' Removes outliers (top 1%), uses median for centrality,
#' dollar-formatted y-axis, no midpoint line.
#'
#' @param data Survey data frame
#' @param dv Price variable (e.g., "PD4")
#' @param title Plot title
#' @param percentile Outlier removal threshold (default 0.99)
#' @param show_points Show jittered individual points?
#' @param alpha Point transparency (0-1)
#' @param base_family Font family
#'
#' @return List with plot, filtered_data, statistics, vda, dunn
create_price_plot <- function(data,
                              dv,
                              title = NULL,
                              subtitle = NULL,
                              percentile = 0.99,
                              show_points = TRUE,
                              alpha = 0.3,
                              base_family = PLOT_FONT,
                              style = get_plot_style("web")) {

  # ── Reorder groups ──
  actual_groups <- unique(data$GROUP[!is.na(data$GROUP)])
  desired_order <- c("C", "A", "B", "B1")
  factor_levels <- desired_order[desired_order %in% actual_groups]
  group_label_map <- c(
    "C" = "Control (C)", "A" = "Short (A)",
    "B" = "Full + Primer (B)", "B1" = "Full + Primer (B1)"
  )
  data$GROUP <- factor(data$GROUP, levels = factor_levels,
                       labels = group_label_map[factor_levels])

  # ── Remove outliers ──
  filtered_data <- remove_outliers_by_group(data, dv, "GROUP", percentile)

  # ── Validate ──
  validation <- validate_group_data(filtered_data, dv, "GROUP")
  message(validation$message)

  if (!validation$is_valid || nrow(validation$filtered_data) < 3) {
    warning("Insufficient data for price testing.")
    stats <- calculate_descriptive_stats(filtered_data, dv, "GROUP")
    return(list(
      plot = NULL, filtered_data = filtered_data, statistics = stats,
      vda = NULL, dunn = NULL, validation_failed = TRUE
    ))
  }

  filtered_data <- validation$filtered_data

  # ── Statistical tests ──
  formula_obj <- as.formula(paste(dv, "~ GROUP"))
  dunn <- rstatix::dunn_test(filtered_data, formula_obj, p.adjust.method = "holm")
  vda  <- rcompanion::multiVDA(x = filtered_data[[dv]], g = filtered_data[["GROUP"]])

  # ── Group medians (prices use median, not mean) ──
  group_stats <- filtered_data %>%
    dplyr::group_by(GROUP) %>%
    dplyr::summarise(
      median_val = median(!!rlang::sym(dv), na.rm = TRUE),
      .groups = "drop"
    )
  # ── Caption (stats only — no centrality legend) ──
  caption_md <- build_kw_subtitle(filtered_data, dv, "GROUP")

  # ── Build plot — no midpoint line, dollar y-axis ──
  p <- ggplot2::ggplot(
    filtered_data,
    ggplot2::aes(x = GROUP, y = !!rlang::sym(dv))
  ) +
    ggplot2::geom_boxplot(
      width = 0.15, outlier.shape = NA, fill = style$boxplot_fill
    ) +
    ggplot2::geom_violin(
      width = 1.4,
      fill = style$violin_fill, color = style$violin_outline,
      alpha = 0.4, linewidth = 0.5
    )

  # Show individual price estimates if requested
  if (show_points) {
    p <- p + ggplot2::geom_jitter(width = 0.1, alpha = alpha, size = 0.8)
  }

  p <- p +
    ggplot2::geom_point(
      data = group_stats,
      ggplot2::aes(x = GROUP, y = median_val),
      size = style$mean_dot_size, color = style$mean_dot_color, inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = group_stats,
      ggplot2::aes(
        x = GROUP, y = median_val,
        label = sprintf("$%s",
                        formatC(median_val, format = "f",
                                big.mark = ",", digits = 0))
      ),
      size = style$label_size, family = base_family,
      nudge_x = 0.20,
      label.size = 0.15, label.padding = ggplot2::unit(0.12, "lines"),
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) ifelse(x >= 1000,
                                  paste0("$", x / 1000, "k"),
                                  paste0("$", x)),
      expand = ggplot2::expansion(mult = c(0.12, 0.05))
    ) +
    ggthemes::theme_tufte(base_family = base_family, base_size = style$base_size) +
    ggplot2::theme(
      plot.title          = ggtext::element_markdown(size = style$title_size, margin = ggplot2::margin(b = 2)),
      plot.title.position = "plot",
      plot.subtitle       = ggplot2::element_text(size = style$subtitle_size, margin = ggplot2::margin(t = 2, b = 2)),
      plot.caption        = ggtext::element_markdown(size = style$caption_size, hjust = 0.5),
      axis.title.y        = ggplot2::element_blank(),
      axis.text.x         = ggplot2::element_text(size = style$axis_text_x, color = "black",
                              margin = ggplot2::margin(t = -5)),
      axis.text.y         = ggplot2::element_text(size = style$axis_text_y, color = "black",
                              margin = ggplot2::margin(r = 0)),
      axis.ticks.y        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks.x        = ggplot2::element_blank(),
      plot.background     = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background    = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin         = ggplot2::margin(t = 5, r = 0, b = 5, l = 10)
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.0, 0.0))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title, subtitle = subtitle, x = NULL,
      caption = caption_md
    )

  # ── VDA brackets (below plot, between data and group labels) ──
  y_min <- min(filtered_data[[dv]], na.rm = TRUE)
  y_range <- max(filtered_data[[dv]], na.rm = TRUE) - y_min
  p <- add_vda_brackets(p, dunn, vda,
                        y_start = y_min - y_range * 0.08,
                        y_step = -(y_range * 0.06),
                        base_family = base_family, style = style)

  # ── Descriptive statistics ──
  stats <- calculate_descriptive_stats(
    filtered_data, dv, "GROUP",
    stats = c("mean", "sd", "median")
  )

  return(list(
    plot = p, filtered_data = filtered_data, statistics = stats,
    vda = vda, dunn = dunn
  ))
}


# ─── Function 4: analyze_scenario_question ───────────────────
#' Complete analysis pipeline for a single question
#'
#' Routes to the appropriate plot function based on question type,
#' computes descriptive statistics, and optionally exports TIFF.
#'
#' @param data Survey data frame
#' @param dv Variable to analyze (e.g., "PD1", "PD25", "PD4")
#' @param question_type One of: "q1", "q2", "q3", "delta", "price"
#' @param scenario_name Full name (e.g., "Public Domain")
#' @param question_num Question number for axis label (e.g., "2.1")
#' @param export_tiff Save plot as TIFF?
#' @param tiff_width Width in inches
#' @param tiff_height Height in inches
#'
#' @return List with plot, statistics, vda, dunn, additional_results
analyze_scenario_question <- function(data,
                                     dv,
                                     question_type,
                                     scenario_name,
                                     question_num = NULL,
                                     export_tiff = TRUE,
                                     tiff_width = 4.6,
                                     tiff_height = 3.5,
                                     style = "web") {

  # ── Title (no subtitle — scenario code + question label in title) ──
  title_parts <- list(
    q1    = "Likelihood of Legal Consequences",
    q2    = "Estimated Legal Consequences (Current)",
    q3    = "Estimated Legal Consequences (Preferred)",
    delta = "Current\u2013Preferred Legal Consequences",
    price = "License Price Estimate"
  )
  q_label_map <- list(
    q1 = "Q1", q2 = "Q2", q3 = "Q3",
    delta = "Delta", price = "Q4"
  )
  scenario_code <- toupper(gsub("[0-9]+", "", dv))
  title    <- paste0("**", scenario_code, " ", q_label_map[[question_type]], ": ",
                     title_parts[[question_type]], " by Group**")
  subtitle <- NULL

  # ── Route to appropriate plot function ──
  resolved_style <- get_plot_style(style)

  if (question_type %in% c("q1", "q2", "q3")) {
    result <- create_scenario_plot(
      data = data, dv = dv, title = title, subtitle = subtitle,
      show_points = FALSE, style = resolved_style
    )
  } else if (question_type == "delta") {
    result <- create_delta_plot(
      data = data, dv = dv, title = title, subtitle = subtitle,
      style = resolved_style
    )
  } else if (question_type == "price") {
    result <- create_price_plot(
      data = data, dv = dv, title = title, subtitle = subtitle,
      style = resolved_style
    )
  }

  # ── Handle validation failure ──
  if (!is.null(result$validation_failed)) {
    warning("Skipping plot due to insufficient data")
    return(list(
      plot = NULL, statistics = result$statistics,
      vda = NULL, dunn = NULL,
      additional_results = NULL, validation_failed = TRUE
    ))
  }

  # ── Export TIFF if requested (always print-styled) ──
  if (export_tiff && !is.null(result$plot)) {
    # Build a separate print-styled plot for TIFF export.
    # Stats are recomputed internally (negligible cost).
    print_style <- get_plot_style("print")
    if (question_type %in% c("q1", "q2", "q3")) {
      print_result <- create_scenario_plot(
        data = data, dv = dv, title = title, subtitle = subtitle,
        show_points = FALSE, style = print_style)
    } else if (question_type == "delta") {
      print_result <- create_delta_plot(
        data = data, dv = dv, title = title, subtitle = subtitle,
        style = print_style)
    } else if (question_type == "price") {
      print_result <- create_price_plot(
        data = data, dv = dv, title = title, subtitle = subtitle,
        style = print_style)
    }
    if (!is.null(print_result$plot)) {
      filename <- paste0(toupper(gsub("(\\d+)", "", dv)),
                         round(tiff_width * 10), ".tiff")
      export_plot_to_tiff(print_result$plot, filename, tiff_width, tiff_height)
    }
  }

  # ── Unify return ──
  additional_results <- if (question_type == "delta") {
    result$wilcox_tests
  } else if (question_type == "price") {
    result$filtered_data
  } else {
    NULL
  }

  return(list(
    plot               = result$plot,
    statistics         = result$statistics,
    vda                = result$vda,
    dunn               = result$dunn,
    additional_results = additional_results
  ))
}
