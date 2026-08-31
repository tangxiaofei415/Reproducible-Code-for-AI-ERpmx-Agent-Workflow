# example usage:
# plot_univariable_linear(
#   df = df,
#   y_var = "Y",
#   y_labels = "Probability of AE",
#   x_vars = c("Cmax1", "AUCss"),
#   x_labels = c("Cmax1 concentration", "AUCss exposure"),
#   group_var = "Gender",
#   output_dir = "linear_plots",
#   x_ticks = c(0, 25, 50, 75, 100),
#   y_ticks = c(-2, -1, 0, 1, 2),
#   show_title = TRUE,
#   show_p_value = TRUE,
#   width = 6,
#   height = 4,
#   dpi = 300,
#   base_size = 14
# )
plot_univariable_linear <- function(
    df = df, 
    y_var = NULL, 
    y_labels = "Probability of AE",
    x_vars = NULL, 
    x_labels = NULL,
    group_var = NULL, 
    output_dir = "linear_plots",
    x_ticks = NULL, 
    y_ticks = NULL,
    show_title = TRUE,
    show_p_value = TRUE,
    width = 6, height = 5, dpi = 300, base_size = 14) {
  require_r_packages(c("ggplot2", "ragg", "showtext"))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  plot_font <- getOption("bioflow_zh_font", "wqy-microhei")
  showtext::showtext_opts(dpi = dpi)
  has_group <- !is.null(group_var) && length(group_var) == 1L && group_var %in% names(df)
  if (!is.null(x_labels)) {
    x_labels <- as.character(x_labels)
    if (length(x_labels) != length(x_vars)) stop("`x_labels` must have the same length as `x_vars`.", call. = FALSE)
  }
  make_scale <- function(v, nm) {
    if (is.null(v)) {
      return(NULL)
    }
    if (!is.numeric(v) || length(v) < 2L) stop(sprintf("`%s` must be a numeric vector with at least 2 values.", nm), call. = FALSE)
    v <- sort(unique(v))
    list(breaks = v)
  }
  clean_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)
  model_stat_rows <- function(plot_df, y_var, x_var, plot_path) {
    tryCatch(
      {
        fit <- stats::lm(stats::reformulate(x_var, response = y_var), data = plot_df)
        coef_table <- summary(fit)$coefficients
        conf <- stats::confint(fit)
        terms <- setdiff(rownames(coef_table), "(Intercept)")
        if (!length(terms)) return(NULL)
        do.call(rbind, lapply(terms, function(term) {
          data.frame(
            family = "linear",
            outcome = y_var,
            variable = x_var,
            level = if (startsWith(term, x_var)) substring(term, nchar(x_var) + 1L) else "",
            term = term,
            n = stats::nobs(fit),
            beta = unname(coef_table[term, "Estimate"]),
            conf_low = unname(conf[term, 1]),
            conf_high = unname(conf[term, 2]),
            p_value = unname(coef_table[term, "Pr(>|t|)"]),
            plot_path = plot_path,
            stringsAsFactors = FALSE
          )
        }))
      },
      error = function(e) NULL
    )
  }
  numeric_summary <- function(x) {
    if (!length(x)) {
      return(c(mean = NA_real_, sd = NA_real_, median = NA_real_, min = NA_real_, max = NA_real_))
    }
    c(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }
  make_descriptive_row <- function(
      summary_type,
      plot_df,
      y_var,
      x_var,
      plot_path,
      level = "",
      group_level = "") {
    x_stats <- if (is.numeric(plot_df[[x_var]])) numeric_summary(plot_df[[x_var]]) else numeric_summary(numeric())
    y_stats <- numeric_summary(plot_df[[y_var]])
    data.frame(
      family = "linear",
      summary_type = summary_type,
      outcome = y_var,
      variable = x_var,
      level = level,
      group_var = if (has_group) group_var else "",
      group_level = group_level,
      n = nrow(plot_df),
      x_mean = unname(x_stats["mean"]),
      x_sd = unname(x_stats["sd"]),
      x_median = unname(x_stats["median"]),
      x_min = unname(x_stats["min"]),
      x_max = unname(x_stats["max"]),
      y_mean = unname(y_stats["mean"]),
      y_sd = unname(y_stats["sd"]),
      y_median = unname(y_stats["median"]),
      y_min = unname(y_stats["min"]),
      y_max = unname(y_stats["max"]),
      plot_path = plot_path,
      stringsAsFactors = FALSE
    )
  }
  make_descriptive_rows <- function(plot_df, y_var, x_var, plot_path) {
    rows <- list()
    rows[[length(rows) + 1L]] <- make_descriptive_row(
      if (is.numeric(plot_df[[x_var]])) "overall_numeric_scatter_summary" else "overall_categorical_boxplot_summary",
      plot_df,
      y_var,
      x_var,
      plot_path
    )
    if (is.numeric(plot_df[[x_var]]) && has_group) {
      parts <- split(seq_len(nrow(plot_df)), plot_df[[group_var]], drop = TRUE)
      for (group_level in names(parts)) {
        idx <- parts[[group_level]]
        rows[[length(rows) + 1L]] <- make_descriptive_row(
          "numeric_x_group_boxplot_summary",
          plot_df[idx, , drop = FALSE],
          y_var,
          x_var,
          plot_path,
          group_level = group_level
        )
      }
    }
    if (!is.numeric(plot_df[[x_var]])) {
      split_key <- if (has_group) {
        interaction(plot_df[[x_var]], plot_df[[group_var]], drop = TRUE, sep = "||")
      } else {
        factor(plot_df[[x_var]])
      }
      parts <- split(seq_len(nrow(plot_df)), split_key, drop = TRUE)
      for (idx in parts) {
        level <- as.character(plot_df[[x_var]][idx[1]])
        group_level <- if (has_group) as.character(plot_df[[group_var]][idx[1]]) else ""
        rows[[length(rows) + 1L]] <- make_descriptive_row(
          "categorical_x_boxplot_summary",
          plot_df[idx, , drop = FALSE],
          y_var,
          x_var,
          plot_path,
          level = level,
          group_level = group_level
        )
      }
    }
    do.call(rbind, rows)
  }
  p_value_label <- function(plot_df, y_var, x_var) {
    p_value <- tryCatch(
      {
        fit <- stats::lm(stats::reformulate(x_var, response = y_var), data = plot_df)
        summary(fit)$coefficients[2, 4]
      },
      error = function(e) NA_real_
    )
    if (is.na(p_value)) {
      ""
    } else if (p_value < 0.001) {
      "P < 0.001"
    } else {
      sprintf("P = %.3f", p_value)
    }
  }
  xs <- make_scale(x_ticks, "x_ticks")
  ys <- make_scale(y_ticks, "y_ticks")
  stat_rows <- list()
  descriptive_rows <- list()
  for (i in seq_along(x_vars)) {
    x_var <- x_vars[i]
    if (!x_var %in% names(df)) next
    plot_path <- file.path(output_dir, paste0("plot_", clean_name(x_var), ".png"))
    x_lab <- if (!is.null(x_labels)) x_labels[i] else x_var
    y_lab <- if (!is.null(y_labels)) y_labels else y_var
    vars_needed <- c(y_var, x_var, if (has_group) group_var)
    plot_df <- df[stats::complete.cases(df[, vars_needed, drop = FALSE]), vars_needed, drop = FALSE]
    if (!nrow(plot_df)) next
    if (has_group) plot_df[[group_var]] <- as.factor(plot_df[[group_var]])
    mapping <- if (has_group) ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[group_var]]) else ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    main_title <- if (isTRUE(show_title)) paste(y_lab, "vs.", x_var) else NULL
    if (is.numeric(plot_df[[x_var]])) {
      if (isTRUE(show_title) && isTRUE(show_p_value)) {
        p_text <- p_value_label(plot_df, y_var, x_var)
        if (nzchar(p_text)) {
          main_title <- paste0(main_title, " (", p_text, ")")
        }
      }
      p_main <- ggplot2::ggplot(plot_df, mapping) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::geom_smooth(ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]), method = "lm", se = TRUE, color = "red", inherit.aes = FALSE) +
        ggplot2::labs(x = x_lab, y = y_lab, title = main_title) +
        ggplot2::theme_bw(base_size = base_size, base_family = plot_font) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = base_size + 2), axis.title = ggplot2::element_text(size = base_size), axis.text = ggplot2::element_text(size = base_size - 1))
      if (!is.null(xs)) p_main <- p_main + ggplot2::scale_x_continuous(breaks = xs$breaks)
      if (!is.null(ys)) p_main <- p_main + ggplot2::scale_y_continuous(breaks = ys$breaks)
      p <- p_main
      if (has_group && requireNamespace("patchwork", quietly = TRUE)) {
        p_main <- p_main +
          ggplot2::labs(x = NULL, title = NULL) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
        p_box <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_var]], y = .data[[group_var]], color = .data[[group_var]])) +
          ggplot2::stat_boxplot(geom = "errorbar", width = 0.35) +
          ggplot2::geom_boxplot(alpha = 0.4, outlier.shape = NA) +
          ggplot2::labs(x = x_lab, y = group_var) +
          ggplot2::theme_bw(base_size = base_size, base_family = plot_font) +
          ggplot2::theme(legend.position = "none", axis.title = ggplot2::element_text(size = base_size), axis.text = ggplot2::element_text(size = base_size - 1))
        if (!is.null(xs)) p_box <- p_box + ggplot2::scale_x_continuous(breaks = xs$breaks)
        p <- p_main / p_box + patchwork::plot_layout(heights = c(3, 1))
        if (!is.null(main_title)) {
          p <- p + patchwork::plot_annotation(title = main_title) &
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = base_size + 2))
        }
      }
    } else {
      p <- ggplot2::ggplot(plot_df, mapping) +
        ggplot2::geom_boxplot(alpha = 0.6, fill = "lightblue", outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
        ggplot2::labs(x = x_lab, y = y_lab, title = main_title) +
        ggplot2::theme_bw(base_size = base_size, base_family = plot_font) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = base_size + 2), axis.title = ggplot2::element_text(size = base_size), axis.text = ggplot2::element_text(size = base_size - 1))
      if (!is.null(ys)) p <- p + ggplot2::scale_y_continuous(breaks = ys$breaks)
    }
    ggplot2::ggsave(filename = plot_path, plot = p, device = ragg::agg_png, width = width, height = height, dpi = dpi)
    stat_row <- model_stat_rows(plot_df, y_var, x_var, plot_path)
    if (!is.null(stat_row) && nrow(stat_row)) {
      stat_rows[[length(stat_rows) + 1L]] <- stat_row
    }
    descriptive_row <- make_descriptive_rows(plot_df, y_var, x_var, plot_path)
    if (!is.null(descriptive_row) && nrow(descriptive_row)) {
      descriptive_rows[[length(descriptive_rows) + 1L]] <- descriptive_row
    }
  }
  if (length(stat_rows)) {
    utils::write.csv(
      do.call(rbind, stat_rows),
      file.path(output_dir, "plot_statistics.csv"),
      row.names = FALSE,
      na = ""
    )
  }
  if (length(descriptive_rows)) {
    utils::write.csv(
      do.call(rbind, descriptive_rows),
      file.path(output_dir, "plot_descriptive_summary.csv"),
      row.names = FALSE,
      na = ""
    )
  }
  message("Plots saved to: ", normalizePath(output_dir))
}
