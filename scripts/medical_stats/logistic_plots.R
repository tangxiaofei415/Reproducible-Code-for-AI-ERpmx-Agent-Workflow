# example usage:
# plot_univariable_logistic(
#   df = df,
#   y_var = "Y",
#   x_vars = c("Cmax1", "AUCss"),
#   group_var = "DOSE",
#   x_ticks = c(0, 25, 50, 75, 100),
#   output_dir = "logistic_plots"
# )
plot_univariable_logistic <- function(
    df = df,
    y_var = NULL,
    x_vars = NULL,
    group_var = NULL,
    output_dir = "logistic_plots",
    link_function = "logit",
    event_label = "Event",
    x_ticks = NULL,
    show_title = TRUE,
    show_p_value = TRUE,
    show_observed_ci = TRUE,
    observed_ci_bins = 4,
    width = 6, height = 5, dpi = 300, base_size = 14) {
  require_r_packages(c("ggplot2", "ragg", "showtext"))
  if (isTRUE(show_observed_ci)) require_r_packages(c("binom", "Hmisc"))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  plot_font <- getOption("bioflow_zh_font", "wqy-microhei")
  showtext::showtext_opts(dpi = dpi)
  if (is.null(y_var) || !y_var %in% names(df)) stop("`y_var` was not found in `df`.", call. = FALSE)
  if (is.null(x_vars) || !length(x_vars)) stop("`x_vars` must include at least one variable.", call. = FALSE)

  has_group <- !is.null(group_var) && length(group_var) == 1L && nzchar(group_var) && group_var %in% names(df)
  clean_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)
  make_breaks <- function(v, nm) {
    if (is.null(v)) {
      return(NULL)
    }
    if (!is.numeric(v) || length(v) < 2L) stop(sprintf("`%s` must be a numeric vector with at least 2 values.", nm), call. = FALSE)
    list(breaks = sort(unique(v)))
  }
  to_binary <- function(x) {
    values <- stats::na.omit(x)
    if (is.logical(values)) {
      return(as.numeric(x))
    }
    if (is.numeric(values) || is.integer(values)) {
      if (!all(unique(values) %in% c(0, 1))) stop("Numeric `y_var` must contain only 0 and 1.", call. = FALSE)
      return(as.numeric(x))
    }
    if (length(unique(values)) != 2L) stop("`y_var` must have exactly 2 levels.", call. = FALSE)
    as.numeric(factor(x)) - 1
  }
  numeric_binomial_summary <- function(plot_df, x_var, plot_path) {
    x <- plot_df[[x_var]]
    n_bins <- min(observed_ci_bins, length(unique(x)))
    if (n_bins < 2L) return(NULL)
    bins <- tryCatch(ggplot2::cut_number(x, n = n_bins), error = function(e) NULL)
    if (is.null(bins)) return(NULL)
    parts <- split(seq_len(nrow(plot_df)), bins)
    do.call(rbind, lapply(names(parts), function(bin_label) {
      idx <- parts[[bin_label]]
      y <- plot_df$y_binary[idx]
      exact_ci <- binom::binom.exact(sum(y), length(y), conf.level = 0.95)
      p <- mean(y)
      data.frame(
        family = "logistic",
        summary_type = "numeric_x_binned_observed_probability",
        outcome = y_var,
        variable = x_var,
        level = "",
        bin = bin_label,
        x = stats::median(x[idx]),
        x_min = min(x[idx], na.rm = TRUE),
        x_max = max(x[idx], na.rm = TRUE),
        n = length(y),
        event_n = sum(y),
        non_event_n = sum(y == 0),
        probability = p,
        conf_low = exact_ci$lower,
        conf_high = exact_ci$upper,
        plot_path = plot_path,
        stringsAsFactors = FALSE
      )
    }))
  }
  categorical_binomial_summary <- function(plot_df, x_var, plot_path) {
    x <- factor(plot_df[[x_var]])
    parts <- split(seq_len(nrow(plot_df)), x, drop = TRUE)
    do.call(rbind, lapply(names(parts), function(level_label) {
      idx <- parts[[level_label]]
      y <- plot_df$y_binary[idx]
      exact_ci <- binom::binom.exact(sum(y), length(y), conf.level = 0.95)
      p <- mean(y)
      data.frame(
        family = "logistic",
        summary_type = "categorical_x_observed_probability",
        outcome = y_var,
        variable = x_var,
        level = level_label,
        bin = "",
        x = NA_real_,
        x_min = NA_real_,
        x_max = NA_real_,
        n = length(y),
        event_n = sum(y),
        non_event_n = sum(y == 0),
        probability = p,
        conf_low = exact_ci$lower,
        conf_high = exact_ci$upper,
        plot_path = plot_path,
        stringsAsFactors = FALSE
      )
    }))
  }
  numeric_x_group_descriptive_summary <- function(plot_df, x_var, plot_path) {
    if (!has_group || !is.numeric(plot_df[[x_var]])) return(NULL)
    parts <- split(seq_len(nrow(plot_df)), plot_df[[group_var]], drop = TRUE)
    do.call(rbind, lapply(names(parts), function(group_level) {
      idx <- parts[[group_level]]
      x <- plot_df[[x_var]][idx]
      y <- plot_df$y_binary[idx]
      data.frame(
        family = "logistic",
        summary_type = "numeric_x_group_boxplot_summary",
        outcome = y_var,
        variable = x_var,
        level = "",
        group_var = group_var,
        group_level = group_level,
        n = length(idx),
        event_n = sum(y),
        non_event_n = sum(y == 0),
        event_rate = mean(y),
        x_mean = mean(x, na.rm = TRUE),
        x_sd = stats::sd(x, na.rm = TRUE),
        x_median = stats::median(x, na.rm = TRUE),
        x_min = min(x, na.rm = TRUE),
        x_max = max(x, na.rm = TRUE),
        plot_path = plot_path,
        stringsAsFactors = FALSE
      )
    }))
  }
  y_label <- paste0("Probability of ", event_label)
  model_stat_rows <- function(model_df, x_var, plot_path) {
    tryCatch(
      {
        fit <- stats::glm(
          stats::reformulate(x_var, response = "y_binary"),
          data = model_df,
          family = stats::binomial(link = link_function)
        )
        coef_table <- summary(fit)$coefficients
        terms <- setdiff(rownames(coef_table), "(Intercept)")
        if (!length(terms)) return(NULL)
        do.call(rbind, lapply(terms, function(term) {
          estimate <- unname(coef_table[term, "Estimate"])
          se <- unname(coef_table[term, "Std. Error"])
          data.frame(
            family = "logistic",
            outcome = y_var,
            variable = x_var,
            level = if (startsWith(term, x_var)) substring(term, nchar(x_var) + 1L) else "",
            term = term,
            n = stats::nobs(fit),
            event_n = sum(model_df$y_binary == 1),
            non_event_n = sum(model_df$y_binary == 0),
            or = exp(estimate),
            conf_low = exp(estimate - 1.96 * se),
            conf_high = exp(estimate + 1.96 * se),
            p_value = unname(coef_table[term, "Pr(>|z|)"]),
            plot_path = plot_path,
            stringsAsFactors = FALSE
          )
        }))
      },
      error = function(e) NULL
    )
  }
  p_value_label <- function(plot_df, x_var) {
    p_value <- tryCatch(
      {
        fit <- stats::glm(
          stats::reformulate(x_var, response = "y_binary"),
          data = plot_df,
          family = stats::binomial(link = link_function)
        )
        summary(fit)$coefficients[2, 4]
      },
      error = function(e) NA_real_
    )
    if (is.na(p_value)) {
      ""
    } else if (p_value < 0.001) {
      "Overall P < 0.001"
    } else {
      sprintf("Overall P = %.3f", p_value)
    }
  }

  xs <- make_breaks(x_ticks, "x_ticks")
  y_binary_all <- to_binary(df[[y_var]])
  stat_rows <- list()
  observed_rows <- list()
  descriptive_rows <- list()
  for (x_var in x_vars) {
    if (!x_var %in% names(df)) next
    plot_path <- file.path(output_dir, paste0("plot_", clean_name(x_var), ".png"))
    model_vars <- c(y_var, x_var)
    model_rows <- stats::complete.cases(df[, model_vars, drop = FALSE])
    model_df <- df[model_rows, model_vars, drop = FALSE]
    if (!nrow(model_df)) next
    model_df$y_binary <- y_binary_all[model_rows]
    if (!all(model_df$y_binary %in% c(0, 1))) next

    plot_vars <- c(y_var, x_var, if (has_group) group_var)
    plot_rows <- stats::complete.cases(df[, plot_vars, drop = FALSE])
    plot_df <- df[plot_rows, plot_vars, drop = FALSE]
    if (!nrow(plot_df)) next
    plot_df$y_binary <- y_binary_all[plot_rows]
    if (!all(plot_df$y_binary %in% c(0, 1))) next
    if (has_group) plot_df[[group_var]] <- as.factor(plot_df[[group_var]])
    main_title <- if (isTRUE(show_title)) paste0(y_label, " vs. ", x_var) else NULL
    observed_row <- NULL
    descriptive_row <- NULL

    if (is.numeric(plot_df[[x_var]])) {
      if (isTRUE(show_title) && isTRUE(show_p_value)) {
        p_text <- p_value_label(model_df, x_var)
        if (nzchar(p_text)) {
          main_title <- paste0(main_title, " (", p_text, ")")
        }
      }
      mapping <- if (has_group) {
        ggplot2::aes(x = .data[[x_var]], y = .data$y_binary, color = .data[[group_var]])
      } else {
        ggplot2::aes(x = .data[[x_var]], y = .data$y_binary)
      }
      p_main <- ggplot2::ggplot(plot_df, mapping) +
        ggplot2::geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
        ggplot2::geom_smooth(
          data = model_df,
          mapping = ggplot2::aes(x = .data[[x_var]], y = .data$y_binary),
          method = "glm",
          method.args = list(family = stats::binomial(link = link_function)),
          se = TRUE,
          color = "red",
          inherit.aes = FALSE
        ) +
        ggplot2::coord_cartesian(ylim = c(-0.08, 1.08)) +
        ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1)) +
        ggplot2::labs(x = x_var, y = y_label, title = main_title) +
        ggplot2::theme_bw(base_size = base_size, base_family = plot_font) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = base_size + 2), axis.title = ggplot2::element_text(size = base_size), axis.text = ggplot2::element_text(size = base_size - 1))
      if (!is.null(xs)) p_main <- p_main + ggplot2::scale_x_continuous(breaks = xs$breaks)
      if (isTRUE(show_observed_ci)) {
        sum_df <- numeric_binomial_summary(plot_df, x_var, plot_path)
        if (!is.null(sum_df) && nrow(sum_df)) {
          p_main <- p_main +
            ggplot2::geom_pointrange(
              data = sum_df,
              ggplot2::aes(x = .data$x, y = .data$probability, ymin = .data$conf_low, ymax = .data$conf_high),
              inherit.aes = FALSE,
              color = "navy",
              linewidth = 0.7
            )
          observed_row <- sum_df
        }
      }
      descriptive_row <- numeric_x_group_descriptive_summary(plot_df, x_var, plot_path)
      p <- p_main
      if (has_group) {
        require_r_packages("patchwork")
        p_main <- p_main +
          ggplot2::labs(x = NULL, title = NULL) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
        p_box <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_var]], y = .data[[group_var]], color = .data[[group_var]])) +
          ggplot2::stat_boxplot(geom = "errorbar", width = 0.35) +
          ggplot2::geom_boxplot(alpha = 0.4, outlier.shape = NA) +
          ggplot2::labs(x = x_var, y = group_var) +
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
      plot_df[[x_var]] <- factor(plot_df[[x_var]])
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_var]], y = .data$y_binary)) +
        ggplot2::geom_jitter(width = 0.2, height = 0.05, alpha = 0.35, color = "grey40") +
        ggplot2::coord_cartesian(ylim = c(-0.08, 1.08)) +
        ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1)) +
        ggplot2::labs(x = x_var, y = y_label, title = main_title) +
        ggplot2::theme_bw(base_size = base_size, base_family = plot_font) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = base_size + 2), axis.title = ggplot2::element_text(size = base_size), axis.text = ggplot2::element_text(size = base_size - 1))
      if (isTRUE(show_observed_ci)) {
        observed_row <- categorical_binomial_summary(plot_df, x_var, plot_path)
        p <- p +
          ggplot2::stat_summary(
            fun.data = "mean_cl_boot",
            geom = "pointrange",
            color = "darkred"
          )
      }
    }

    ggplot2::ggsave(filename = plot_path, plot = p, device = ragg::agg_png, width = width, height = height, dpi = dpi)
    stat_row <- model_stat_rows(model_df, x_var, plot_path)
    if (!is.null(stat_row) && nrow(stat_row)) {
      stat_rows[[length(stat_rows) + 1L]] <- stat_row
    }
    if (!is.null(observed_row) && nrow(observed_row)) {
      observed_rows[[length(observed_rows) + 1L]] <- observed_row
    }
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
  if (length(observed_rows)) {
    utils::write.csv(
      do.call(rbind, observed_rows),
      file.path(output_dir, "plot_observed_summary.csv"),
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
  message("Logistic plots saved to: ", normalizePath(output_dir))
}
