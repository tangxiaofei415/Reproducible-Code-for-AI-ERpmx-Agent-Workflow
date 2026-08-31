# example usage:
# plot_univariable_cox(
#   df = df,
#   time_var = "PFS",
#   status_var = "PFS_status",
#   group_vars = c("Gender", "DOSE"),
#   facet_var = NULL,
#   output_dir = "cox_plots"
# )
plot_univariable_cox <- function(
    df = df,
    time_var = NULL,
    status_var = NULL,
    group_vars = NULL,
    facet_var = NULL,
    output_dir = "cox_plots",
    show_confidence_interval = TRUE,
    show_p_value = TRUE,
    show_risk_table = TRUE,
    show_censoring_table = TRUE,
    show_title = TRUE,
    palette = "default",
    width = 6, height = 7, dpi = 300) {
  require_r_packages(c("survival", "survminer", "ggplot2", "ragg", "showtext"))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  plot_font <- getOption("bioflow_zh_font", "wqy-microhei")
  showtext::showtext_opts(dpi = dpi)
  if (is.null(time_var) || !time_var %in% names(df)) stop("`time_var` was not found in `df`.", call. = FALSE)
  if (is.null(status_var) || !status_var %in% names(df)) stop("`status_var` was not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[time_var]])) stop("`time_var` must be numeric.", call. = FALSE)
  status_values <- stats::na.omit(df[[status_var]])
  if (!all(status_values %in% c(0, 1))) stop("`status_var` must contain only 0 and 1.", call. = FALSE)
  if (is.null(group_vars) || !length(group_vars)) stop("`group_vars` must include at least one variable.", call. = FALSE)

  has_facet <- !is.null(facet_var) && length(facet_var) == 1L && nzchar(facet_var) && facet_var %in% names(df)
  clean_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)
  save_survival_plot <- function(path, km_plot) {
    ragg::agg_png(filename = path, width = width, height = height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(km_plot)
  }
  logrank_p_value <- function(surv_formula, plot_df) {
    tryCatch(
      {
        diff <- survival::survdiff(surv_formula, data = plot_df)
        stats::pchisq(diff$chisq, length(diff$n) - 1L, lower.tail = FALSE)
      },
      error = function(e) NA_real_
    )
  }
  cox_stat_rows <- function(plot_df, group_var, surv_formula, plot_path, logrank_p) {
    tryCatch(
      {
        fit <- survival::coxph(surv_formula, data = plot_df)
        coef_table <- summary(fit)$coefficients
        conf_table <- summary(fit)$conf.int
        terms <- rownames(coef_table)
        if (!length(terms)) return(NULL)
        do.call(rbind, lapply(terms, function(term) {
          data.frame(
            family = "cox",
            outcome = paste0("Surv(", time_var, ", ", status_var, ")"),
            variable = group_var,
            level = if (startsWith(term, group_var)) substring(term, nchar(group_var) + 1L) else "",
            term = term,
            n = stats::nobs(fit),
            event_n = sum(plot_df[[status_var]] == 1),
            hr = unname(conf_table[term, "exp(coef)"]),
            conf_low = unname(conf_table[term, "lower .95"]),
            conf_high = unname(conf_table[term, "upper .95"]),
            p_value = unname(coef_table[term, "Pr(>|z|)"]),
            logrank_p_value = logrank_p,
            plot_path = plot_path,
            facet_var = if (has_facet) facet_var else "",
            stringsAsFactors = FALSE
          )
        }))
      },
      error = function(e) {
        data.frame(
          family = "cox",
          outcome = paste0("Surv(", time_var, ", ", status_var, ")"),
          variable = group_var,
          level = "",
          term = group_var,
          n = nrow(plot_df),
          event_n = sum(plot_df[[status_var]] == 1),
          hr = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_,
          p_value = logrank_p,
          logrank_p_value = logrank_p,
          plot_path = plot_path,
          facet_var = if (has_facet) facet_var else "",
          stringsAsFactors = FALSE
        )
      }
    )
  }
  survival_table_rows <- function(fit, plot_df, group_var, plot_path) {
    clean_strata_label <- function(strata) {
      label <- as.character(strata)
      for (prefix in c(paste0("`", group_var, "`="), paste0(group_var, "="))) {
        if (startsWith(label, prefix)) {
          label <- substring(label, nchar(prefix) + 1L)
        }
      }
      label
    }
    baseline_rows <- NULL
    if (!is.null(fit$strata) && length(fit$strata)) {
      baseline_rows <- data.frame(
        family = "cox",
        table_type = "survival_risk_censor",
        row_type = "baseline",
        outcome = paste0("Surv(", time_var, ", ", status_var, ")"),
        variable = group_var,
        strata = names(fit$strata),
        level = vapply(names(fit$strata), clean_strata_label, character(1)),
        time = 0,
        n = nrow(plot_df),
        event_n = sum(plot_df[[status_var]] == 1),
        n_risk = as.integer(fit$strata),
        n_event = 0,
        n_censor = 0,
        cum_censor = 0,
        survival = 1,
        conf_low = NA_real_,
        conf_high = NA_real_,
        plot_path = plot_path,
        facet_var = if (has_facet) facet_var else "",
        stringsAsFactors = FALSE
      )
    } else {
      baseline_rows <- data.frame(
        family = "cox",
        table_type = "survival_risk_censor",
        row_type = "baseline",
        outcome = paste0("Surv(", time_var, ", ", status_var, ")"),
        variable = group_var,
        strata = "overall",
        level = "",
        time = 0,
        n = nrow(plot_df),
        event_n = sum(plot_df[[status_var]] == 1),
        n_risk = nrow(plot_df),
        n_event = 0,
        n_censor = 0,
        cum_censor = 0,
        survival = 1,
        conf_low = NA_real_,
        conf_high = NA_real_,
        plot_path = plot_path,
        facet_var = if (has_facet) facet_var else "",
        stringsAsFactors = FALSE
      )
    }
    surv_summary <- summary(fit, censored = TRUE)
    if (!length(surv_summary$time)) {
      return(baseline_rows)
    }
    strata_values <- if (!is.null(surv_summary$strata)) as.character(surv_summary$strata) else rep("overall", length(surv_summary$time))
    event_rows <- data.frame(
      family = "cox",
      table_type = "survival_risk_censor",
      row_type = "survival_summary",
      outcome = paste0("Surv(", time_var, ", ", status_var, ")"),
      variable = group_var,
      strata = strata_values,
      level = vapply(strata_values, clean_strata_label, character(1)),
      time = surv_summary$time,
      n = nrow(plot_df),
      event_n = sum(plot_df[[status_var]] == 1),
      n_risk = surv_summary$n.risk,
      n_event = surv_summary$n.event,
      n_censor = surv_summary$n.censor,
      cum_censor = NA_real_,
      survival = surv_summary$surv,
      conf_low = if (!is.null(surv_summary$lower)) surv_summary$lower else NA_real_,
      conf_high = if (!is.null(surv_summary$upper)) surv_summary$upper else NA_real_,
      plot_path = plot_path,
      facet_var = if (has_facet) facet_var else "",
      stringsAsFactors = FALSE
    )
    rows <- rbind(baseline_rows, event_rows)
    rows$cum_censor <- ave(rows$n_censor, rows$strata, FUN = cumsum)
    rows
  }

  stat_rows <- list()
  survival_rows <- list()
  for (group_var in group_vars) {
    if (!group_var %in% names(df)) next
    vars_needed <- c(time_var, status_var, group_var, if (has_facet) facet_var)
    plot_df <- df[stats::complete.cases(df[, vars_needed, drop = FALSE]), vars_needed, drop = FALSE]
    if (!nrow(plot_df)) next
    plot_df[[group_var]] <- droplevels(factor(plot_df[[group_var]]))
    if (nlevels(plot_df[[group_var]]) < 2L) next
    if (has_facet) plot_df[[facet_var]] <- droplevels(factor(plot_df[[facet_var]]))

    surv_formula <- stats::as.formula(
      paste0("survival::Surv(`", time_var, "`, `", status_var, "`) ~ `", group_var, "`")
    )
    fit <- survival::survfit(surv_formula, data = plot_df)
    fit$call$formula <- surv_formula
    plot_title <- if (isTRUE(show_title)) paste("Kaplan-Meier Curve by", group_var) else NULL
    logrank_p <- logrank_p_value(surv_formula, plot_df)

    if (has_facet && exists("ggsurvplot_facet", where = asNamespace("survminer"), inherits = FALSE)) {
      km_plot <- tryCatch(
        survminer::ggsurvplot_facet(
          fit,
          data = plot_df,
          facet.by = facet_var,
          conf.int = show_confidence_interval,
          pval = show_p_value,
          palette = palette,
          ggtheme = ggplot2::theme_bw(base_family = plot_font)
        ),
        error = function(e) {
          if (!isTRUE(show_p_value)) stop(e)
          warning("Retrying faceted Cox plot without P-value annotation: ", conditionMessage(e), call. = FALSE)
          survminer::ggsurvplot_facet(
            fit,
            data = plot_df,
            facet.by = facet_var,
            conf.int = show_confidence_interval,
            pval = FALSE,
            palette = palette,
            ggtheme = ggplot2::theme_bw(base_family = plot_font)
          )
        }
      )
      if (!is.null(plot_title) && is.list(km_plot) && "plot" %in% names(km_plot)) {
        km_plot$plot <- km_plot$plot + ggplot2::labs(title = plot_title)
      }
    } else {
      km_plot <- survminer::ggsurvplot(
        fit,
        data = plot_df,
        conf.int = show_confidence_interval,
        pval = show_p_value,
        risk.table = show_risk_table,
        cumcensor = show_censoring_table,
        palette = palette,
        title = plot_title,
        xlab = "Time",
        ylab = "Survival probability",
        ggtheme = ggplot2::theme_bw(base_family = plot_font)
      )
    }

    filename <- paste0(
      "plot_",
      clean_name(group_var),
      if (has_facet) paste0("_by_", clean_name(facet_var)) else "",
      ".png"
    )
    plot_path <- file.path(output_dir, filename)
    save_survival_plot(plot_path, km_plot)
    stat_row <- cox_stat_rows(plot_df, group_var, surv_formula, plot_path, logrank_p)
    if (!is.null(stat_row) && nrow(stat_row)) {
      stat_rows[[length(stat_rows) + 1L]] <- stat_row
    }
    survival_row <- survival_table_rows(fit, plot_df, group_var, plot_path)
    if (!is.null(survival_row) && nrow(survival_row)) {
      survival_rows[[length(survival_rows) + 1L]] <- survival_row
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
  if (length(survival_rows)) {
    utils::write.csv(
      do.call(rbind, survival_rows),
      file.path(output_dir, "plot_survival_table.csv"),
      row.names = FALSE,
      na = ""
    )
  }
  message("Cox plots saved to: ", normalizePath(output_dir))
}
