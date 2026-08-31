# example usage:
# generate_batch_cox_regression(
#   df = df,
#   time_var = "time",
#   status_var = "status",
#   x_vars = c("age", "sex", "ph.ecog"),
#   model_method = "univariable",
#   display = "both",
#   output_dir = "cox_batch_regression"
# )
generate_batch_cox_regression <- function(
    df = df,
    time_var = NULL,
    status_var = NULL,
    x_vars = NULL,
    model_method = c("univariable", "multivariable"),
    display = c("both", "table", "forest"),
    output_dir = "cox_batch_regression",
    digits = 3,
    width = NULL, height = NULL, dpi = 300) {
  model_method <- match.arg(model_method)
  display <- match.arg(display)
  require_r_packages("survival")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (is.null(time_var) || !time_var %in% names(df)) stop("`time_var` was not found in `df`.", call. = FALSE)
  if (is.null(status_var) || !status_var %in% names(df)) stop("`status_var` was not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[time_var]])) stop("`time_var` must be numeric for Cox regression.", call. = FALSE)

  status_values <- stats::na.omit(df[[status_var]])
  if (!all(status_values %in% c(0, 1))) stop("`status_var` must contain only 0 and 1.", call. = FALSE)

  df_model_source <- as.data.frame(df)
  x_vars <- intersect(x_vars, names(df))
  if (!length(x_vars)) stop("No valid `x_vars` were found in `df`.", call. = FALSE)

  is_categorical_variable <- function(x) {
    if (is.factor(x) || is.character(x)) return(TRUE)
    if (is.numeric(x) || is.integer(x)) return(length(unique(stats::na.omit(x))) <= 10)
    FALSE
  }
  prepare_model_df <- function(data, vars) {
    model_df <- data[stats::complete.cases(data[, vars, drop = FALSE]), vars, drop = FALSE]
    for (x_var in intersect(x_vars, names(model_df))) {
      if (is.character(model_df[[x_var]]) || is.factor(model_df[[x_var]])) {
        model_df[[x_var]] <- droplevels(factor(model_df[[x_var]]))
      }
    }
    model_df
  }
  term_level <- function(term, variable) {
    sub(paste0("^`?", variable, "`?"), "", term)
  }
  term_variable <- function(term, variables) {
    matches <- variables[vapply(variables, function(variable) {
      startsWith(term, variable) || startsWith(term, paste0("`", variable, "`"))
    }, logical(1))]
    if (length(matches)) matches[which.max(nchar(matches))] else term
  }
  fit_one_term <- function(fit, term, variable, level = "", n = fit$n) {
    sm <- summary(fit)$coefficients
    ci <- suppressMessages(stats::confint(fit))
    data.frame(
      variable = variable,
      level = level,
      term = term,
      n = n,
      hr = exp(unname(sm[term, "coef"])),
      conf_low = exp(unname(ci[term, 1])),
      conf_high = exp(unname(ci[term, 2])),
      p_value = unname(sm[term, "Pr(>|z|)"]),
      stringsAsFactors = FALSE
    )
  }

  results <- list()
  if (model_method == "univariable") {
    for (x_var in x_vars) {
      temp <- prepare_model_df(df_model_source, c(time_var, status_var, x_var))
      if (!nrow(temp)) {
        warning(sprintf("Skipping `%s`: no complete rows.", x_var), call. = FALSE)
        next
      }
      if (!is_categorical_variable(temp[[x_var]])) {
        temp$x <- as.numeric(temp[[x_var]])
        fit <- survival::coxph(survival::Surv(temp[[time_var]], temp[[status_var]]) ~ x, data = temp)
        sm <- summary(fit)$coefficients
        if (!"x" %in% rownames(sm)) next
        results[[length(results) + 1L]] <- fit_one_term(fit, "x", x_var, n = nrow(temp))
      } else {
        temp[[x_var]] <- droplevels(factor(temp[[x_var]]))
        levels_x <- levels(temp[[x_var]])
        if (length(levels_x) < 2L) next
        ref_level <- levels_x[1]
        results[[length(results) + 1L]] <- data.frame(
          variable = x_var,
          level = ref_level,
          term = paste0(x_var, ref_level),
          n = sum(temp[[x_var]] == ref_level),
          hr = 1,
          conf_low = NA_real_,
          conf_high = NA_real_,
          p_value = NA_real_,
          stringsAsFactors = FALSE
        )
        Surv <- survival::Surv
        fit <- survival::coxph(
          stats::as.formula(paste0("Surv(`", time_var, "`, `", status_var, "`) ~ `", x_var, "`")),
          data = temp
        )
        terms <- rownames(summary(fit)$coefficients)
        for (term in terms) {
          level <- term_level(term, x_var)
          if (!level %in% levels_x[-1]) next
          results[[length(results) + 1L]] <- fit_one_term(
            fit,
            term,
            x_var,
            level,
            n = sum(temp[[x_var]] == level)
          )
        }
      }
    }
  } else {
    model_df <- prepare_model_df(df_model_source, c(time_var, status_var, x_vars))
    if (!nrow(model_df)) stop("No complete rows are available for the requested model.", call. = FALSE)
    assign(".autoreg_df", model_df, envir = .GlobalEnv)
    on.exit(rm(".autoreg_df", envir = .GlobalEnv), add = TRUE)
    Surv <- survival::Surv
    autoreg_formula <- stats::as.formula(
      paste0(
        "Surv(`", time_var, "`, `", status_var, "`) ~ ",
        paste(paste0("`", x_vars, "`"), collapse = " + ")
      )
    )
    fit <- survival::coxph(autoreg_formula, data = .autoreg_df)
    terms <- rownames(summary(fit)$coefficients)
    for (term in terms) {
      variable <- term_variable(term, x_vars)
      results[[length(results) + 1L]] <- fit_one_term(fit, term, variable, term_level(term, variable))
    }
  }

  if (!length(results)) stop("No regression results were produced.", call. = FALSE)
  result_df <- do.call(rbind, results)
  result_df$hr_ci <- ifelse(
    is.na(result_df$conf_low),
    "Reference",
    sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), result_df$hr, result_df$conf_low, result_df$conf_high)
  )
  format_p_value <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("<0.001***")
    if (p < 0.01) return(paste0(sprintf("%.4f", p), "**"))
    if (p < 0.05) return(paste0(sprintf("%.4f", p), "*"))
    sprintf("%.4f", p)
  }
  result_df$p_value_text <- vapply(result_df$p_value, format_p_value, character(1))

  output <- list(table = result_df)
  csv_path <- file.path(output_dir, paste0("cox_batch_regression_", model_method, ".csv"))
  utils::write.csv(result_df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  output$csv_path <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)

  if (display %in% c("both", "forest")) {
    if (model_method == "univariable") {
      require_r_packages(c("forestploter", "grid", "ragg"))
      plot_df <- data.frame(
        Variable = ifelse(result_df$level == "" | is.na(result_df$p_value), result_df$variable, ""),
        Level = result_df$level,
        N = result_df$n,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      plot_df$`  ` <- paste(rep(" ", 20), collapse = " ")
      plot_df$`HR (95% CI)` <- result_df$hr_ci
      plot_df$P <- result_df$p_value_text
      p <- forestploter::forest(
        plot_df,
        est = ifelse(is.na(result_df$conf_low), NA, result_df$hr),
        lower = result_df$conf_low,
        upper = result_df$conf_high,
        ci_column = 4,
        ref_line = 1
      )
    } else {
      require_r_packages(c("autoReg", "ragg"))
      p <- autoReg::modelPlot(
        fit,
        uni = FALSE,
        multi = TRUE,
        show.ref = TRUE
      )
    }
    png_path <- file.path(output_dir, paste0("cox_batch_regression_", model_method, "_forest.png"))
    plot_size <- resolve_forest_plot_size(
      result_df,
      width = width,
      height = height,
      default_width = 8,
      default_height = 6,
      add_reference_rows = model_method != "univariable"
    )
    ragg::agg_png(filename = png_path, width = plot_size$width, height = plot_size$height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
    output$plot_path <- normalizePath(png_path, winslash = "/", mustWork = FALSE)
  }

  message("Batch Cox regression outputs saved to: ", normalizePath(output_dir))
  invisible(output)
}
