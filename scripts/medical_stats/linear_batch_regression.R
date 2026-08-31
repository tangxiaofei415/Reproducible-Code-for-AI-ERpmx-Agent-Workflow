# example usage:
# generate_batch_linear_regression(
#   df = df,
#   y_var = "Y",
#   x_vars = c("Cmax1", "AUCss", "Gender"),
#   model_method = "univariable",
#   display = "both",
#   output_dir = "linear_batch_regression"
# )
generate_batch_linear_regression <- function(
    df = df,
    y_var = NULL,
    x_vars = NULL,
    model_method = c("univariable", "multivariable"),
    display = c("both", "table", "forest"),
    output_dir = "linear_batch_regression",
    digits = 3,
    width = NULL, height = NULL, dpi = 300) {
  model_method <- match.arg(model_method)
  display <- match.arg(display)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (is.null(y_var) || !y_var %in% names(df)) stop("`y_var` was not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[y_var]])) stop("`y_var` must be numeric for linear regression.", call. = FALSE)
  if (is.null(x_vars) || !length(x_vars)) stop("`x_vars` must include at least one variable.", call. = FALSE)

  x_vars <- intersect(x_vars, names(df))
  if (!length(x_vars)) stop("No valid `x_vars` were found in `df`.", call. = FALSE)

  term_level <- function(term, variable) {
    for (prefix in c(variable, paste0("`", variable, "`"))) {
      if (startsWith(term, prefix)) {
        return(substring(term, nchar(prefix) + 1L))
      }
    }
    ""
  }
  term_variable <- function(term, variables) {
    matches <- variables[vapply(variables, function(variable) {
      startsWith(term, variable) || startsWith(term, paste0("`", variable, "`"))
    }, logical(1))]
    if (length(matches)) matches[which.max(nchar(matches))] else term
  }
  fit_one_term <- function(fit, term, variable, level = "") {
    sm <- summary(fit)$coefficients
    ci <- suppressMessages(stats::confint(fit))
    data.frame(
      variable = variable,
      level = level,
      term = term,
      n = stats::nobs(fit),
      beta = unname(sm[term, "Estimate"]),
      conf_low = unname(ci[term, 1]),
      conf_high = unname(ci[term, 2]),
      p_value = unname(sm[term, "Pr(>|t|)"]),
      stringsAsFactors = FALSE
    )
  }

  results <- list()
  if (model_method == "univariable") {
    for (x_var in x_vars) {
      model_df <- df[stats::complete.cases(df[, c(y_var, x_var), drop = FALSE]), c(y_var, x_var), drop = FALSE]
      if (!nrow(model_df)) {
        warning(sprintf("Skipping `%s`: no complete rows.", x_var), call. = FALSE)
        next
      }
      if (is.character(model_df[[x_var]]) || is.factor(model_df[[x_var]])) {
        model_df[[x_var]] <- droplevels(factor(model_df[[x_var]]))
      }
      fit <- stats::lm(stats::reformulate(x_var, response = y_var), data = model_df)
      terms <- setdiff(rownames(summary(fit)$coefficients), "(Intercept)")
      if (!length(terms)) next
      if (is.factor(model_df[[x_var]])) {
        ref_level <- levels(model_df[[x_var]])[1]
        results[[length(results) + 1L]] <- data.frame(
          variable = x_var,
          level = ref_level,
          term = paste0(x_var, ref_level),
          n = sum(model_df[[x_var]] == ref_level),
          beta = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_,
          p_value = NA_real_,
          stringsAsFactors = FALSE
        )
      }
      for (term in terms) {
        results[[length(results) + 1L]] <- fit_one_term(fit, term, x_var, term_level(term, x_var))
      }
    }
  } else {
    model_df <- df[stats::complete.cases(df[, c(y_var, x_vars), drop = FALSE]), c(y_var, x_vars), drop = FALSE]
    if (!nrow(model_df)) stop("No complete rows are available for the requested model.", call. = FALSE)
    for (x_var in x_vars) {
      if (is.character(model_df[[x_var]]) || is.factor(model_df[[x_var]])) {
        model_df[[x_var]] <- droplevels(factor(model_df[[x_var]]))
      }
    }
    assign(".autoreg_df", model_df, envir = .GlobalEnv)
    on.exit(rm(".autoreg_df", envir = .GlobalEnv), add = TRUE)
    fit <- stats::lm(stats::reformulate(x_vars, response = y_var), data = .autoreg_df)
    terms <- setdiff(rownames(summary(fit)$coefficients), "(Intercept)")
    for (term in terms) {
      variable <- term_variable(term, x_vars)
      results[[length(results) + 1L]] <- fit_one_term(fit, term, variable, term_level(term, variable))
    }
  }

  if (!length(results)) stop("No regression results were produced.", call. = FALSE)
  result_df <- do.call(rbind, results)
  result_df$beta_ci <- ifelse(
    is.na(result_df$beta),
    "Reference",
    sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), result_df$beta, result_df$conf_low, result_df$conf_high)
  )
  result_df$p_value_text <- ifelse(
    is.na(result_df$p_value),
    "",
    ifelse(result_df$p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), result_df$p_value))
  )

  output <- list(table = result_df)
  csv_path <- file.path(output_dir, paste0("linear_batch_regression_", model_method, ".csv"))
  utils::write.csv(result_df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  output$csv_path <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)

  if (display %in% c("both", "forest")) {
    if (model_method == "univariable") {
      require_r_packages(c("forestploter", "grid", "ragg"))
      plot_df <- data.frame(
        Variable = ifelse(result_df$level == "", result_df$variable, ""),
        Level = result_df$level,
        N = result_df$n,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      plot_df$`  ` <- paste(rep(" ", 20), collapse = " ")
      plot_df$`Slope(95% CI)` <- result_df$beta_ci
      plot_df$P <- result_df$p_value_text
      p <- forestploter::forest(
        plot_df,
        est = result_df$beta,
        lower = result_df$conf_low,
        upper = result_df$conf_high,
        ci_column = 4,
        ref_line = 0,
        nudge_y = 0.2
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
    png_path <- file.path(output_dir, paste0("linear_batch_regression_", model_method, "_forest.png"))
    plot_size <- resolve_forest_plot_size(
      result_df,
      width = width,
      height = height,
      default_width = 7,
      default_height = 3,
      add_reference_rows = model_method != "univariable"
    )
    ragg::agg_png(filename = png_path, width = plot_size$width, height = plot_size$height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
    output$plot_path <- normalizePath(png_path, winslash = "/", mustWork = FALSE)
  }

  message("Batch linear regression outputs saved to: ", normalizePath(output_dir))
  invisible(output)
}
