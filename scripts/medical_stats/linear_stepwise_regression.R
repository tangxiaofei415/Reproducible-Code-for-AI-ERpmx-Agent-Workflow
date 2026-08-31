# example usage:
# generate_stepwise_linear_regression(
#   df = df,
#   y_var = "Y",
#   x_vars = c("Cmax1", "AUCss", "Gender"),
#   strategy = "bidirection",
#   display = "both",
#   output_dir = "linear_stepwise_regression"
# )
generate_stepwise_linear_regression <- function(
    df = df,
    y_var = NULL,
    x_vars = NULL,
    strategy = c("bidirection", "backward", "forward", "both"),
    display = c("both", "table", "forest"),
    output_dir = "linear_stepwise_regression",
    metric = "AIC",
    sle = 0.05,
    sls = 0.05,
    digits = 3,
    width = NULL, height = NULL, dpi = 300) {
  strategy <- match.arg(strategy)
  display <- match.arg(display)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (is.null(y_var) || !y_var %in% names(df)) stop("`y_var` was not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[y_var]])) stop("`y_var` must be numeric for linear regression.", call. = FALSE)
  if (is.null(x_vars) || !length(x_vars)) stop("`x_vars` must include at least one variable.", call. = FALSE)

  x_vars <- intersect(x_vars, names(df))
  if (!length(x_vars)) stop("No valid `x_vars` were found in `df`.", call. = FALSE)
  model_df <- df[stats::complete.cases(df[, c(y_var, x_vars), drop = FALSE]), c(y_var, x_vars), drop = FALSE]
  if (!nrow(model_df)) stop("No complete rows are available for the requested model.", call. = FALSE)
  for (x_var in x_vars) {
    if (is.character(model_df[[x_var]]) || is.factor(model_df[[x_var]])) {
      model_df[[x_var]] <- droplevels(factor(model_df[[x_var]]))
    }
  }

  full_formula <- stats::reformulate(x_vars, response = y_var)
  if (requireNamespace("StepReg", quietly = TRUE)) {
    step_fit <- StepReg::stepwise(
      formula = full_formula,
      data = model_df,
      type = "linear",
      strategy = strategy,
      metric = metric,
      sle = as.numeric(sle),
      sls = as.numeric(sls),
      test_method_glm = "LRT"
    )
    final_formula_str <- data.frame(step_fit$voted_model)[, 1]
    if (is.null(final_formula_str) || !length(final_formula_str) || is.na(final_formula_str[1])) {
      stop("Stepwise regression did not select any variables.", call. = FALSE)
    }
    final_fit <- stats::lm(stats::as.formula(final_formula_str[1]), data = model_df)
  } else {
    full_fit <- stats::lm(full_formula, data = model_df)
    null_fit <- stats::lm(stats::reformulate("1", response = y_var), data = model_df)
    direction <- switch(strategy, backward = "backward", forward = "forward", bidirection = "both", both = "both")
    final_fit <- stats::step(
      full_fit,
      scope = list(lower = stats::formula(null_fit), upper = stats::formula(full_fit)),
      direction = direction,
      trace = 0
    )
  }

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

  sm <- summary(final_fit)$coefficients
  ci <- suppressMessages(stats::confint(final_fit))
  terms <- setdiff(rownames(sm), "(Intercept)")
  result_df <- if (length(terms)) {
    do.call(rbind, lapply(terms, function(term) {
      variable <- term_variable(term, x_vars)
      data.frame(
        variable = variable,
        level = term_level(term, variable),
        term = term,
        n = stats::nobs(final_fit),
        beta = unname(sm[term, "Estimate"]),
        conf_low = unname(ci[term, 1]),
        conf_high = unname(ci[term, 2]),
        p_value = unname(sm[term, "Pr(>|t|)"]),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      variable = character(),
      level = character(),
      term = character(),
      n = integer(),
      beta = numeric(),
      conf_low = numeric(),
      conf_high = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )
  }
  result_df$beta_ci <- sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), result_df$beta, result_df$conf_low, result_df$conf_high)
  result_df$p_value_text <- ifelse(result_df$p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), result_df$p_value))
  result_df$final_formula <- paste(deparse(stats::formula(final_fit)), collapse = " ")

  output <- list(
    final_formula = stats::formula(final_fit),
    table = result_df
  )
  csv_path <- file.path(output_dir, "linear_stepwise_regression.csv")
  utils::write.csv(result_df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  output$csv_path <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)

  if (display %in% c("both", "forest")) {
    if (!nrow(result_df)) stop("The final stepwise model contains no predictor variables, so a forest plot cannot be drawn.", call. = FALSE)
    require_r_packages(c("autoReg", "ragg"))
    assign(".autoreg_df", model_df, envir = .GlobalEnv)
    on.exit(rm(".autoreg_df", envir = .GlobalEnv), add = TRUE)
    plot_fit <- stats::lm(
      stats::formula(final_fit),
      data = .autoreg_df
    )
    p <- autoReg::modelPlot(
      plot_fit,
      uni = FALSE,
      multi = TRUE,
      show.ref = TRUE
    )
    png_path <- file.path(output_dir, "linear_stepwise_regression_forest.png")
    plot_size <- resolve_forest_plot_size(
      result_df,
      width = width,
      height = height,
      default_width = 8,
      default_height = 6,
      add_reference_rows = TRUE
    )
    ragg::agg_png(filename = png_path, width = plot_size$width, height = plot_size$height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
    output$plot_path <- normalizePath(png_path, winslash = "/", mustWork = FALSE)
  }

  message("Stepwise linear regression outputs saved to: ", normalizePath(output_dir))
  invisible(output)
}
