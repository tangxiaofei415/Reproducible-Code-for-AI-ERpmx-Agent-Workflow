# example usage:
# generate_stepwise_cox_regression(
#   df = df,
#   time_var = "time",
#   status_var = "status",
#   x_vars = c("age", "sex", "ph.ecog"),
#   strategy = "bidirection",
#   display = "both",
#   output_dir = "cox_stepwise_regression"
# )
generate_stepwise_cox_regression <- function(
    df = df,
    time_var = NULL,
    status_var = NULL,
    x_vars = NULL,
    strategy = c("bidirection", "backward", "forward", "both"),
    display = c("both", "table", "forest"),
    output_dir = "cox_stepwise_regression",
    metric = "AIC",
    sle = 0.05,
    sls = 0.05,
    digits = 3,
    width = NULL, height = NULL, dpi = 300) {
  strategy <- match.arg(strategy)
  display <- match.arg(display)
  require_r_packages("survival")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (is.null(time_var) || !time_var %in% names(df)) stop("`time_var` was not found in `df`.", call. = FALSE)
  if (is.null(status_var) || !status_var %in% names(df)) stop("`status_var` was not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[time_var]])) stop("`time_var` must be numeric for Cox regression.", call. = FALSE)

  status_values <- stats::na.omit(df[[status_var]])
  if (!all(status_values %in% c(0, 1))) stop("`status_var` must contain only 0 and 1.", call. = FALSE)

  x_vars <- intersect(x_vars, names(df))
  if (!length(x_vars)) stop("No valid `x_vars` were found in `df`.", call. = FALSE)
  model_df <- df[stats::complete.cases(df[, c(time_var, status_var, x_vars), drop = FALSE]), c(time_var, status_var, x_vars), drop = FALSE]
  if (!nrow(model_df)) stop("No complete rows are available for the requested model.", call. = FALSE)
  for (x_var in x_vars) {
    if (is.character(model_df[[x_var]]) || is.factor(model_df[[x_var]])) {
      model_df[[x_var]] <- droplevels(factor(model_df[[x_var]]))
    }
  }

  build_formula <- function(xs) {
    stats::as.formula(
      paste0(
        "survival::Surv(`", time_var, "`, `", status_var, "`) ~ ",
        paste(paste0("`", xs, "`"), collapse = " + ")
      )
    )
  }

  full_formula <- build_formula(x_vars)
  if (requireNamespace("StepReg", quietly = TRUE)) {
    step_fit <- StepReg::stepwise(
      formula = full_formula,
      data = model_df,
      type = "cox",
      strategy = strategy,
      metric = metric,
      sle = as.numeric(sle),
      sls = as.numeric(sls)
    )
    if (!is.null(step_fit$voted_model$model)) {
      final_formula_str <- step_fit$voted_model$model
    } else {
      final_formula_str <- data.frame(step_fit$voted_model)[, 1]
    }
    if (
      is.null(final_formula_str) ||
      !length(final_formula_str) ||
      is.na(final_formula_str[1]) ||
      grepl("~ *1 *$", final_formula_str[1])
    ) {
      stop("Stepwise regression did not select any variables.", call. = FALSE)
    }
    final_fit <- survival::coxph(stats::as.formula(final_formula_str[1]), data = model_df)
  } else {
    full_fit <- survival::coxph(full_formula, data = model_df)
    null_formula <- stats::as.formula(
      paste0("survival::Surv(`", time_var, "`, `", status_var, "`) ~ 1")
    )
    null_fit <- survival::coxph(null_formula, data = model_df)
    direction <- switch(strategy, backward = "backward", forward = "forward", bidirection = "both", both = "both", "both")
    start_fit <- if (direction == "forward") null_fit else full_fit
    final_fit <- stats::step(
      start_fit,
      scope = list(lower = stats::formula(null_fit), upper = stats::formula(full_fit)),
      direction = direction,
      trace = 0
    )
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

  sm <- summary(final_fit)$coefficients
  ci <- suppressMessages(stats::confint(final_fit))
  terms <- rownames(sm)
  if (!length(terms)) stop("The final stepwise model contains no predictor variables.", call. = FALSE)
  result_df <- do.call(rbind, lapply(terms, function(term) {
    variable <- term_variable(term, x_vars)
    data.frame(
      variable = variable,
      level = term_level(term, variable),
      term = term,
      n = final_fit$n,
      hr = exp(unname(sm[term, "coef"])),
      conf_low = exp(unname(ci[term, 1])),
      conf_high = exp(unname(ci[term, 2])),
      p_value = unname(sm[term, "Pr(>|z|)"]),
      stringsAsFactors = FALSE
    )
  }))

  result_df$hr_ci <- sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), result_df$hr, result_df$conf_low, result_df$conf_high)
  result_df$p_value_text <- ifelse(result_df$p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), result_df$p_value))
  result_df$final_formula <- paste(deparse(stats::formula(final_fit)), collapse = " ")

  output <- list(
    final_formula = stats::formula(final_fit),
    table = result_df
  )
  csv_path <- file.path(output_dir, "cox_stepwise_regression.csv")
  utils::write.csv(result_df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  output$csv_path <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)

  if (display %in% c("both", "forest")) {
    require_r_packages(c("autoReg", "ragg"))
    assign(".autoreg_df", model_df, envir = .GlobalEnv)
    on.exit(rm(".autoreg_df", envir = .GlobalEnv), add = TRUE)
    selected_terms <- attr(stats::terms(final_fit), "term.labels")
    if (!length(selected_terms)) stop("The final stepwise model contains no predictor variables, so a forest plot cannot be drawn.", call. = FALSE)
    Surv <- survival::Surv
    plot_formula <- stats::as.formula(
      paste0(
        "Surv(`", time_var, "`, `", status_var, "`) ~ ",
        paste(selected_terms, collapse = " + ")
      )
    )
    plot_fit <- survival::coxph(
      plot_formula,
      data = .autoreg_df
    )
    p <- autoReg::modelPlot(
      plot_fit,
      uni = FALSE,
      multi = TRUE,
      show.ref = TRUE
    )
    png_path <- file.path(output_dir, "cox_stepwise_regression_forest.png")
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

  message("Stepwise Cox regression outputs saved to: ", normalizePath(output_dir))
  invisible(output)
}
