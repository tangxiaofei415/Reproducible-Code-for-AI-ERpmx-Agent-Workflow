# generate_logistic_prediction(
#   df = df,
#   y_var = "Y",
#   x_vars = c("AUC1", "Cmax1"),
#   dose_var = "DOSE",
#   exposure_values = c(20, 40),
#   response_values = c(0.25, 0.5),
#   output_dir = "logistic_prediction"
# )
generate_logistic_prediction <- function(
    df = df,
    y_var = NULL,
    x_vars = NULL,
    dose_var = NULL,
    exposure_values = numeric(),
    response_values = numeric(),
    output_dir = "logistic_prediction",
    link_function = "logit",
    confidence_level = 0.95,
    digits = 4) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (is.null(y_var) || !y_var %in% names(df)) stop("`y_var` was not found in `df`.", call. = FALSE)
  if (is.null(x_vars) || !length(x_vars)) stop("`x_vars` must include at least one exposure variable.", call. = FALSE)
  missing_x <- setdiff(x_vars, names(df))
  if (length(missing_x)) stop("Exposure variables were not found in `df`: ", paste(missing_x, collapse = ", "), call. = FALSE)
  if (!is.null(dose_var) && nzchar(dose_var) && !dose_var %in% names(df)) stop("`dose_var` was not found in `df`.", call. = FALSE)
  if (!length(exposure_values) && !length(response_values) && (is.null(dose_var) || !nzchar(dose_var))) {
    stop("Provide at least one of `dose_var`, `exposure_values`, or `response_values`.", call. = FALSE)
  }
  if (length(exposure_values) && !is.numeric(exposure_values)) stop("`exposure_values` must be numeric.", call. = FALSE)
  if (length(response_values) && !is.numeric(response_values)) stop("`response_values` must be numeric probabilities.", call. = FALSE)
  if (!is.numeric(confidence_level) || length(confidence_level) != 1L || confidence_level <= 0 || confidence_level >= 1) {
    stop("`confidence_level` must be a single numeric value between 0 and 1.", call. = FALSE)
  }
  if (length(exposure_values) && any(!is.finite(exposure_values))) stop("`exposure_values` must be finite.", call. = FALSE)
  if (length(response_values) && link_function != "logit") {
    stop("Reverse prediction from response values currently requires `link_function = \"logit\"`.", call. = FALSE)
  }

  z_value <- stats::qnorm(1 - (1 - confidence_level) / 2)
  link_inverse <- stats::binomial(link = link_function)$linkinv

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

  model_for <- function(x_var, extra_vars = character()) {
    target_vars <- unique(c(y_var, x_var, extra_vars))
    complete_rows <- stats::complete.cases(df[, target_vars, drop = FALSE])
    model_df <- df[complete_rows, target_vars, drop = FALSE]
    if (!nrow(model_df)) stop("No complete rows are available for `", y_var, "` and `", x_var, "`.", call. = FALSE)
    if (!is.numeric(model_df[[x_var]])) stop("Exposure variable `", x_var, "` must be numeric for logistic prediction.", call. = FALSE)
    model_df$y_binary <- to_binary(model_df[[y_var]])
    if (length(unique(model_df$y_binary)) != 2L) stop("`y_var` must have two observed outcome levels after missing-value filtering.", call. = FALSE)
    fit <- stats::glm(
      stats::reformulate(x_var, response = "y_binary"),
      data = model_df,
      family = stats::binomial(link = link_function)
    )
    list(fit = fit, data = model_df)
  }

  predict_from_exposure <- function(fit, x_var, values) {
    pred_data <- data.frame(value = values)
    names(pred_data) <- x_var
    pred_link <- stats::predict(fit, newdata = pred_data, type = "link", se.fit = TRUE)
    data.frame(
      Exposure = values,
      Prediction = round(link_inverse(pred_link$fit), digits),
      Pred_95CI_Lower = round(link_inverse(pred_link$fit - z_value * pred_link$se.fit), digits),
      Pred_95CI_Upper = round(link_inverse(pred_link$fit + z_value * pred_link$se.fit), digits),
      stringsAsFactors = FALSE
    )
  }

  results <- list()

  if (!is.null(dose_var) && nzchar(dose_var)) {
    dose_results <- do.call(rbind, lapply(x_vars, function(x_var) {
      model_info <- model_for(x_var, dose_var)
      dose_df <- model_info$data
      groups <- sort(unique(dose_df[[dose_var]]))
      one_x <- do.call(rbind, lapply(groups, function(group_value) {
        group_data <- dose_df[dose_df[[dose_var]] == group_value, , drop = FALSE]
        exposure <- group_data[[x_var]]
        median_exp <- stats::median(exposure, na.rm = TRUE)
        se_exp <- if (length(exposure) > 1L) stats::sd(exposure, na.rm = TRUE) / sqrt(length(exposure)) else 0
        exposure_ci <- c(median_exp, median_exp - z_value * se_exp, median_exp + z_value * se_exp)
        pred_data <- data.frame(value = exposure_ci)
        names(pred_data) <- x_var
        pred <- stats::predict(model_info$fit, newdata = pred_data, type = "response")
        data.frame(
          Predictor = x_var,
          Group = group_value,
          Median_Exposure = round(median_exp, digits),
          Exposure_95CI_Lower = round(exposure_ci[2], digits),
          Exposure_95CI_Upper = round(exposure_ci[3], digits),
          Prediction = round(pred[1], digits),
          Pred_95CI_Lower = round(pred[2], digits),
          Pred_95CI_Upper = round(pred[3], digits),
          stringsAsFactors = FALSE
        )
      }))
      one_x[order(one_x$Median_Exposure), , drop = FALSE]
    }))
    dose_path <- file.path(output_dir, "logistic_prediction_by_dose_group.csv")
    utils::write.csv(dose_results, dose_path, row.names = FALSE)
    results$by_dose_group <- dose_results
  }

  if (length(exposure_values)) {
    exposure_results <- do.call(rbind, lapply(x_vars, function(x_var) {
      model_info <- model_for(x_var)
      one_x <- predict_from_exposure(model_info$fit, x_var, exposure_values)
      data.frame(Predictor = x_var, one_x, stringsAsFactors = FALSE)
    }))
    exposure_path <- file.path(output_dir, "logistic_prediction_by_exposure_value.csv")
    utils::write.csv(exposure_results, exposure_path, row.names = FALSE)
    results$by_exposure_value <- exposure_results
  }

  if (length(response_values)) {
    invalid_response <- !is.finite(response_values) | response_values <= 0 | response_values >= 1
    if (any(invalid_response)) {
      warning("`response_values` must be between 0 and 1; invalid values were skipped.", call. = FALSE)
      response_values <- response_values[!invalid_response]
    }
    if (length(response_values)) {
      response_results <- do.call(rbind, lapply(x_vars, function(x_var) {
        model_info <- model_for(x_var)
        coefs <- stats::coef(model_info$fit)
        if (length(coefs) < 2L || is.na(coefs[2]) || coefs[2] == 0) {
          stop("Cannot reverse-predict exposure for `", x_var, "` because the model slope is zero or unavailable.", call. = FALSE)
        }
        vcov_matrix <- stats::vcov(model_info$fit)
        one_x <- do.call(rbind, lapply(response_values, function(response_value) {
          required_exposure <- (stats::qlogis(response_value) - coefs[1]) / coefs[2]
          se <- sqrt(vcov_matrix[1, 1] + required_exposure^2 * vcov_matrix[2, 2] + 2 * required_exposure * vcov_matrix[1, 2])
          data.frame(
            Predictor = x_var,
            Target_Response = response_value,
            Estimated_Exposure = round(required_exposure, digits),
            Exposure_95CI_Lower = round(max(0, required_exposure - z_value * se), digits),
            Exposure_95CI_Upper = round(required_exposure + z_value * se, digits),
            stringsAsFactors = FALSE
          )
        }))
        one_x
      }))
      response_path <- file.path(output_dir, "logistic_prediction_by_response_value.csv")
      utils::write.csv(response_results, response_path, row.names = FALSE)
      results$by_response_value <- response_results
    }
  }

  if (!length(results)) stop("No logistic prediction outputs were generated.", call. = FALSE)
  message("Logistic prediction outputs saved to: ", normalizePath(output_dir))
  invisible(results)
}
