# BioFlow
# Default medical statistics flow template.
#
# For simple one-off standard runs, edit this thread-local script, then run:
# run_r("source('scripts/medical_stats/r_flow.R')")
#
# For actual analyses that should be reproducible, auditable, or compared
# against other configurations, prefer copying this file to a task-specific
# flow such as `scripts/medical_stats/r_flow_<task>.R`, edit that copy, then run:
# run_r("source('scripts/medical_stats/r_flow_<task>.R')")
#
# Keep template source files as function definitions only. This flow is the
# orchestration layer for selected variables, run switches, and output paths.

sourced_template_files <- c(
  "scripts/medical_stats/common.R",
  "scripts/medical_stats/cleaning.R",
  "scripts/medical_stats/baseline_table.R",
  "scripts/medical_stats/linear_plots.R",
  "scripts/medical_stats/logistic_plots.R",
  "scripts/medical_stats/logistic_prediction.R",
  "scripts/medical_stats/cox_plots.R",
  "scripts/medical_stats/linear_batch_regression.R",
  "scripts/medical_stats/linear_stepwise_regression.R",
  "scripts/medical_stats/logistic_batch_regression.R",
  "scripts/medical_stats/logistic_stepwise_regression.R",
  "scripts/medical_stats/cox_batch_regression.R",
  "scripts/medical_stats/cox_stepwise_regression.R"
)

for (template_file in sourced_template_files) {
  source(template_file)
}

# ---- RUN STEPS ----
run_cleaning <- FALSE
run_table1 <- TRUE
regression_family <- NULL # "linear", "logistic", "cox", or NULL. Required for plots, prediction, or regression summaries.
run_univariable_plots <- FALSE
run_logistic_prediction <- FALSE # Requires regression_family <- "logistic".
run_batch_regression <- FALSE
run_stepwise_regression <- FALSE

# ---- USER CONFIG START ----
# General
data_path <- "output/data/cleaned_data.rds" # Thread-relative data path, e.g. "data/input.csv" or "output/data/cleaned_data.rds".

# Cleaning
cleaned_output_dir <- "output/data" # Thread-relative cleaned data output directory.

# Table 1
x_vars <- c("Age", "Weight", "BMI", "Scr", "eGFR") # Baseline variables to include in Table 1.
stratify_var <- "DOSE" # Optional grouping variable or c("group1", "group2"); keep NULL for no stratification.
table1_output_dir <- "output/descriptive" # Thread-relative Table 1 output directory.

# Linear
linear_y_var <- NULL # Optional numeric outcome for linear plots or regression summaries.
linear_x_vars <- character() # Optional predictors for linear regression summaries; also used for linear plots when linear_plot_x_vars is empty.
linear_plot_x_vars <- character() # Optional predictors for linear plots; defaults to linear_x_vars when empty.
linear_group_var <- NULL # Optional color grouping variable for linear plots.

# Logistic
logistic_y_var <- NULL # Optional binary outcome for logistic plots, prediction, or regression summaries.
logistic_x_vars <- character() # Optional predictors for logistic plots or regression summaries.
logistic_group_var <- NULL # Optional color grouping variable for logistic plots.
logistic_prediction_dose_var <- NULL # Optional dose/group variable for logistic prediction by dose group.
logistic_prediction_exposure_values <- numeric() # Optional exposure values for predicted response probabilities.
logistic_prediction_response_values <- numeric() # Optional target response probabilities for reverse exposure prediction.

# Cox
cox_time_var <- NULL # Optional survival time variable for Cox/KM plots and regression summaries.
cox_status_var <- NULL # Optional event status variable coded 0/1.
cox_group_vars <- character() # Optional grouping variables for Cox/KM plots.
cox_facet_var <- NULL # Optional facet variable for Cox/KM plots.
cox_x_vars <- character() # Optional predictors for Cox regression summaries.

# Regression
batch_model_method <- c("univariable", "multivariable") # "univariable", "multivariable", or c("univariable", "multivariable"). Keep both unless only one model type is requested.
batch_display <- "both" # "both", "table", or "forest". Keep "both" unless only one output type is requested.
stepwise_strategy <- "bidirection" # "bidirection", "backward", "forward", or "both".
stepwise_display <- "both" # "both", "table", or "forest". Keep "both" unless only one output type is requested.
forest_plot_width <- NULL # NULL auto-calculates; set a positive number in inches to override all regression forest plots in this flow.
forest_plot_height <- NULL # NULL auto-calculates; set a positive number in inches to override all regression forest plots in this flow.
plot_show_p_value <- TRUE # Whether univariable plots show P values in titles or plot annotations.

# Outputs
linear_plot_dir <- "output/figures/linear" # Thread-relative plot output directory.
logistic_plot_dir <- "output/figures/logistic" # Thread-relative logistic plot output directory.
logistic_prediction_output_dir <- "output/prediction/logistic" # Thread-relative logistic prediction output directory.
cox_plot_dir <- "output/figures/cox" # Thread-relative Cox/KM plot output directory.
linear_batch_output_dir <- "output/regression/linear_batch" # Thread-relative batch regression output directory.
linear_stepwise_output_dir <- "output/regression/linear_stepwise" # Thread-relative stepwise regression output directory.
logistic_batch_output_dir <- "output/regression/logistic_batch" # Thread-relative batch logistic regression output directory.
logistic_stepwise_output_dir <- "output/regression/logistic_stepwise" # Thread-relative stepwise logistic regression output directory.
cox_batch_output_dir <- "output/regression/cox_batch" # Thread-relative batch Cox regression output directory.
cox_stepwise_output_dir <- "output/regression/cox_stepwise" # Thread-relative stepwise Cox regression output directory.
# ---- USER CONFIG END ----

run_cleaning_workflow <- function(df) {
  df <- clean_medical_data(
    df = df,
    output_dir = cleaned_output_dir
  )
  log_msg("Wrote cleaned data outputs to: ", cleaned_output_dir)
  df
}

run_table1_workflow <- function(df) {
  generate_table1_outputs(
    df = df,
    x_vars = x_vars,
    stratify_var = stratify_var,
    output_dir = table1_output_dir
  )
  log_msg("Wrote Table 1 outputs to: ", table1_output_dir)
  invisible(NULL)
}

batch_methods_for_run <- function() {
  batch_methods <- unique(as.character(batch_model_method))
  if (!length(batch_methods) || any(!batch_methods %in% c("univariable", "multivariable"))) {
    stop('Agent-friendly config error: `batch_model_method` must contain only "univariable" and/or "multivariable".', call. = FALSE)
  }
  batch_methods
}

run_linear_workflow <- function(df) {
  linear_model_y_var <- linear_y_var
  linear_model_x_vars <- linear_x_vars
  linear_plot_vars <- if (length(linear_plot_x_vars) > 0) linear_plot_x_vars else linear_model_x_vars
  linear_model_group_var <- linear_group_var

  if (isTRUE(run_univariable_plots)) {
    plot_univariable_linear(
      df = df,
      y_var = linear_model_y_var,
      x_vars = linear_plot_vars,
      group_var = linear_model_group_var,
      output_dir = linear_plot_dir,
      show_p_value = plot_show_p_value
    )
    log_msg("Wrote linear plots to: ", linear_plot_dir)
  }

  if (isTRUE(run_batch_regression)) {
    for (batch_method in batch_methods_for_run()) {
      generate_batch_linear_regression(
        df = df,
        y_var = linear_model_y_var,
        x_vars = linear_model_x_vars,
        model_method = batch_method,
        display = batch_display,
        output_dir = linear_batch_output_dir,
        width = forest_plot_width,
        height = forest_plot_height
      )
    }
    log_msg("Wrote batch linear regression outputs to: ", linear_batch_output_dir)
  }

  if (isTRUE(run_stepwise_regression)) {
    generate_stepwise_linear_regression(
      df = df,
      y_var = linear_model_y_var,
      x_vars = linear_model_x_vars,
      strategy = stepwise_strategy,
      display = stepwise_display,
      output_dir = linear_stepwise_output_dir,
      width = forest_plot_width,
      height = forest_plot_height
    )
    log_msg("Wrote stepwise linear regression outputs to: ", linear_stepwise_output_dir)
  }
}

run_logistic_workflow <- function(df) {
  logistic_model_y_var <- logistic_y_var
  logistic_model_x_vars <- logistic_x_vars
  logistic_model_group_var <- logistic_group_var

  if (isTRUE(run_univariable_plots)) {
    plot_univariable_logistic(
      df = df,
      y_var = logistic_model_y_var,
      x_vars = logistic_model_x_vars,
      group_var = logistic_model_group_var,
      output_dir = logistic_plot_dir,
      show_p_value = plot_show_p_value
    )
    log_msg("Wrote logistic plots to: ", logistic_plot_dir)
  }

  if (isTRUE(run_logistic_prediction)) {
    generate_logistic_prediction(
      df = df,
      y_var = logistic_model_y_var,
      x_vars = logistic_model_x_vars,
      dose_var = logistic_prediction_dose_var,
      exposure_values = logistic_prediction_exposure_values,
      response_values = logistic_prediction_response_values,
      output_dir = logistic_prediction_output_dir
    )
    log_msg("Wrote logistic prediction outputs to: ", logistic_prediction_output_dir)
  }

  if (isTRUE(run_batch_regression)) {
    for (batch_method in batch_methods_for_run()) {
      generate_batch_logistic_regression(
        df = df,
        y_var = logistic_model_y_var,
        x_vars = logistic_model_x_vars,
        model_method = batch_method,
        display = batch_display,
        output_dir = logistic_batch_output_dir,
        width = forest_plot_width,
        height = forest_plot_height
      )
    }
    log_msg("Wrote batch logistic regression outputs to: ", logistic_batch_output_dir)
  }

  if (isTRUE(run_stepwise_regression)) {
    generate_stepwise_logistic_regression(
      df = df,
      y_var = logistic_model_y_var,
      x_vars = logistic_model_x_vars,
      strategy = stepwise_strategy,
      display = stepwise_display,
      output_dir = logistic_stepwise_output_dir,
      width = forest_plot_width,
      height = forest_plot_height
    )
    log_msg("Wrote stepwise logistic regression outputs to: ", logistic_stepwise_output_dir)
  }
}

run_cox_workflow <- function(df) {
  cox_model_time_var <- cox_time_var
  cox_model_status_var <- cox_status_var
  cox_model_group_vars <- cox_group_vars
  cox_model_facet_var <- cox_facet_var
  cox_model_x_vars <- cox_x_vars

  if (isTRUE(run_univariable_plots)) {
    plot_univariable_cox(
      df = df,
      time_var = cox_model_time_var,
      status_var = cox_model_status_var,
      group_vars = cox_model_group_vars,
      facet_var = cox_model_facet_var,
      output_dir = cox_plot_dir,
      show_p_value = plot_show_p_value
    )
    log_msg("Wrote Cox plots to: ", cox_plot_dir)
  }

  if (isTRUE(run_batch_regression)) {
    for (batch_method in batch_methods_for_run()) {
      generate_batch_cox_regression(
        df = df,
        time_var = cox_model_time_var,
        status_var = cox_model_status_var,
        x_vars = cox_model_x_vars,
        model_method = batch_method,
        display = batch_display,
        output_dir = cox_batch_output_dir,
        width = forest_plot_width,
        height = forest_plot_height
      )
    }
    log_msg("Wrote batch Cox regression outputs to: ", cox_batch_output_dir)
  }

  if (isTRUE(run_stepwise_regression)) {
    generate_stepwise_cox_regression(
      df = df,
      time_var = cox_model_time_var,
      status_var = cox_model_status_var,
      x_vars = cox_model_x_vars,
      strategy = stepwise_strategy,
      display = stepwise_display,
      output_dir = cox_stepwise_output_dir,
      width = forest_plot_width,
      height = forest_plot_height
    )
    log_msg("Wrote stepwise Cox regression outputs to: ", cox_stepwise_output_dir)
  }
}

run_regression_workflows <- function(df) {
  if (!(isTRUE(run_univariable_plots) || isTRUE(run_logistic_prediction) || isTRUE(run_batch_regression) || isTRUE(run_stepwise_regression))) {
    return(invisible(NULL))
  }

  if (is.null(regression_family) || identical(regression_family, "")) {
    stop('Set regression_family to "linear", "logistic", or "cox" before running plots, logistic prediction, or regression summaries. Agent-friendly config error: configure the single selected family near the run switches.', call. = FALSE)
  }
  if (!regression_family %in% c("linear", "logistic", "cox")) {
    stop('regression_family must be one of "linear", "logistic", or "cox". Agent-friendly config error: choose exactly one supported family.', call. = FALSE)
  }
  if (isTRUE(run_logistic_prediction) && regression_family != "logistic") {
    stop('Agent-friendly config error: `run_logistic_prediction` requires `regression_family <- "logistic"`.', call. = FALSE)
  }

  if (regression_family == "linear") run_linear_workflow(df)
  if (regression_family == "logistic") run_logistic_workflow(df)
  if (regression_family == "cox") run_cox_workflow(df)
  invisible(NULL)
}

run_medical_stats_flow <- function() {
  log_msg("Reading data: ", data_path)
  df <- read_medical_data(data_path)

  if (isTRUE(run_cleaning)) {
    df <- run_cleaning_workflow(df)
  }
  if (isTRUE(run_table1)) {
    run_table1_workflow(df)
  }
  run_regression_workflows(df)
  invisible(df)
}

run_medical_stats_flow()
