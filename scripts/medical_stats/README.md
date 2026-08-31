# Medical Statistics R Templates

These source templates are copied into each thread under `scripts/medical_stats/`.

This README is an on-demand reference for the medical statistics templates. The canonical agent workflow and modification rules live in `/skills/medical-statistics-standard/SKILL.md`. Read this README when you need field-level configuration details, generated artifact paths, or package requirements.

Edit only the thread-local copies under `scripts/medical_stats/`. Do not edit files under `/skills`.

## Flow Selection

Use `r_flow.R` as the default execution script for simple one-off standard runs.

For actual analyses that should be reproducible, auditable, or compared across configurations, copy `scripts/medical_stats/r_flow.R` to a task-specific flow such as `scripts/medical_stats/r_flow_<task>.R`, edit that copy, and run:

```r
source("scripts/medical_stats/r_flow_<task>.R")
```

When using the agent runtime, execute the flow through `run_r`, for example:

```text
run_r("source('scripts/medical_stats/r_flow_<task>.R')")
```

If multiple incompatible configurations are required, create multiple explicit flow copies rather than repeatedly overwriting `r_flow.R`.

When the workflow is executed through the backend `run_medical_flow` tool, each successful run is also preserved automatically as an immutable snapshot under `scripts/medical_stats/flow_runs/` and recorded in `scripts/medical_stats/flow_history.json`. Reproducible packages should replay those snapshots in history order so a final package can reproduce every generated table, plot, and model run, not only the latest `r_flow.R` configuration.

## Template Files

- `common.R`: shared helpers for package checks, data reading, runtime messages, and Chinese-capable plotting font setup.
- `inspect_data.R`: independent data inspection script using the same `read_medical_data()` reader as `r_flow.R`; it reads the configured data frame and prints the raw `skimr::skim()` result without custom summary columns or extra inspection wrappers.
- `cleaning.R`: editable cleaning template. When enabled, the default `clean_medical_data()` saves and returns the input data.
- `baseline_table.R`: Table 1 function definitions.
- `linear_plots.R`: optional continuous-outcome univariable linear plot function definitions.
- `logistic_plots.R`: optional binary-outcome univariable logistic plot function definitions.
- `logistic_prediction.R`: optional logistic prediction function definitions for dose-group, exposure-value, and response-value scenarios.
- `cox_plots.R`: optional survival univariable Cox/Kaplan-Meier plot function definitions.
- `linear_batch_regression.R`: optional batch univariable/multivariable linear regression function definitions.
- `linear_stepwise_regression.R`: optional stepwise linear regression function definitions.
- `logistic_batch_regression.R`: optional batch univariable/multivariable logistic regression function definitions.
- `logistic_stepwise_regression.R`: optional stepwise logistic regression function definitions.
- `cox_batch_regression.R`: optional batch univariable/multivariable Cox regression function definitions.
- `cox_stepwise_regression.R`: optional stepwise Cox regression function definitions.
- `r_flow.R`: default execution script with workflow step switches, one selected `regression_family`, a grouped `USER CONFIG` block, and workflow helper functions.

## Run Switches

All workflow steps default to disabled.

- `run_cleaning`: set `TRUE` to run `clean_medical_data()` from `cleaning.R`.
- `run_table1`: set `TRUE` to generate Table 1 outputs.
- `regression_family`: set exactly one of `"linear"`, `"logistic"`, or `"cox"` when running plots, logistic prediction, batch regression, or stepwise regression. Keep `NULL` otherwise.
- `run_univariable_plots`: set `TRUE` to generate univariable plots for the selected `regression_family`.
- `run_logistic_prediction`: set `TRUE` to generate logistic prediction CSV outputs. Requires `regression_family <- "logistic"`.
- `run_batch_regression`: set `TRUE` to generate batch univariable and/or multivariable regression summaries for the selected family.
- `run_stepwise_regression`: set `TRUE` to generate stepwise regression summaries for the selected family.

Do not run multiple regression families in one flow execution. Use separate task-specific flow copies when multiple families are explicitly needed.

## USER CONFIG Reference

### General

- `data_path`: thread-relative input data path, for example `data/input.csv`, `data/input.xlsx`, or `output/data/cleaned_data.rds`.

### Cleaning

- `cleaned_output_dir`: cleaned data output directory, used only when `run_cleaning <- TRUE`.

Cleaning behavior belongs in `cleaning.R` or a task-specific cleaning template copy. Do not hard-code cleaning parameters in `r_flow.R`.

### Table 1

- `x_vars`: baseline variables to include in Table 1.
- `stratify_var`: optional grouping variable or character vector of grouping variables; use `NULL` for no stratification.
- `table1_output_dir`: Table 1 output directory.

### Linear Plots and Regression

- `linear_y_var`: numeric outcome variable for univariable linear plots or linear regression summaries.
- `linear_x_vars`: predictors for linear regression summaries; also used for univariable linear plots when `linear_plot_x_vars` is empty.
- `linear_plot_x_vars`: predictors for univariable linear plots only; keep empty to default to `linear_x_vars`.
- `linear_group_var`: optional color grouping variable for univariable linear plots.
- `linear_plot_dir`: PNG output directory for univariable linear plots.
- `linear_batch_output_dir`: CSV and PNG output directory for batch linear regression.
- `linear_stepwise_output_dir`: CSV and PNG output directory for stepwise linear regression.

### Logistic Plots, Prediction, and Regression

- `logistic_y_var`: binary outcome variable for univariable logistic plots, logistic prediction, or logistic regression summaries.
- `logistic_x_vars`: predictors for univariable logistic plots, logistic prediction, or logistic regression summaries.
- `logistic_group_var`: optional color grouping variable for univariable logistic plots.
- `logistic_plot_dir`: PNG output directory for univariable logistic plots.
- `logistic_prediction_dose_var`: optional dose/group variable for logistic prediction by dose group.
- `logistic_prediction_exposure_values`: optional numeric exposure values for predicted response probabilities.
- `logistic_prediction_response_values`: optional target response probabilities between 0 and 1 for reverse exposure prediction.
- `logistic_prediction_output_dir`: CSV output directory for logistic prediction.
- `logistic_batch_output_dir`: CSV and PNG output directory for batch logistic regression.
- `logistic_stepwise_output_dir`: CSV and PNG output directory for stepwise logistic regression.

### Cox/Kaplan-Meier Plots and Cox Regression

- `cox_time_var`: survival time variable for Cox/Kaplan-Meier plots and Cox regression summaries.
- `cox_status_var`: event status variable coded 0/1.
- `cox_group_vars`: grouping variables for Cox/Kaplan-Meier plots.
- `cox_facet_var`: optional facet variable for Cox/Kaplan-Meier plots.
- `cox_x_vars`: predictors for Cox regression summaries. Cox/Kaplan-Meier plots use `cox_group_vars`.
- `cox_plot_dir`: PNG output directory for Cox/Kaplan-Meier plots.
- `cox_batch_output_dir`: CSV and PNG output directory for batch Cox regression.
- `cox_stepwise_output_dir`: CSV and PNG output directory for stepwise Cox regression.

### Regression Summary Controls

- `batch_model_method`: `"univariable"`, `"multivariable"`, or `c("univariable", "multivariable")` for the selected family. Default `c("univariable", "multivariable")` runs both.
- `batch_display`: `"both"`, `"table"`, or `"forest"`. Default `"both"` writes the regression table CSV and corresponding forest plot. `"forest"` only suppresses the rendered table view; it still writes the CSV evidence companion for the forest plot.
- `stepwise_strategy`: `"bidirection"`, `"backward"`, `"forward"`, or `"both"`.
- `stepwise_display`: `"both"`, `"table"`, or `"forest"`. Default `"both"` writes the regression table CSV and corresponding forest plot. `"forest"` only suppresses the rendered table view; it still writes the CSV evidence companion for the forest plot.
- `forest_plot_width`: `NULL` or a positive number in inches. `NULL` auto-calculates regression forest plot width from result text length; a number manually overrides all regression forest plots in the flow.
- `forest_plot_height`: `NULL` or a positive number in inches. `NULL` auto-calculates regression forest plot height from result rows; a number manually overrides all regression forest plots in the flow.
- `plot_show_p_value`: `TRUE` or `FALSE`; controls whether univariable plots show P values in titles or plot annotations without editing plot template files.

## Common Configuration Examples

### Table 1

```r
run_table1 <- TRUE
data_path <- "data/input.csv"
x_vars <- c("age", "sex", "weight")
stratify_var <- "arm"
table1_output_dir <- "output/descriptive"
```

### Linear plots and regression

```r
regression_family <- "linear"
run_univariable_plots <- TRUE
run_batch_regression <- TRUE
batch_model_method <- c("univariable", "multivariable")

linear_y_var <- "Y"
linear_x_vars <- c("age", "weight")
linear_plot_x_vars <- "age"
linear_group_var <- NULL
```

### Logistic plots, prediction, and stepwise regression

```r
regression_family <- "logistic"
run_univariable_plots <- TRUE
run_logistic_prediction <- TRUE
run_stepwise_regression <- TRUE

logistic_y_var <- "outcome"
logistic_x_vars <- c("age", "risk_score")
logistic_prediction_exposure_values <- c(10, 20, 30)
logistic_prediction_response_values <- c(0.25, 0.5, 0.75)
```

### Cox/Kaplan-Meier plots and Cox regression

```r
regression_family <- "cox"
run_univariable_plots <- TRUE
run_batch_regression <- TRUE
run_stepwise_regression <- TRUE

cox_time_var <- "time"
cox_status_var <- "status"
cox_group_vars <- c("arm")
cox_x_vars <- c("age", "arm")
```

## Generated Artifacts

### Cleaning

When `run_cleaning <- TRUE`, the flow writes:

- `output/data/cleaned_data.csv`
- `output/data/cleaned_data.rds`

The directory is controlled by `cleaned_output_dir`.

### Table 1

When `run_table1 <- TRUE`, the flow writes:

- `output/descriptive/table1.csv`
- `output/descriptive/table1.html`

The directory is controlled by `table1_output_dir`.

### Univariable Plots

When `regression_family <- "linear"` and `run_univariable_plots <- TRUE`, the flow writes PNG files under `linear_plot_dir`, such as:

- `output/figures/linear/*.png`
- `output/figures/linear/plot_statistics.csv`
- `output/figures/linear/plot_descriptive_summary.csv`

When `regression_family <- "logistic"` and `run_univariable_plots <- TRUE`, the flow writes PNG files under `logistic_plot_dir`, such as:

- `output/figures/logistic/*.png`
- `output/figures/logistic/plot_statistics.csv`
- `output/figures/logistic/plot_observed_summary.csv`
- `output/figures/logistic/plot_descriptive_summary.csv` when a numeric predictor is paired with a grouping boxplot.

Logistic plots show observed probability intervals by default. Numeric predictors use binned observed probabilities with exact binomial confidence intervals to match the legacy `xgxr::xgx_stat_ci(distribution = "binomial")` behavior. Categorical predictors use the legacy `mean_cl_boot` point-interval behavior in the PNG, while `plot_observed_summary.csv` records exact binomial event rates and confidence intervals for structured interpretation. Change the thread-local `logistic_plots.R` function defaults `show_observed_ci` or `observed_ci_bins` only when this detail needs adjustment.

When `regression_family <- "cox"` and `run_univariable_plots <- TRUE`, the flow writes PNG files under `cox_plot_dir`, such as:

- `output/figures/cox/*.png`
- `output/figures/cox/plot_statistics.csv`
- `output/figures/cox/plot_survival_table.csv`

Cox plots use `survminer::ggsurvplot()` defaults for P-value placement, show a main Kaplan-Meier title by default, use `Time` as the default x-axis title, and include both the risk table and cumulative censoring table (`cumcensor`) by default. `plot_survival_table.csv` stores the textual evidence for these lower tables, including `n_risk`, `n_event`, `n_censor`, `cum_censor`, survival probability, and confidence limits by stratum and time.

Use `plot_show_p_value <- FALSE` in `r_flow.R` when the confirmed univariable plot configuration should hide P values while keeping template defaults unchanged.

### Logistic Prediction

When `regression_family <- "logistic"` and `run_logistic_prediction <- TRUE`, the flow writes CSV files under `logistic_prediction_output_dir` for enabled scenarios:

- by dose group
- by exposure value
- by response value

### Regression Summaries

When `regression_family <- "linear"` and `run_batch_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `linear_batch_output_dir`.

When `regression_family <- "linear"` and `run_stepwise_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `linear_stepwise_output_dir`.

When `regression_family <- "logistic"` and `run_batch_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `logistic_batch_output_dir`.

When `regression_family <- "logistic"` and `run_stepwise_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `logistic_stepwise_output_dir`.

When `regression_family <- "cox"` and `run_batch_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `cox_batch_output_dir`.

When `regression_family <- "cox"` and `run_stepwise_regression <- TRUE`, the flow writes CSV and corresponding PNG forest files under `cox_stepwise_output_dir`.

## Cleaning Defaults

Default binning in `cleaning.R` uses quantile bins with factor interval labels. For a column such as `Age` split into 4 bins, create `Age_Q4` with `ggplot2::cut_number(Age, n = 4)` or the template helper `add_quantile_bin(df, "Age", 4)`.

Values are factor levels such as `(20, 44]`, ordered from low to high by interval, not integer bin IDs.

## R Packages

The templates require these R packages:

- `readr`
- `readxl`
- `tibble`
- `stringr`
- `table1`
- `ggplot2`
- `ragg`
- `skimr`
- `showtext`
- `survival`
- `survminer`

Additional package requirements:
- Logistic observed probability intervals require `binom` and `Hmisc`.
- Grouped numeric logistic plots require `patchwork`.
- Forest-plot regression outputs require `forestploter`.
- Stepwise linear, logistic, and Cox regression use `StepReg` when available and fall back to `stats::step()`.

`common.R` configures Chinese-capable plot fonts with fixed `wqy-microhei`, `showtext::showtext_auto()`, and `showtext::showtext_opts(dpi = 300)` to match standard `ragg` PNG output. Make that font available in the runtime because the standard template does not search alternatives or fall back to another family.
