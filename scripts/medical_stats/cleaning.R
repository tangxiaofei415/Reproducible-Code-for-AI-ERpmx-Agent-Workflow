clean_medical_data <- function(df, output_dir = NULL) {
  require_r_packages(c("readr", "tibble", "dplyr", "ggplot2"))

  df <- tibble::as_tibble(df)

  add_quantile_bin <- function(data, var, n) {
    new_col <- paste0(var, "_Q", n)
    data |>
      dplyr::mutate(
        "{new_col}" := {
          bins <- ggplot2::cut_number(.data[[var]], n = n)
          factor(bins, levels = levels(bins), ordered = FALSE)
        }
      )
  }

  # example: type_conversions
  # df <- df |>
  #   dplyr::mutate(
  #     char_col = as.character(any_col),
  #     factor_col = forcats::as_factor(category_col),
  #     int_from_char = readr::parse_integer(raw_int_string_col),
  #     num_from_char = readr::parse_double(raw_num_string_col),
  #     bool_from_char = readr::parse_logical(raw_bool_string_col),
  #     date_from_char = lubridate::ymd(raw_date_string_col),
  #     dt_from_char = lubridate::ymd_hms(raw_datetime_string_col),
  #     int_from_factor = readr::parse_integer(as.character(factorized_int_string_col)),
  #     int_from_num = as.integer(num_col),
  #     num_from_int = as.double(int_col),
  #     int_from_bool = as.integer(bool_col),
  #     num_from_bool = as.double(bool_col)
  #   )

  # example: factor_level_modifications
  # df <- df |>
  #   dplyr::mutate(
  #     factor_col = forcats::fct_relevel(factor_col, "level_a", "level_b", "level_c", after = 0),
  #     factor_col = forcats::fct_recode(factor_col, new_level = "old_level"),
  #     factor_col = forcats::fct_collapse(factor_col, combined_level = c("old_level1", "old_level2")),
  #     factor_col = forcats::fct_lump_n(factor_col, n = 5, other_level = "Other"),
  #     factor_col = forcats::fct_drop(factor_col),
  #     factor_col = forcats::fct_infreq(factor_col),
  #     factor_col = forcats::fct_rev(factor_col)
  #   )

  # example: quantile_bins
  # Default binning convention:
  # - quantile bins via ggplot2::cut_number()
  # - new column name is <original_column>_Q<n>
  # - values are factor interval labels, e.g. Age_Q4 contains levels such as (20, 44]
  # - factor levels follow the interval order from low to high
  # df <- df |>
  #   dplyr::mutate(
  #     Age_Q4 = {
  #       bins <- ggplot2::cut_number(Age, n = 4)
  #       factor(bins, levels = levels(bins), ordered = FALSE)
  #     }
  #   )
  # df <- add_quantile_bin(df, "Age", 4)

  # example: omit_missing_values
  # df <- df |> tidyr::drop_na() # drops rows with any NA in any column
  # df <- df |> dplyr::filter(!is.na(some_col)) # drops rows with NA in specific column(s)

  if (!is.null(output_dir) && nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(df, file.path(output_dir, "cleaned_data.csv"))
    saveRDS(df, file.path(output_dir, "cleaned_data.rds"))
  }

  df
}
