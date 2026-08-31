require_r_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing required R package(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

validate_data_path <- function(data_path) {
  require_r_packages(c("stringr"))

  if (is.null(data_path) || !nzchar(as.character(data_path))) {
    stop("config$data_path is required.", call. = FALSE)
  }
  if (stringr::str_detect(data_path, stringr::regex("^/|\\.\\."))) {
    stop("config$data_path must be a thread-relative path without '..'.", call. = FALSE)
  }
}

read_medical_data <- function(data_path) {
  require_r_packages(c("readr", "readxl", "tibble", "stringr"))
  validate_data_path(data_path)

  extension <- stringr::str_to_lower(tools::file_ext(data_path))
  data <- switch(
    extension,
    csv = readr::read_csv(data_path, show_col_types = FALSE, progress = FALSE),
    tsv = readr::read_tsv(data_path, show_col_types = FALSE, progress = FALSE),
    xls = readxl::read_excel(data_path),
    xlsx = readxl::read_excel(data_path),
    rds = readr::read_rds(data_path),
    stop("Unsupported data file type. Supported: .csv, .tsv, .xls, .xlsx, .rds", call. = FALSE)
  )

  tibble::as_tibble(data)
}

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  message(line)
}

resolve_forest_plot_size <- function(
    result_df,
    width = NULL,
    height = NULL,
    default_width = 8,
    default_height = 6,
    min_width = default_width,
    min_height = default_height,
    max_width = 16,
    max_height = 24,
    base_width = 6.2,
    base_height = 1.2,
    char_width = 0.055,
    row_height = 0.35,
    add_reference_rows = FALSE) {
  resolve_dimension <- function(value, automatic, name) {
    if (is.null(value) || !length(value) || all(is.na(value))) return(automatic)
    numeric_value <- suppressWarnings(as.numeric(value[1]))
    if (!is.finite(numeric_value) || numeric_value <= 0) {
      stop("`", name, "` must be NULL or a positive numeric value in inches.", call. = FALSE)
    }
    numeric_value
  }

  if (!is.data.frame(result_df)) result_df <- as.data.frame(result_df)
  rows <- nrow(result_df)
  if (isTRUE(add_reference_rows) && all(c("variable", "level") %in% names(result_df))) {
    level_values <- as.character(result_df$level)
    variable_values <- as.character(result_df$variable)
    reference_vars <- unique(variable_values[!is.na(level_values) & nzchar(level_values)])
    rows <- rows + length(reference_vars)
  }
  rows <- max(1L, rows)

  text_columns <- intersect(
    c("variable", "level", "term", "beta_ci", "or_ci", "hr_ci", "p_value_text"),
    names(result_df)
  )
  text_values <- if (length(text_columns)) {
    unlist(lapply(result_df[text_columns], as.character), use.names = FALSE)
  } else {
    character()
  }
  text_values <- text_values[!is.na(text_values)]
  max_chars <- if (length(text_values)) max(nchar(text_values, type = "width")) else 0L

  automatic_width <- max(min_width, min(max_width, base_width + max_chars * char_width))
  automatic_height <- max(min_height, min(max_height, base_height + rows * row_height))

  list(
    width = resolve_dimension(width, automatic_width, "width"),
    height = resolve_dimension(height, automatic_height, "height")
  )
}

configure_bioflow_fonts <- function(zh_font = "wqy-microhei", dpi = 300) {
  require_r_packages(c("showtext", "ggplot2"))

  options(bioflow_zh_font = zh_font)
  showtext::showtext_opts(dpi = dpi)
  showtext::showtext_auto()
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = zh_font)
  )
  graphics::par(family = zh_font)
  log_msg("Configured plotting font: ", zh_font)

  invisible(zh_font)
}

configure_bioflow_fonts()
