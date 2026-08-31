generate_table1_outputs <- function(df, x_vars, stratify_var = NULL, output_dir) {
  require_r_packages(c("readr", "table1"))

  css <- '
.Rtable1 table, table.Rtable1 { font-family: "Arial", Arial, sans-serif; font-size: 10pt; border-collapse: collapse; padding: 0px; margin: 0px; }
.Rtable1 td { white-space: nowrap; }
.Rtable1 th, .Rtable1 td { border: 0; text-align: center; padding: 0.5ex 1.5ex; margin: 0px; }
.Rtable1 thead>tr:first-child>th { border-top: 2pt solid black; }
.Rtable1 thead>tr:last-child>th { border-bottom: 1pt solid black; }
.Rtable1 tbody>tr:last-child>td { border-bottom: 2pt solid black; }
.Rtable1 th.grouplabel { padding-left: 0; padding-right: 0; }
.Rtable1 th.grouplabel>div { margin-left: 1.5ex; margin-right: 1.5ex; border-bottom: 1pt solid black; }
.Rtable1 th.grouplabel:last-child>div { margin-right: 0; }
.Rtable1 .rowlabel { text-align: left; padding-left: 2.5ex; }
.Rtable1 .firstrow.rowlabel { padding-left: 0.5ex; font-weight: bold; }
.Rtable1 .Rtable1-footnote { font-size: smaller; padding: 0px; margin: 0px; text-align: left; white-space: normal; }
'
  quote_var <- function(x) paste0("`", x, "`")
  stratify_vars <- if (is.null(stratify_var)) character() else stratify_var
  stratify_vars <- stratify_vars[!is.na(stratify_vars) & nzchar(stratify_vars)]
  missing_vars <- setdiff(c(x_vars, stratify_vars), names(df))
  if (length(missing_vars) > 0) stop("Variables not found in `df`: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  df[stratify_vars] <- lapply(df[stratify_vars], as.factor)
  x_txt <- paste(quote_var(x_vars), collapse = " + ")
  stratify_txt <- if (length(stratify_vars) > 0) paste0(" | ", paste(quote_var(stratify_vars), collapse = " * ")) else ""
  fml_txt <- paste0("~ ", x_txt, stratify_txt)
  t1 <- table1::table1(as.formula(fml_txt), data = df)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_lines(c("<style>", css, "</style>", as.character(t1)), file.path(output_dir, "table1.html"))
  readr::write_csv(as.data.frame(t1, stringsAsFactors = FALSE), file.path(output_dir, "table1.csv"))
  return(t1)
}
