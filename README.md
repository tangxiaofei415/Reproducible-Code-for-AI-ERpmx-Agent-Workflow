# Reproducible Medical Statistics Source Package

This package contains the source files needed to re-run the completed logistic
regression analysis from the thread.

## Contents

- `data/test1.csv` — uploaded raw input data.
- `scripts/medical_stats/` — analysis templates, the cleaning plan, flow history,
  and immutable flow snapshots.
- `output/data/cleaned_data.csv` and `output/data/cleaned_data.rds` — cleaned,
  complete-case analysis dataset used as the input for all downstream steps.
- `reproduce.R` — entrypoint that replays the recorded flow snapshots in order.
- `manifest.json` — packaging manifest describing what was included and excluded.

## How to re-run

1. Unzip `reproducible-package.zip` into a directory.
2. Set the extracted package root as R's working directory (the directory that
   directly contains `data/`, `scripts/`, `output/`, `README.md`, and
   `reproduce.R`).
3. Run the reproducible entrypoint:

   `source("reproduce.R")`

`reproduce.R` reads `scripts/medical_stats/flow_history.json` and sources every
recorded `snapshot_flow_path` in chronological order. This replays Table 1,
univariable logistic plots, and batch logistic regression, and regenerates their
outputs under `output/`.

## Cleaning note

The cleaning step was performed with the standard cleaning workflow and is
documented in `scripts/medical_stats/cleaning_plan.json`:

- Dropped the imported index column `...1`.
- Converted categorical variables to factors with the recorded levels.
- Applied complete-case (listwise) deletion, reducing the dataset from 100 to
  67 rows.

The resulting cleaned data is included as `output/data/cleaned_data.csv` and
`output/data/cleaned_data.rds`, so the downstream flow snapshots can be replayed
directly. To re-derive the cleaned data from the raw CSV, re-apply the recorded
cleaning plan and complete-case deletion described above.

## Requirements

The flow uses standard R packages including `readr`, `dplyr`, `tibble`,
`ggplot2`, `skimr`, `showtext`, `jsonlite`, `StepReg`, and `autoReg`. Install any
missing packages in the R session that runs `reproduce.R`.
