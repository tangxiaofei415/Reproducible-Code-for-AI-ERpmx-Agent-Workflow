# BioFlow data inspection
# Edit data_path, then run:
# run_r("source('scripts/medical_stats/inspect_data.R')")

source("scripts/medical_stats/common.R")

# ---- USER CONFIG START ----
data_path <- "data/test1.csv"    # Thread-relative path, e.g. "data/input.csv" or "output/data/cleaned_data.rds".
# ---- USER CONFIG END ----

require_r_packages(c("skimr"))

log_msg("Inspecting data with skimr: ", data_path)
df <- read_medical_data(data_path)

skim_result <- skimr::skim(df)
print(skim_result)
