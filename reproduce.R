# Reproduce the recorded medical statistics workflow.
#
# Run from the extracted package root (the directory that directly contains
# data/, scripts/, output/, README.md, and this file):
#
#   source("reproduce.R")
#
# This entrypoint reads scripts/medical_stats/flow_history.json and sources
# every recorded flow snapshot in chronological order, so it replays all
# completed flow steps rather than only the latest r_flow.R configuration.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Package 'jsonlite' is required. Install it with install.packages('jsonlite').",
    call. = FALSE
  )
}

# Some plotting helpers (forestploter, autoReg) briefly open R's default
# graphics device. In this workflow's R environment that device writes to a
# relative .runtime/r/plots/ path, so make sure that directory exists before
# replaying the snapshots. This is harmless in a plain R environment.
dir.create(".runtime/r/plots", recursive = TRUE, showWarnings = FALSE)

history_path <- "scripts/medical_stats/flow_history.json"
if (!file.exists(history_path)) {
  stop("flow_history.json not found: ", history_path, call. = FALSE)
}

history <- jsonlite::fromJSON(history_path)
snapshots <- history$runs$snapshot_flow_path

if (is.null(snapshots) || !length(snapshots)) {
  stop("No flow snapshots were recorded in flow_history.json.", call. = FALSE)
}

for (snapshot in snapshots) {
  if (!file.exists(snapshot)) {
    stop("Missing flow snapshot: ", snapshot, call. = FALSE)
  }
  message("Replaying flow snapshot: ", snapshot)
  source(snapshot)
}

message("All recorded flow snapshots were replayed successfully.")
