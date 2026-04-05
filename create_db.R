# =============================================================================
#  create_db.R — Build groundwater.duckdb from your source files
#
#  Run this script ONCE (or whenever your source data changes) before
#  launching the Shiny app.
#
#  Usage:
#    Rscript create_db.R
#    # or interactively: source("create_db.R")
# =============================================================================

library(DBI)
library(duckdb)

# ── Configuration ─────────────────────────────────────────────────────────────

DB_PATH   <- "groundwater.duckdb"
META_FILE <- "data/data_gems_meta.csv"
TS_FILE   <- "data/data_gems_dynamic.csv"

# Metadata columns : well_id, proj_id, coords_x, coords_y, depth, up_filter,
#                    lo_filter, scr_length, aquifer_med, pre_state
# Timeseries columns: well_id, date, gwl

# ── Load source data ──────────────────────────────────────────────────────────

message("Loading metadata…")

# Uncomment the loader that matches your file format:
meta_df <- readr::read_csv(META_FILE, show_col_types = FALSE)
# meta_df <- readRDS(META_FILE)
# meta_df <- arrow::read_parquet(META_FILE)

message(sprintf("  → %d wells loaded", nrow(meta_df)))

message("Loading time series…")

# For large time-series files, DuckDB can read CSV/Parquet directly without
# loading everything into R first.  The two paths below show both options.

# ── Option A: load into R first (fine for files up to ~500 MB in RAM) ─────────
ts_df <- readr::read_csv(TS_FILE, col_types = readr::cols(
  well_id = readr::col_character(),
  date    = readr::col_date(),
  gwl     = readr::col_double()
))
message(sprintf("  → %d rows loaded", nrow(ts_df)))

# ── Option B: let DuckDB import the CSV directly (better for very large files)
# (comment out Option A and uncomment below if needed)
# ts_df <- NULL   # will be written via SQL below

# ── Write to DuckDB ───────────────────────────────────────────────────────────

message(sprintf("Writing to %s…", DB_PATH))

# Open a fresh writable connection (overwrites existing DB)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH)

# Metadata table
DBI::dbWriteTable(con, "metadata", meta_df, overwrite = TRUE)
message("  ✓ metadata table written")

# Timeseries table — Option A (from R data.frame)
DBI::dbWriteTable(con, "timeseries", ts_df, overwrite = TRUE)
message("  ✓ timeseries table written")

# ── Option B: import CSV directly in SQL (uncomment if using Option B above) ──
# DBI::dbExecute(con, sprintf("
#   CREATE OR REPLACE TABLE timeseries AS
#   SELECT
#     well_id::VARCHAR  AS well_id,
#     date::DATE        AS date,
#     gwl::DOUBLE       AS gwl
#   FROM read_csv_auto('%s', header = true)
# ", normalizePath(TS_FILE)))

# ── Indexes ───────────────────────────────────────────────────────────────────
# A composite index on (well_id, date) makes the per-well range queries
# in the Shiny app very fast even on millions of rows.

message("Creating indexes…")

DBI::dbExecute(con,
  "CREATE INDEX IF NOT EXISTS idx_ts_well_date
   ON timeseries (well_id, date)")

DBI::dbExecute(con,
  "CREATE INDEX IF NOT EXISTS idx_ts_well
   ON timeseries (well_id)")

message("  ✓ indexes created")

# ── Verify ────────────────────────────────────────────────────────────────────

message("\nVerification:")
message(sprintf(
  "  metadata   : %d rows × %d cols",
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM metadata")$n,
  ncol(DBI::dbGetQuery(con, "SELECT * FROM metadata LIMIT 0"))
))
message(sprintf(
  "  timeseries : %d rows × %d cols",
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM timeseries")$n,
  ncol(DBI::dbGetQuery(con, "SELECT * FROM timeseries LIMIT 0"))
))
message(sprintf(
  "  date range : %s → %s",
  DBI::dbGetQuery(con, "SELECT MIN(date) AS d FROM timeseries")$d,
  DBI::dbGetQuery(con, "SELECT MAX(date) AS d FROM timeseries")$d
))

DBI::dbDisconnect(con, shutdown = TRUE)
message(sprintf("\nDone. Database written to: %s", normalizePath(DB_PATH)))
