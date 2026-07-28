# stats.R - enlargement vs. division stats for the Nepenthes project, in R.
#
# R port of analyze_glands.py (in the vault project folder under imagej/).
# Runs the Mann-Whitney U test (R calls it wilcox.test) on the same CSVs the
# figures use:
#   data/cells_*.csv           - epidermal cell areas (from measure_cells.ijm)
#   data/intergland_counts.csv - cells between adjacent glands (typed by hand)
#
# Open in RStudio and click Source, or run: source("stats.R")
# With no data present it runs on a small DEMO set so you can see it work.

library(readr)
library(dplyr)

ALPHA <- 0.05

# ---- one mature-vs-immature comparison ----------------------------------
# Returns "up" / "down" only when the difference is significant, else "flat".
compare <- function(label, imm, mat, unit) {
  imm <- imm[!is.na(imm)]
  mat <- mat[!is.na(mat)]
  if (length(imm) < 3 || length(mat) < 3) {
    cat(sprintf("  %s: not enough data (immature n=%d, mature n=%d)\n",
                label, length(imm), length(mat)))
    return(NA_character_)
  }
  # Mann-Whitney U test == Wilcoxon rank-sum, two-sided, unpaired
  w <- suppressWarnings(wilcox.test(mat, imm, alternative = "two.sided"))
  mi <- mean(imm); mm <- mean(mat)
  ratio <- if (mi != 0) mm / mi else NA_real_
  arrow <- if (mm > mi) "up" else if (mm < mi) "down" else "flat"
  sig <- if (w$p.value < ALPHA) "SIGNIFICANT" else "not significant"
  cat(sprintf("  %s:\n", label))
  cat(sprintf("    immature mean = %.3f %s (n=%d)\n", mi, unit, length(imm)))
  cat(sprintf("    mature   mean = %.3f %s (n=%d)\n", mm, unit, length(mat)))
  cat(sprintf("    mature is %s, x%.2f, Mann-Whitney p = %.4f (%s)\n",
              arrow, ratio, w$p.value, sig))
  if (w$p.value < ALPHA) arrow else "flat"
}

# ---- read off the decision rule -----------------------------------------
verdict <- function(area_dir, count_dir) {
  cat("\nVERDICT\n")
  flatish <- function(x) is.na(x) || x == "flat"
  if (!is.na(area_dir) && area_dir == "up" && flatish(count_dir)) {
    cat("  Cells got BIGGER but not more numerous -> ENLARGEMENT.\n")
  } else if (!is.na(count_dir) && count_dir == "up" && flatish(area_dir)) {
    cat("  More cells, same size -> DIVISION (cells added).\n")
  } else if (!is.na(area_dir) && !is.na(count_dir) &&
             area_dir == "up" && count_dir == "up") {
    cat("  Cells bigger AND more numerous -> BOTH contribute (mixed).\n")
  } else if (flatish(area_dir) && flatish(count_dir)) {
    cat("  No clear change detected -> INCONCLUSIVE with this data.\n")
  } else {
    cat(sprintf("  Unusual pattern (area=%s, count=%s) -> inspect by hand.\n",
                area_dir, count_dir))
  }
  cat("  Reminder: one pitcher per stage means this is a pattern in these\n")
  cat("  specimens, not a population-level result.\n")
}

# ---- load cell-area data ------------------------------------------------
cell_files <- list.files("data", pattern = "^cells_.*\\.csv$", full.names = TRUE)
if (length(cell_files) > 0) {
  cells <- cell_files |>
    lapply(read_csv, show_col_types = FALSE) |>
    bind_rows()
} else {
  message("No cells_*.csv found in data/ - using DEMO data (a division scenario).")
  set.seed(1)
  cells <- bind_rows(
    tibble(stage = "immature", zone = "stomach", Area = rnorm(40, 120, 25)),
    tibble(stage = "mature",   zone = "stomach", Area = rnorm(40, 118, 24))
  )
}
cells <- cells |> mutate(stage = tolower(stage), zone = tolower(zone))

# ---- cell-area test, per zone -------------------------------------------
area_dir <- NA_character_
for (z in sort(unique(cells$zone))) {
  cat(sprintf("\n=== CELL SIZE, zone: %s ===\n", z))
  sub <- cells |> filter(zone == z)
  imm <- sub |> filter(stage == "immature") |> pull(Area)
  mat <- sub |> filter(stage == "mature")   |> pull(Area)
  area_dir <- compare("Epidermal cell area", imm, mat, "um^2")
}

# ---- inter-gland cell-count test ----------------------------------------
count_dir <- NA_character_
count_path <- "data/intergland_counts.csv"
if (file.exists(count_path)) {
  counts <- read_csv(count_path, show_col_types = FALSE) |>
    mutate(stage = tolower(stage))
  cat("\n=== CELLS BETWEEN NEIGHBOURING GLANDS ===\n")
  imm <- counts |> filter(stage == "immature") |> pull(cell_count)
  mat <- counts |> filter(stage == "mature")   |> pull(cell_count)
  count_dir <- compare("Cells between adjacent glands", imm, mat, "cells")
} else {
  cat("\n(no intergland_counts.csv yet - the size result alone is suggestive",
      "but the count is what clinches enlargement vs. division)\n")
}

verdict(area_dir, count_dir)
