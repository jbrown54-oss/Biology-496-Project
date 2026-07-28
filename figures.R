# figures.R - build the Nepenthes project figures in R.
#
# Reads the CSVs your ImageJ pipeline produces (measure_cells.ijm, in the vault
# project folder under imagej/):
#   data/cells_*.csv           - traced epidermal cell areas
#   data/intergland_counts.csv - cells counted between adjacent glands (typed by hand)
#
# Copy those CSVs into this project's data/ folder, then Source this file.
# If no data is present yet it runs on a small DEMO dataset so you can see it work.

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(patchwork)

# ---- a clean, presentation-ready theme ----------------------------------
theme_nep <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )
stage_colors <- c(immature = "#4C9F70", mature = "#2C5F8A")
stage_order  <- c("immature", "mature")

# ---- load cell-area data ------------------------------------------------
cell_files <- list.files("data", pattern = "^cells_.*\\.csv$", full.names = TRUE)

if (length(cell_files) > 0) {
  cells <- cell_files |>
    lapply(read_csv, show_col_types = FALSE) |>
    bind_rows()
  demo <- FALSE
} else {
  message("No cells_*.csv found in data/ - using DEMO data. Delete this once you have real files.")
  set.seed(1)
  cells <- bind_rows(
    tibble(stage = "immature", zone = "stomach", Area = rnorm(40, 120, 25)),
    tibble(stage = "mature",   zone = "stomach", Area = rnorm(40, 118, 24))  # division scenario: same size
  )
  demo <- TRUE
}

cells <- cells |>
  mutate(stage = factor(tolower(stage), levels = stage_order))

# ---- figure 1: epidermal cell area by stage -----------------------------
p_area <- ggplot(cells, aes(stage, Area, fill = stage)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.85) +
  geom_jitter(width = 0.12, size = 1, alpha = 0.4) +
  scale_fill_manual(values = stage_colors) +
  labs(
    title = paste0("Epidermal cell area", if (demo) " (DEMO DATA)" else ""),
    x = NULL, y = expression(area~(mu*m^2))
  ) +
  theme_nep

# ---- load + plot inter-gland cell counts (if present) -------------------
count_path <- "data/intergland_counts.csv"
if (file.exists(count_path)) {
  counts <- read_csv(count_path, show_col_types = FALSE) |>
    mutate(stage = factor(tolower(stage), levels = stage_order))

  p_count <- ggplot(counts, aes(stage, cell_count, fill = stage)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.85) +
    geom_jitter(width = 0.12, size = 1, alpha = 0.4) +
    scale_fill_manual(values = stage_colors) +
    labs(title = "Cells between adjacent glands", x = NULL, y = "cell count") +
    theme_nep

  fig <- p_area + p_count + plot_annotation(tag_levels = "A")
} else {
  message("No intergland_counts.csv yet - showing cell-area figure only.")
  fig <- p_area
}

# ---- save ---------------------------------------------------------------
ggsave("output/cell_comparison.png", fig, width = 8, height = 4, dpi = 300)
message("Saved: output/cell_comparison.png")
print(fig)
