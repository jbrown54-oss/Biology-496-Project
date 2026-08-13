# prepare_data.R ---------------------------------------------------------
# Turn the raw ImageJ exports in data/raw/ into tidy CSVs in data/.
# Run once (or via "0 Prepare data.ipynb"); the figure notebooks then just
# read_csv() the tidy files.
#
# Raw quirks this handles:
#   * a leading unnamed column (ImageJ's row number)
#   * a Label column on some exports and not others
#   * Mean / SD / Min / Max summary rows appended by ImageJ

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tibble); library(purrr)
})

RAW <- "data/raw"

# 50x field: 2560 x 1920 um, data bar excluded
FIELD_UM2 <- 2560 * 1920
FIELD_MM2 <- FIELD_UM2 / 1e6      # 4.9152

read_imagej <- function(file) {
  d <- read_csv(file.path(RAW, file), show_col_types = FALSE,
                name_repair = function(x) ifelse(x == "" | is.na(x), "row", x))
  # drop ImageJ's trailing summary rows
  if ("Label" %in% names(d)) d <- filter(d, !(Label %in% c("Mean","SD","Min","Max")))
  d |> filter(!(as.character(row) %in% c("Mean","SD","Min","Max")))
}

# ---- 1. gland positions -------------------------------------------------
gland_positions <- bind_rows(
  read_imagej("ISB50x.csv")               |> transmute(stage = "immature", image = "istb50x", x_um = X, y_um = Y),
  read_imagej("MSB50x Gland position.csv")|> transmute(stage = "mature",   image = "msb50x",  x_um = X, y_um = Y)
) |> filter(!is.na(x_um), !is.na(y_um))

# ---- 2. gland areas -----------------------------------------------------
gland_areas <- bind_rows(
  read_imagej("ISB50x Gland area.csv") |> transmute(stage = "immature", image = "istb50x", area_um2 = Area),
  read_imagej("MSB50x Gland area.csv") |> transmute(stage = "mature",   image = "msb50x",  area_um2 = Area)
) |> filter(!is.na(area_um2))

# ---- 3. epidermal cell areas -------------------------------------------
# msf800x is kept but flagged: its areas imply a 3 um cell, ~100x too small.
cell_areas <- bind_rows(
  read_imagej("ithftrichome800x Cell area.csv")   |> transmute(stage="immature", zone="throat",  image="ithftrichome800x",   area_um2=Area, use=TRUE),
  read_imagej("ithftrichome2 800x Cell area.csv") |> transmute(stage="immature", zone="throat",  image="ithftrichome2_800x", area_um2=Area, use=TRUE),
  read_imagej("mthf Cell area.csv")               |> transmute(stage="mature",   zone="throat",  image="mthf",               area_um2=Area, use=TRUE),
  read_imagej("msf800x Cell area.csv")            |> transmute(stage="mature",   zone="stomach", image="msf800x",            area_um2=Area, use=FALSE)
) |> filter(!is.na(area_um2))

# ---- 4. trichome counts, typed off the 50x overviews --------------------
trichome_density <- tribble(
  ~zone,     ~stage,     ~count, ~image,
  "throat",  "immature",    145, "ithf50x",
  "throat",  "mature",       89, "mthf50x",
  "stomach", "immature",     75, "istf50x",
  "stomach", "mature",       40, "msf50x"
) |> mutate(field_mm2 = FIELD_MM2, density_per_mm2 = count / field_mm2)

# ---- write --------------------------------------------------------------
if (sys.nframe() == 0L) {
  write_csv(gland_positions,  "data/gland_positions.csv")
  write_csv(gland_areas,      "data/gland_areas.csv")
  write_csv(cell_areas,       "data/cell_areas.csv")
  write_csv(trichome_density, "data/trichome_density.csv")

  cat("wrote 4 tidy files to data/\n\n")
  cat("gland_positions :", nrow(gland_positions), "rows\n")
  print(count(gland_positions, stage, image))
  cat("\ngland_areas     :", nrow(gland_areas), "rows\n")
  print(count(gland_areas, stage))
  cat("\ncell_areas      :", nrow(cell_areas), "rows\n")
  print(count(cell_areas, stage, zone, use))
  cat("\ntrichome_density:\n")
  print(as.data.frame(trichome_density), row.names = FALSE)
}
