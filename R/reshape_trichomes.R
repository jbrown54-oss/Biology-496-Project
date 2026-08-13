# reshape_trichomes.R ----------------------------------------------------
# Turn "trichome measurements.xlsx" (wide block layout, one block per
# trichome) into one tidy row per measurement.
#
# The sheet is laid out as 4 side-by-side blocks x 2 header rows. Each block
# is a label cell + a unit cell, with an index/value pair underneath, and an
# "avg length of branches" summary row that must NOT be read as data.
#
# Output columns are named to match the plotting script:
#   Stage, Tissue, Face, Trichome, TrichomeID, Branch, Perimeter, Length, Note

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(readr)
})

reshape_trichomes <- function(
    xlsx = "trichome measurements.xlsx",
    # NB: readxl drops the sheet's empty leading column, so these are readxl
    # indices, not the letters you see in Excel (col A is dropped, B -> 1).
    header_rows = c(1L, 9L),
    block_cols  = c(1L, 4L, 7L, 10L)) {

  raw <- read_excel(xlsx, sheet = 1, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  cell <- function(r, c) {
    if (r > nrow(raw) || c > ncol(raw)) return(NA_character_)
    v <- raw[[c]][r]
    if (length(v) == 0) NA_character_ else v
  }

  out <- list()

  for (h in header_rows) {
    for (cc in block_cols) {
      label <- cell(h, cc)
      unit  <- cell(h, cc + 1L)
      if (is.na(label) || !nzchar(trimws(label))) next

      # walk down until the block runs out
      vals <- c(); idx <- c()
      r <- h + 1L
      repeat {
        a <- cell(r, cc); b <- cell(r, cc + 1L)
        if (is.na(a) && is.na(b)) break
        # "avg length of branches" rows: first cell is text, not an index
        if (!is.na(a) && is.na(suppressWarnings(as.numeric(a)))) { r <- r + 1L; next }
        if (!is.na(b)) { vals <- c(vals, as.numeric(b)); idx <- c(idx, as.integer(a)) }
        r <- r + 1L
        if (r > nrow(raw) + 1L) break
      }
      if (!length(vals)) next

      # ---- parse the sample code out of the label ----
      code <- tolower(gsub("[^a-z0-9]", "", tolower(label)))
      m <- regmatches(code, regexec("^([mi])(th|st)([fb])?", code))[[1]]
      stage  <- c(m = "mature", i = "immature")[[m[2]]]
      tissue <- c(th = "throat", st = "stomach")[[m[3]]]
      face   <- if (length(m) >= 4 && nzchar(m[4]))
                  c(f = "outside", b = "inside")[[m[4]]] else NA_character_

      is_circle <- grepl("circle", label, ignore.case = TRUE)
      metric    <- if (grepl("perim", unit, ignore.case = TRUE)) "perimeter" else "length"

      out[[length(out) + 1L]] <- tibble(
        Stage      = stage,
        Tissue     = tissue,
        Face       = face,
        Trichome   = if (is_circle) "circle" else "branched",
        TrichomeID = trimws(label),
        Branch     = if (metric == "length" && length(vals) > 1) idx else NA_integer_,
        Perimeter  = if (metric == "perimeter") vals else NA_real_,
        Length     = if (metric == "length")    vals else NA_real_,
        Note       = NA_character_
      )
    }
  }

  d <- bind_rows(out)

  # Known caveats, carried in the data rather than left in someone's head.
  d <- d |>
    mutate(
      Note = case_when(
        grepl("^istf_trichome1$", TrichomeID) ~
          paste("Measured at 350x, not 800x. A long UNBRANCHED hair -",
                "a different morphotype. Never compare against the mature",
                "branch lengths."),
        is.na(Face) ~ "Face not recorded in the original label - confirm outside vs inside.",
        TRUE ~ NA_character_
      )
    )
  d
}

if (sys.nframe() == 0L) {
  d <- reshape_trichomes()
  dir.create("data", showWarnings = FALSE)
  write_csv(d, "data/trichome_measurements_tidy.csv", na = "")
  cat("wrote data/trichome_measurements_tidy.csv -", nrow(d), "rows\n\n")
  print(as.data.frame(d), row.names = FALSE)
}
