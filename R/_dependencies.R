# _dependencies.R --------------------------------------------------------
# Not meant to be run. This file exists so renv can SEE packages that are
# needed but never appear in a library() call it can scan.
#
# renv discovers dependencies by scanning source files for library()/require().
# Two things escape that:
#   * IRkernel  - launched by Jupyter before any project code runs. If it is
#                 missing from the lockfile, a fresh clone gets a kernel that
#                 dies on startup with no useful error.
#   * packages used only inside .ipynb cells, depending on renv's notebook
#     support in the installed version.
#
# Listing them here keeps `renv::snapshot()` honest without pinning the whole
# system library.

library(IRkernel)   # Jupyter R kernel
library(tidyverse)  # dplyr / ggplot2 / readr / tibble / tidyr
library(cowplot)    # theme_cowplot()
library(readxl)     # reading trichome measurements.xlsx
library(patchwork)  # multi-panel figure assembly
library(knitr)      # notebook -> document rendering
