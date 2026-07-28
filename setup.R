# setup.R - restore this project's package environment (renv).
#
# This project uses renv: a private, project-local package library pinned in
# renv.lock. When you open Nepenthes-Figures.Rproj, renv activates automatically.
#
# First time on a new computer: open this file and click Source (or run
# source("setup.R")). It installs the exact package versions from renv.lock.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}
renv::restore(prompt = FALSE)
message("Environment restored. You can now Source figures.R and stats.R.")
