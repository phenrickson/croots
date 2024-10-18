# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dplyr", "tidyr", "purrr", "ggplot2", "googlesheets4")
)

# source functions
tar_source("R")

# pipeline
list(
  tar_target(
    recruits_raw,
    load_recruits()
  ),
  tar_target(
    recruits_tidied,
    recruits_raw |>
      tidy_recruits() |>
      tidy_overall()
  )
)
