#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow
#
#                     Project setup
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2023
#
#----------------------------------------------------------#

# Script to prepare all components of the environment to run the Project.
#   Needs to be run only once

#----------------------------------------------------------#
# Step 0: Install {renv} for package management -----
#----------------------------------------------------------#

if (
  "renv" %in% utils::installed.packages()
) {
  library(renv)
} else {
  # install package
  utils::install.packages("renv")

  # load the package
  library(renv)
}

#----------------------------------------------------------#
# Step 1: Activate 'renv' project -----
#----------------------------------------------------------#

# NOTE: The R may ask the User to restart the session (R).
#   After that, continue with the next step

renv::activate()

#----------------------------------------------------------#
# Step 1: Install {here} for file navigation -----
#----------------------------------------------------------#

if (
  "here" %in% utils::installed.packages()
) {
  library(here)
} else {
  # install package
  utils::install.packages("here")

  # load the package
  library(here)
}

#----------------------------------------------------------#
# Step 2: Synchronize package versions with the project -----
#----------------------------------------------------------#

# If there is no lock file present make a new snapshot
if (
  isTRUE("library_list.lock" %in% list.files(here::here("renv")))
) {
  cat("The project already has a lockfile. Restoring packages", "\n")

  renv::restore(
    lockfile = here::here("renv/library_list.lock")
  )

  cat("Set up completed. You can continute to run the project", "\n")

  cat("Do NOT run the rest of this script", "\n")
} else {
  cat("The project seems to be new (no lockfile)", "\n")

  cat("Continue with this script", "\n")
}

#----------------------------------------------------------#
# Step 3: Install packages to the project -----
#----------------------------------------------------------#

# install all packages in the lst from CRAN
sapply(
  c(
    "assertthat",
    "Bchron",
    "dplyr",
    "forcats",
    "furrr",
    "future",
    "ggplot2",
    "ggpubr",
    "grDevices",
    "here",
    "httr",
    "IntCal",
    "janitor",
    "knitr",
    "lifecycle",
    "magrittr",
    "methods",
    "neotoma2",
    "purrr",
    "qs",
    "raster",
    "rcarbon",
    "readr",
    "readxl",
    "remotes",
    "rioja",
    "rlang",
    "sf",
    "sp",
    "stats",
    "stringr",
    "tibble",
    "tidyr",
    "tidyselect",
    "tidyverse",
    "usethis",
    "utils",
    "waldo",
    "zip"
  ),
  function (x) renv::install(x, prompt = FALSE)
)

# install RUtilpol from GitHub
remotes::install_github(
  repo = "HOPE-UIB-BIO/R-Utilpol-package",
  ref = "HEAD",
  quiet = FALSE,
  upgrade = "ask"
)

# install RFossilpol from GitHub
remotes::install_github(
  repo = "HOPE-UIB-BIO/R-Fossilpol-package@*release",
  quiet = FALSE,
  upgrade = "ask"
)

#----------------------------------------------------------#
# Step 4: Save versions of packages -----
#----------------------------------------------------------#

renv::snapshot(
  lockfile = here::here("renv/library_list.lock")
)

cat("Set up completed. You can continute to run the project", "\n")
