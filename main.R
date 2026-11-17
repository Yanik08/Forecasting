# main.R ---------------------------------------------------------------------
# Swiss CPI inflation forecast, 2026Q4-2029Q3 (Economic Forecasting, Project 2)
#
# Runs the whole project in order. Open Forecasting.Rproj, then run this file:
#   source("main.R")
# Every parameter is set once, here. The scripts read them from this
# environment, so change them here and nowhere else.

library(here)
library(tidyverse)


# Parameters -------------------------------------------------------------------

# Data
refresh_raw  <- FALSE   # TRUE: download the latest releases into RAW_DATA/
run_cleaning <- FALSE   # TRUE: re-run 01 and 02 (DTA_CLEAN/ is already committed)

# Samples and horizon
start_estimation <- as.Date("2000-01-01")  # first quarter used by the models
first_origin     <- as.Date("2012-10-01")  # first origin of the evaluation (2012Q4)
fcst_horizon     <- 12                     # quarters: 2026Q4-2029Q3

# Model orders: ARIMA(p,1,q) with drift, on 100 * log(index) seasonally
# adjusted with X-13 (no seasonal term). Selected by BIC in
# 03_descriptive_analysis.qmd, section 5. Imported goods excl. oil also
# include a dummy for the strong-franc period 2009Q1-2016Q4, which shifts
# their mean growth during that period only (zero in the forecast).
# Benchmarks for the evaluation (06), applied directly to headline CPI
# seasonally adjusted with X-13, without disaggregation:
#   headline:     ARIMA with the lowest BIC on adjusted headline CPI, 2000Q1-2026Q3
#   headline_ar1: AR(1) on quarterly headline inflation, the standard benchmark
arima_specs <- list(
  rent      = list(order = c(3, 1, 0)),
  imp_xoil  = list(order = c(0, 1, 2),
                   dummy = c(from = "2009-01-01", to = "2016-10-01")),  # strong franc
  dom_xrent = list(order = c(1, 1, 1)),
  headline     = list(order = c(0, 1, 1)),
  headline_ar1 = list(order = c(1, 1, 0))
)

# Density forecast and scenario
n_sims         <- 5000                     # simulated paths
seed           <- 2026                     # for reproducible simulations
oil_shock_date <- as.Date("2027-01-01")    # Brent shock starts in 2027Q1
oil_shock_size <- 50                       # permanent rise in Brent, in %


# Run --------------------------------------------------------------------------

if (run_cleaning) {
  source(here("Scripts", "01_load_data.R"))
  # The cleaning is a Quarto notebook: extract its R code and run it
  source(knitr::purl(here("Scripts", "02_dta_cleaning.qmd"),
                     output = tempfile(fileext = ".R"), quiet = TRUE))
}

# Estimation is a Quarto notebook too: extract its R code and run it
source(knitr::purl(here("Scripts", "04_model_estimation.qmd"),   # estimates, tests, checks
                   output = tempfile(fileext = ".R"), quiet = TRUE))
source(knitr::purl(here("Scripts", "05_forecasting.qmd"),        # final forecast, fan chart
                   output = tempfile(fileext = ".R"), quiet = TRUE))
source(knitr::purl(here("Scripts", "06_evaluation.qmd"),         # spaghetti chart, tables
                   output = tempfile(fileext = ".R"), quiet = TRUE))
