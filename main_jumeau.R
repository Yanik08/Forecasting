# main_jumeau.R ---------------------------------------------------------------------
# Swiss CPI inflation forecast, 2026Q4-2029Q3 (Economic Forecasting, Project 2)
#
# Runs the whole project in order. Open Forecasting.Rproj, then run this file:
#   source("main_jumeau.R")
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

# Jumeau version of main.R: runs the *_jumeau scripts, in which we test
# external drivers in the block models. Same parameters as main.R, except the
# model specifications below. Results go to output/jumeau/ and figures/jumeau/.
#   rent:      ARIMA(3,1,0) with the mortgage rate, lags 1 and 2 (spec$driver,
#              spec$driver_lags; see R/models_jumeau.R and 03_..._jumeau.qmd)
#   dom_xrent: ARIMA(1,1,1) with the SNB policy rate, lag 1 (BIC comparison
#              in 03_..._jumeau.qmd).

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
  rent      = list(order = c(3, 1, 0), driver = "mortgage_rate", driver_lags = 2),
  imp_xoil  = list(order = c(0, 1, 2),
                   dummy = c(from = "2009-01-01", to = "2016-10-01")),  # strong franc
  dom_xrent = list(order = c(1, 1, 1), driver = "policy_rate", driver_lags = 1),
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
# External drivers (jumeau): DTA_CLEAN/drivers_jumeau.csv
if (run_cleaning || !file.exists(here("DTA_CLEAN", "drivers_jumeau.csv"))) {
  source(knitr::purl(here("Scripts", "02_dta_cleaning_jumeau.qmd"),
                     output = tempfile(fileext = ".R"), quiet = TRUE))
}

# Jumeau pipeline: results in output/jumeau/ and figures/jumeau/
for (f in c("04_model_estimation_jumeau", "05_forecasting_jumeau", "06_evaluation_jumeau")) {
  source(knitr::purl(here("Scripts", paste0(f, ".qmd")),
                     output = tempfile(fileext = ".R"), quiet = TRUE))
}
