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

# Model orders: ARIMA(p,1,q)(P,0,Q)_4 with drift, on 100 * log(index).
# As estimated with the TA (see the model doc, section 2). `headline` is the
# benchmark: an ARIMA directly on headline CPI, without disaggregation.
arima_specs <- list(
  rent      = list(order = c(1, 1, 1), seasonal = c(1, 0, 0)),
  imp_xoil  = list(order = c(2, 1, 1), seasonal = c(1, 0, 0)),
  dom_xrent = list(order = c(0, 1, 0), seasonal = c(1, 0, 0)),
  headline  = list(order = c(1, 1, 0), seasonal = c(1, 0, 0))
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

source(here("Scripts", "04_model_estimation.R"))  # estimates, tests, checks
source(here("Scripts", "05_forecasting.R"))       # final forecast, fan chart
source(here("Scripts", "06_evaluation.R"))        # spaghetti chart, tables
