# Forecasting

Economic Forecasting for Decision Making (MSc, HEC Lausanne), Project 2: Swiss CPI inflation.

We forecast Swiss year-on-year CPI inflation for 2026Q4–2029Q3 (12 quarters), with a point
forecast, a density forecast and a pseudo out-of-sample evaluation. Headline CPI is split
into four blocks, each block is forecast separately, and the blocks are re-aggregated with
the CPI weights.

## How to run

Open `Forecasting.Rproj`, then run `source("main.R")`. It runs every step in order and
writes all tables to `output/` and all figures to `figures/`. All parameters (sample start,
horizon, first evaluation origin, model orders, number of simulations, oil-shock scenario)
are set once, at the top of `main.R`.

By default `main.R` uses the cleaned data already in `DTA_CLEAN/`. Set
`run_cleaning <- TRUE` to rebuild it from `RAW_DATA/` first, and `refresh_raw <- TRUE` to
download the latest releases.

## Project structure

```
Forecasting/
|-- Forecasting.Rproj
|-- main.R                        # parameters + runs everything in order
|-- README.md
|-- R/
|   |-- functions.R               # download helpers (used by 01)
|   `-- models.R                  # model, forecast, aggregation and test functions
|-- Scripts/
|   |-- 01_load_data.R            # downloads raw data into RAW_DATA/
|   |-- 02_dta_cleaning.qmd       # cleaning, block indices, weights, monthly -> quarterly
|   |-- 03_descriptive_analysis.qmd  # descriptive stats, seasonality, unit roots, model selection
|   |-- 04_model_estimation.R     # estimates, diagnostics, cointegration, checks
|   |-- 05_forecasting.R          # final forecast, density, oil-shock scenario, fan chart
|   `-- 06_evaluation.R           # recursive forecasts, spaghetti chart, evaluation tables
|-- RAW_DATA/                     # raw files, never edited by hand
|-- DTA_CLEAN/                    # cleaned and quarterly data
|-- output/                       # tables (csv)
`-- figures/                      # charts (png)
```

`03_descriptive_analysis.qmd` is a companion notebook, not part of the pipeline: `main.R`
does not run it, because it builds no data and writes no output. It documents how the
models were chosen (descriptive statistics, seasonality and unit-root tests, ARIMA
selection, cointegration and ECM lag choice). Open `03_descriptive_analysis.html` to read
it, or render the `.qmd` in RStudio to re-run it. The numbers quoted in the report come
from `04_model_estimation.R`.

## Models

All block models use y = 100 × log(index) on 2000Q1–2026Q3, so Δy is quarter-on-quarter
inflation in % and y_t − y_{t−4} is y-y inflation. The derivations of the optimal h-step
forecasts are in the model document.

| Block | Model | Drivers |
|---|---|---|
| Oil products | Error correction model (Engle–Granger), 1 lag | Brent (USD), CHF per USD, linear trend |
| Rents | ARIMA(1,1,1)(1,0,0)₄ with drift | none |
| Imported excl. oil | ARIMA(2,1,1)(1,0,0)₄ with drift | none |
| Domestic excl. rents | ARIMA(0,1,0)(1,0,0)₄ with drift | none |
| CPI weights | Random walk: last observed weights held fixed | none |

- **Point forecast:** each model is iterated forward with future shocks set to zero. Brent
  and USD/CHF follow a random walk. The block forecasts are aggregated to headline CPI
  link by link (quarterly links at Q4), with the 2026 weights.
- **Density forecast:** 5,000 simulated paths. Each future quarter draws one past quarter
  and uses that quarter's residuals for all four blocks and its changes in Brent and
  USD/CHF, so the correlation between blocks and drivers is kept.
- **Scenario:** a permanent 50% rise in Brent from 2027Q1.
- **Evaluation** (definitions as in the lecture slides, section 3.1): recursive
  (expanding-window) forecasts from origins 2012Q4 to 2026Q2, horizons 1–12. Absolute
  standards: Mincer–Zarnowitz, unbiasedness and strong efficiency (HAC tests), weak
  efficiency (Ljung–Box on the residuals of an MA(h−1) fitted to the errors), and the
  error variance by horizon. Relative standards: MSFE and MAFE ratios against an ARIMA on
  headline CPI, a random walk on y-y inflation and the sample mean, with
  Diebold–Mariano tests (Harvey–Leybourne–Newbold correction).
- **Re-aggregation check:** rebuilding headline y-y inflation from the four blocks gives a
  maximum gap of 0.14 pp from the published series since 2000 (mean 0.02 pp).

## Outputs

| File | Content |
|---|---|
| `output/estimates_arima.csv`, `output/estimates_ecm.csv` | Estimated coefficients and diagnostics |
| `output/reaggregation_check.csv` | Published vs rebuilt headline y-y inflation |
| `output/forecast_headline.csv` | Point forecast, 5/16/50/84/95% quantiles, oil-shock scenario |
| `output/forecast_blocks.csv` | Point forecast of each block (y-y, %) |
| `output/eval_forecasts.csv` | All recursive forecasts and outcomes |
| `output/eval_absolute.csv`, `output/eval_relative.csv` | Evaluation tables by horizon |
| `figures/fan_chart_headline.png` | Fan chart with the oil-shock scenario |
| `figures/spaghetti_headline.png` | Recursive forecasts against the outcome |
| `figures/error_variance_by_h.png` | Variance of the forecast errors by horizon |

## Data

All raw data lives in `RAW_DATA/` and is never edited by hand. `Scripts/01_load_data.R`
downloads every file from its source (API or stable link) and reads it into R.

- **Normal run:** files already in `RAW_DATA/` are reused, so results are reproducible.
- **Update the data:** set `refresh_raw <- TRUE` in `main.R` to replace every file with the
  latest release.
- `RAW_DATA/download_log.csv` records the source, link and date of each download.

The models use the CPI indices and weights, Brent and the CHF/USD rate. The other series
(GDP, policy rate, euro-area HICP, unemployment, reference rate) are kept for context and
for comparison with the SNB's conditional forecast, but no model uses them.

| File | Source | Series / ID | Frequency, unit | Used by the models |
|---|---|---|---|---|
| `bfs_cpi_LIK25B25.xlsx` | BFS (FSO), via DAM API | LIK detailed results since 1982, basket 2025, Dec 2025 = 100, BFS no. su-d-05.02.66 | Monthly index, NSA | Yes |
| `bfs_cpi_weights.xlsx` | BFS (FSO), via DAM API | LIK weights of all baskets since 1914, BFS no. su-d-05.02.90 | Annual weights, % | Yes |
| `snb_fx_devkum.csv` | SNB data portal API | Cube `devkum`, M0, EUR1 and USD1 | Monthly average, CHF per unit | Yes (USD) |
| `fred_brent_MCOILBRENTEU.csv` | FRED API (`fredr`) | `MCOILBRENTEU`, Brent crude | Monthly average, USD per barrel, NSA | Yes |
| `seco_qna_p_cssa.xlsx` | SECO | Quarterly GDP, production approach, seasonally, calendar and sport-event adjusted | Quarterly, CHF mn | No |
| `snb_policy_rates_snboffzisa.csv` | SNB data portal API | Cube `snboffzisa` (LZ = SNB policy rate since June 2019; UG0/OG0 = 3M Libor target range 2000–2019) | End of month, % | No |
| `eurostat_hicp_ea.csv` | Eurostat API (`eurostat` package) | `prc_hicp_minr`, EA, I25, TOTAL and TOT_X_NRG | Monthly index 2025 = 100, NSA | No |
| `bfs_unemployed_by_canton.csv` | BFS (source: SECO), via DAM API | Registered unemployed by canton, BFS no. ts-x-03.03.02.04 | Monthly, persons | No |
| `bwo_reference_rate.xlsx` | Federal Office for Housing (BWO) | Mortgage reference rate and average mortgage rate | Quarterly, % | No |

`bwo_reference_rate.xlsx` is the only file not downloaded by the script: the BWO
publishes no API or file, so the table was copied from
<https://www.bwo.admin.ch/de/entwicklung-referenzzinssatz-und-durchschnittszinssatz>.
To update it, copy the new rows from that page.

Downloading Brent needs a free FRED API key (create an account on fred.stlouisfed.org,
then My Account → API Keys). Save it once with `file.edit("~/.Renviron")`, adding the line
`FRED_API_KEY=your_key`, and restart R. Never commit the key. It is only needed to
download: reading the snapshot in `RAW_DATA/` works without it.

## R packages

Data: `here`, `tidyverse`, `readxl`, `jsonlite`, `curl`, `eurostat`, `fredr`, `knitr`.
Models and evaluation: `urca` (unit-root and cointegration tests), `sandwich`
(Newey–West standard errors). The descriptive notebook also needs `forecast`.
