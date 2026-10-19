# Forecasting

Economic Forecasting for Decision Making (MSc, HEC Lausanne), Project 2: Swiss CPI inflation.

## Data

All raw data lives in `RAW_DATA/` and is never edited by hand. `scripts/01_load_data.R`
downloads every file from its source (API or stable link) and reads it into R.

- **Normal run:** files already in `RAW_DATA/` are reused, so results are reproducible.
- **Update the data:** set `refresh_raw <- TRUE` (in `main.R`, or before sourcing
  `01_load_data.R`) to replace every file with the latest release.
- `RAW_DATA/download_log.csv` records the source, link and date of each download.

| File | Source | Series / ID | Frequency, unit |
|---|---|---|---|
| `bfs_cpi_LIK25B25.xlsx` | BFS (FSO), via DAM API | LIK detailed results since 1982, basket 2025, Dec 2025 = 100, BFS no. su-d-05.02.66 | Monthly index, NSA |
| `bfs_cpi_weights.xlsx` | BFS (FSO), via DAM API | LIK weights of all baskets since 1914, BFS no. su-d-05.02.90 | Annual weights, % |
| `seco_qna_p_cssa.xlsx` | SECO | Quarterly GDP, production approach, seasonally, calendar and sport-event adjusted | Quarterly, CHF mn |
| `snb_fx_devkum.csv` | SNB data portal API | Cube `devkum`, M0, EUR1 and USD1 | Monthly average, CHF per unit |
| `snb_policy_rates_snboffzisa.csv` | SNB data portal API | Cube `snboffzisa` (LZ = SNB policy rate since June 2019; UG0/OG0 = 3M Libor target range 2000–2019) | End of month, % |
| `fred_brent_MCOILBRENTEU.csv` | FRED API (`fredr`) | `MCOILBRENTEU`, Brent crude | Monthly average, USD per barrel, NSA |
| `eurostat_hicp_ea.csv` | Eurostat API (`eurostat` package) | `prc_hicp_minr`, EA, I25, TOTAL and TOT_X_NRG | Monthly index 2025 = 100, NSA |
| `bfs_unemployed_by_canton.csv` | BFS (source: SECO), via DAM API | Registered unemployed by canton, BFS no. ts-x-03.03.02.04 | Monthly, persons |
| `bwo_reference_rate.xlsx` | Federal Office for Housing (BWO) | Mortgage reference rate and average mortgage rate | Quarterly, % |

`bwo_reference_rate.xlsx` is the only file not downloaded by the script: the BWO
publishes no API or file, so the table was copied from
<https://www.bwo.admin.ch/de/entwicklung-referenzzinssatz-und-durchschnittszinssatz>.
To update it, copy the new rows from that page.

R packages: `here`, `tidyverse`, `readxl`, `jsonlite`, `curl`, `eurostat`, `fredr`.

Downloading Brent needs a free FRED API key (create an account on fred.stlouisfed.org,
then My Account → API Keys). Save it once with `file.edit("~/.Renviron")`, adding the line
`FRED_API_KEY=your_key`, and restart R. Never commit the key. It is only needed to
download: reading the snapshot in `RAW_DATA/` works without it.
