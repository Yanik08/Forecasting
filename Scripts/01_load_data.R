# 01_load_data.R -----------------------------------------------------------
# Downloads every raw data file into RAW_DATA/ and reads it into R.
#
# - A file is downloaded only if it is not in RAW_DATA/ yet, so a normal run
#   uses the committed snapshots and reproduces our results exactly.
# - Set refresh_raw <- TRUE (in main.R) to replace all snapshots with the
#   latest releases; download_log.csv records when each file was fetched.
# - Raw files are never edited. All cleaning happens in 02_clean_data.R.
#
# Packages: here, tidyverse, readxl, jsonlite, curl, eurostat, fredr

library(here)
library(tidyverse)
library(readxl)
source(here("R", "functions.R"))

if (!exists("refresh_raw")) refresh_raw <- FALSE
raw_dir <- here("RAW_DATA")
dir.create(raw_dir, showWarnings = FALSE)
raw <- function(file) file.path(raw_dir, file)
needs_download <- function(file) refresh_raw || !file.exists(raw(file))


# 1. Swiss CPI, detailed indices ------------------------------------------
# BFS, LIK (Dec 2025 = 100), detailed results since 1982, basket structure
# 2025, incl. special aggregates [LIK25B25], BFS number su-d-05.02.66.
# Monthly, not seasonally adjusted. Includes the aggregates we need for the
# blocks: petroleum products (112_101), housing rental (113_101), domestic
# (1819_118) and imported products (1819_119).
# The file only holds the weights of the current year (2026), so we do not
# keep them here: the weights of every year are read in part 2.
f_cpi <- "bfs_cpi_LIK25B25.xlsx"
if (needs_download(f_cpi)) {
  download_raw(bfs_latest("su-d-05.02.66"), raw(f_cpi), "BFS su-d-05.02.66")
}

cpi_index <- read_excel(raw(f_cpi), sheet = "INDEX_m", skip = 3,
                        na = c("", "...")) |>         # "..." = not available yet
  filter(!is.na(PosNo)) |>                       # drop footnotes under the table
  distinct(Code, .keep_all = TRUE) |>            # Total and main groups are repeated
                                                 # above the special aggregates
  select(code = 1, pos_no = 2, level = 4, coicop = 5, item = 13,
         matches("^[0-9]{5}$")) |>               # monthly columns (Excel dates)
  mutate(across(matches("^[0-9]{5}$"), as.numeric)) |>  # footnote text made some columns character
  pivot_longer(matches("^[0-9]{5}$"), names_to = "date", values_to = "index") |>
  mutate(date = excel_date(date),
         coicop = str_remove(coicop, "^'"))


# 2. Swiss CPI, weights of every basket since 1914 -------------------------
# BFS, LIK detailed weights of the original baskets [GEWICHTUNGEN],
# BFS number su-d-05.02.90. One sheet per basket (see read_cpi_weights()).
# Weights in % of the total. Until 1993 one weight per basket, valid until
# the next basket; since 2000 one weight per year. Codes (e.g. "1819_119",
# as in cpi_index) exist only from basket 2015 (years 2016+); older baskets
# are identified by pos_no and item, and their numbering differs by basket.
# The sheet LIK20->LIK25 maps the 2020 basket to the 2025 one and holds no
# new weights, so it is skipped.
f_cpi_w <- "bfs_cpi_weights.xlsx"
if (needs_download(f_cpi_w)) {
  download_raw(bfs_latest("su-d-05.02.90"), raw(f_cpi_w), "BFS su-d-05.02.90")
}

cpi_weights <- excel_sheets(raw(f_cpi_w)) |>
  setdiff("LIK20->LIK25") |>
  map(\(s) read_cpi_weights(raw(f_cpi_w), s)) |>
  list_rbind()


# 3. Swiss GDP, quarterly -------------------------------------------------
# SECO, quarterly national accounts, production approach, seasonally,
# calendar and sport-event adjusted (qna_p_cssa). CHF million; real = chained
# volumes, reference year 2020. If the link breaks after a SECO update, take
# the new one from https://www.seco.admin.ch/en/gross-domestic-product
f_gdp <- "seco_qna_p_cssa.xlsx"
if (needs_download(f_gdp)) {
  download_raw("https://www.seco.admin.ch/dam/en/sd-web/ov1gH4D7B7tT/qna_p_cssa.xlsx",
               raw(f_gdp), "SECO qna_p_cssa")
}

gdp <- map(c("nom_q", "real_q", "defl_q"), \(s) read_seco_q(raw(f_gdp), s)) |>
  list_rbind()


# 4. Exchange rates CHF per EUR and per USD --------------------------------
# SNB data portal, cube devkum, monthly averages (M0), CHF per 1 EUR / 1 USD.
f_fx <- "snb_fx_devkum.csv"
if (needs_download(f_fx)) {
  download_raw(snb_url("devkum", from = "1987-08", dim_sel = "D0(M0),D1(EUR1,USD1)"), raw(f_fx), "SNB devkum")
}

fx <- read_snb(raw(f_fx)) |>
  transmute(date = ym(Date), currency = str_remove(D1, "1$"), chf_per_unit = Value)


# 5. Official policy rates ------------------------------------------------
# SNB data portal, cube snboffzisa, end of month, in %, since 2000.
# LZ = SNB policy rate (only since June 2019); UG0 / OG0 = lower / upper bound
# of the SNB's target range for the 3-month CHF Libor (Jan 2000 - June 2019).
# Also contains Fed (UG1, OG1), ECB (H, SRF, EF), BoE (L0) and BoJ (L1) rates.
f_policy <- "snb_policy_rates_snboffzisa.csv"
if (needs_download(f_policy)) {
  download_raw(snb_url("snboffzisa", from = "2000-01",
                       dim_sel = "D0(LZ,UG0,OG0,UG1,OG1,H,SRF,EF,L0,L1)"),
               raw(f_policy), "SNB snboffzisa")
}

policy_rates <- read_snb(raw(f_policy)) |>
  transmute(date = ym(Date), series = D0, rate = Value)


# 6. Brent crude oil price ------------------------------------------------
# FRED, series MCOILBRENTEU, USD per barrel, monthly average, not seasonally
# adjusted. Downloaded through the FRED API (fredr), which needs a free API
# key saved as FRED_API_KEY in ~/.Renviron (see README). The key is only
# needed to download: the committed snapshot can be read without it.
f_brent <- "fred_brent_MCOILBRENTEU.csv"
if (needs_download(f_brent)) {
  fredr::fredr(series_id = "MCOILBRENTEU") |>
    select(date, series_id, value) |>
    write_csv(raw(f_brent))
  log_download(raw(f_brent), "FRED MCOILBRENTEU",
               "fredr::fredr(series_id = 'MCOILBRENTEU')")
}

brent <- read_csv(raw(f_brent), show_col_types = FALSE) |>
  select(date, brent_usd = value)


# 7. Euro area HICP -------------------------------------------------------
# Eurostat, prc_hicp_minr (HICP, ECOICOP ver. 2), euro area (EA, changing
# composition), index 2025 = 100, not seasonally adjusted.
# TOTAL = all items, TOT_X_NRG = all items excluding energy.
f_hicp <- "eurostat_hicp_ea.csv"
if (needs_download(f_hicp)) {
  eurostat::get_eurostat("prc_hicp_minr", time_format = "date", cache = FALSE,
                         filters = list(geo = "EA", unit = "I25",
                                        coicop18 = c("TOTAL", "TOT_X_NRG"))) |>
    write_csv(raw(f_hicp))
  log_download(raw(f_hicp), "Eurostat prc_hicp_minr",
               "eurostat::get_eurostat('prc_hicp_minr'), geo=EA, unit=I25")
}

hicp_ea <- read_csv(raw(f_hicp), show_col_types = FALSE) |>
  rename(any_of(c(date = "TIME_PERIOD", date = "time"))) |>
  select(date, coicop18, index = values)


# 8. Registered unemployed by canton --------------------------------------
# BFS (source: SECO, unemployment statistics), monthly since 1973,
# BFS number ts-x-03.03.02.04 (CSV version of je-f-03.03.02.04).
# Note: the switch to the LAMDA system creates a break in Dec 2025 / Jan 2026.
f_unemp <- "bfs_unemployed_by_canton.csv"
if (needs_download(f_unemp)) {
  download_raw(bfs_latest("ts-x-03.03.02.04"), raw(f_unemp), "BFS ts-x-03.03.02.04")
}

unemployed <- read_csv(raw(f_unemp), show_col_types = FALSE,
                       col_types = cols(VALUE = "d", .default = "c")) |>
  transmute(date = ym(PERIOD), region = REGION, canton = CANTON,
            unemployed = VALUE, status = STATUS)


# 9. Mortgage reference rate for rents ------------------------------------
# Federal Office for Housing (BWO/OFL), hypothekarischer Referenzzinssatz
# and average mortgage rate, published quarterly. No API: the table was
# copied by hand from https://www.bwo.admin.ch/de/entwicklung-referenzzinssatz-und-durchschnittszinssatz
# and is the only file in RAW_DATA/ that is not downloaded by this script.
f_bwo <- "bwo_reference_rate.xlsx"
if (!file.exists(raw(f_bwo))) {
  stop(f_bwo, " is missing: it has no API and must be in RAW_DATA/ (see README).")
}

# Rates are mostly text like "1,25 %", but when the table was copied Excel
# turned whole numbers like "2 %" into the number 0.02. Both become 1.25, 2, ...
percent <- function(x) {
  out <- parse_number(x, locale = locale(decimal_mark = ","))
  plain <- !str_detect(x, "%")
  out[plain] <- as.numeric(x[plain]) * 100
  out
}

# Since Dec 2011 the reference rate is the average rate rounded to the
# nearest 0.25 pp (footnote in the source table).
reference_rate <- read_excel(raw(f_bwo), col_types = "text") |>
  set_names(c("reference_rate", "valid_from", "avg_mortgage_rate", "survey_date")) |>
  filter(!is.na(valid_from)) |>                  # drop the footnote row
  transmute(valid_from = excel_date(valid_from),
            reference_rate = percent(reference_rate),
            avg_mortgage_rate = percent(avg_mortgage_rate),
            survey_date = excel_date(survey_date)) |>
  arrange(valid_from)


message("01_load_data.R done: cpi_index, cpi_weights, gdp, fx, policy_rates, brent, ",
        "hicp_ea, unemployed, reference_rate")
