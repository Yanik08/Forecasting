# Helper functions --------------------------------------------------------
# Used by scripts/01_load_data.R.

# Download `url` to `dest` exactly as the source serves it (raw, untouched)
# and record it in download_log.csv next to the file (file, source, url,
# date), so we always know which vintage of the data we used.
download_raw <- function(url, dest, source) {
  message("Downloading: ", basename(dest))
  # Some servers refuse requests that don't look like a browser (SECO) or
  # break over HTTP/2 (FRED), so we send a browser user agent and use HTTP/1.1
  # (http_version = 2 is curl's code for HTTP/1.1).
  handle <- curl::new_handle(useragent = "Mozilla/5.0", http_version = 2,
                             failonerror = TRUE)
  ok <- tryCatch({
    curl::curl_download(url, dest, handle = handle, quiet = TRUE)
    TRUE
  }, error = function(e) { message(conditionMessage(e)); FALSE })
  if (!ok) {
    unlink(dest)                                  # never keep a broken file
    stop("Download failed for ", basename(dest), " from ", url)
  }
  log_download(dest, source, url)
}

# Add (or replace) the line for `dest` in download_log.csv.
log_download <- function(dest, source, url) {
  log_file <- file.path(dirname(dest), "download_log.csv")
  entry <- data.frame(file = basename(dest), source = source, url = url,
                      downloaded = format(Sys.time(), "%Y-%m-%d %H:%M"))
  if (file.exists(log_file)) {
    log <- utils::read.csv(log_file, stringsAsFactors = FALSE)
    entry <- rbind(log[log$file != entry$file, ], entry)
  }
  utils::write.csv(entry[order(entry$file), ], log_file, row.names = FALSE)
  invisible(dest)
}

# BFS (Federal Statistical Office): download link of the latest release of a
# table, found through the DAM API from its BFS number (e.g. "su-d-05.02.66").
# Every release gets a new asset id, but the BFS number stays the same.
bfs_latest <- function(bfs_number) {
  api <- paste0("https://dam-api.bfs.admin.ch/hub/api/dam/assets?orderNr=", bfs_number)
  res <- jsonlite::fromJSON(api, simplifyVector = FALSE)
  if (length(res$data) == 0) stop("No BFS asset found for ", bfs_number)
  ids   <- vapply(res$data, function(a) a$ids$damId, numeric(1))
  asset <- res$data[[which.max(ids)]]                       # newest release
  Filter(function(l) l$rel == "master", asset$links)[[1]]$href
}

# SNB data portal: CSV link for a data cube. dim_sel picks series,
# e.g. "D0(M0),D1(EUR1,USD1)"; NULL takes every series in the cube.
# Always give `from`: without it some cubes return only the last 13 months.
snb_url <- function(cube, from, dim_sel = NULL) {
  url <- paste0("https://data.snb.ch/api/cube/", cube, "/data/csv/en?fromDate=", from)
  if (!is.null(dim_sel)) url <- paste0(url, "&dimSel=", dim_sel)
  url
}

# SNB CSV files start with 2 metadata lines (cube id, publishing date) and a
# blank line; values are ";" separated.
read_snb <- function(path) {
  readr::read_delim(path, delim = ";", skip = 2, show_col_types = FALSE)
}

# Excel stores dates as days since 1899-12-30; readxl turns date column
# headers into these numbers ("30286" = 1982-12-01).
excel_date <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")

# SECO quarterly GDP workbook: one sheet per concept (nom_q, real_q, defl_q).
# Row 11 holds the ESA codes (B1GQ = GDP, ...) above the level columns; each
# level column is followed by its q-o-q growth rate, which we drop.
read_seco_q <- function(path, sheet) {
  x <- readxl::read_excel(path, sheet = sheet, skip = 10, col_names = FALSE,
                          .name_repair = "minimal")
  codes <- as.character(unlist(x[1, ]))
  level_cols <- which(!is.na(codes) & seq_along(codes) > 2)
  body <- x[-1, ]
  body <- body[grepl("^[0-9]{4}$", body[[1]]), ]           # data rows only
  values <- lapply(level_cols, function(j) as.numeric(body[[j]]))
  names(values) <- codes[level_cols]
  tibble::tibble(year = as.integer(body[[1]]), quarter = as.integer(body[[2]])) |>
    dplyr::bind_cols(tibble::as_tibble(values)) |>
    tidyr::pivot_longer(-c(year, quarter), names_to = "code", values_to = "value") |>
    dplyr::mutate(date = lubridate::make_date(year, 3 * quarter - 2, 1),
                  concept = sheet, .before = 1)
}
