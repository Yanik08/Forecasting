# Model helpers ------------------------------------------------------------
# Used by Scripts/04_model_estimation.R, 05_forecasting.R and 06_evaluation.R.
#
# Notation follows the model doc ("Model Specifications by Block"):
#   y = 100 * log(index), so diff(y) is quarter-on-quarter inflation in %
#   and y_t - y_{t-4} is year-on-year inflation in % (log approximation).
#   T = last observed quarter (the forecast origin), h = 1, ..., H.
#
# All forecast functions return a matrix with one row per horizon and one
# column per path. With zero shocks and one column you get the point
# forecast (the optimal forecast derived in the doc); with random shocks and
# many columns you get simulated paths for the density forecast.

log100 <- function(x) 100 * log(x)

# The four blocks, always in this order
block_names <- c("oil", "rent", "imp_xoil", "dom_xrent")


# 1. Data --------------------------------------------------------------------

# Quarterly data from `start`, with the block weights (in % of the basket)
# and the log variables used by the models.
# The weights come from cpi_weights_full.csv (one row per year: published
# from 2016, recovered before), joined by year. We take them from there
# rather than from quarterly_data.csv, because the current cleaning notebook
# writes quarterly_data.csv without weight columns.
# Block weights: imported excl. oil = imported - oil, domestic excl. rents =
# domestic - rents (all oil is imported, all rents are domestic).
load_quarterly <- function(start) {
  weights <- readr::read_csv(here::here("DTA_CLEAN", "cpi_weights_full.csv"),
                             show_col_types = FALSE) %>%
    dplyr::select(year, w_rent, w_oil, w_domestic, w_imported)
  readr::read_csv(here::here("DTA_CLEAN", "quarterly_data.csv"),
                  show_col_types = FALSE) %>%
    dplyr::select(-dplyr::starts_with("w_")) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::left_join(weights, by = "year") %>%
    dplyr::filter(date >= start) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(w_imp_xoil  = w_imported - w_oil,
                  w_dom_xrent = w_domestic - w_rent,
                  y_cpi       = log100(cpi_total),
                  y_oil       = log100(cpi_oil),
                  y_rent      = log100(cpi_rent),
                  y_imp_xoil  = log100(cpi_imp_xoil),
                  y_dom_xrent = log100(cpi_dom_xrent),
                  y_brent     = log100(brent_usd),     # Brent, USD per barrel
                  y_usd       = log100(chf_per_usd),   # CHF per USD
                  pi_cpi      = y_cpi - dplyr::lag(y_cpi, 4))  # headline y-y, %
}


# 2. ARIMA blocks ------------------------------------------------------------

# ARIMA(p,1,q)(P,0,Q)_4 with drift, on the log level y.
# The drift enters as a linear trend in levels (xreg = 1, 2, ..., n), which
# after differencing is the constant mu of the doc: (diff(y) - mu) is ARMA.
# Estimation by maximum likelihood (stats::arima, CSS for starting values);
# if that fails to converge in a short recursive sample, fall back to CSS.
fit_arima <- function(y, spec) {
  args <- list(x = y, order = spec$order,
               seasonal = list(order = spec$seasonal, period = 4),
               xreg = cbind(drift = seq_along(y)))
  tryCatch(do.call(stats::arima, args),
           error = function(e) do.call(stats::arima, c(args, method = "CSS")))
}

# Multiply two lag polynomials given as coefficient vectors c(1, c1, c2, ...)
polymul <- function(a, b) {
  out <- rep(0, length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    idx <- i:(i + length(b) - 1)
    out[idx] <- out[idx] + a[i] * b
  }
  out
}

# Seasonal polynomial 1 + c1 L^s + c2 L^2s + ...
seasonal_poly <- function(coefs, s = 4) {
  out <- c(1, rep(0, s * length(coefs)))
  if (length(coefs) > 0) out[1 + s * seq_along(coefs)] <- coefs
  out
}

# Step 1 of the doc's derivation: multiply out the AR side,
#   a(L) = phi(L) Phi(L^4) = 1 - a_1 L - ... - a_P L^P,
# and the MA side, m(L) = theta(L) Theta(L^4) = 1 + m_1 L + ...
# so that x_t = sum a_i x_{t-i} + eps_t + sum m_j eps_{t-j}, x_t = diff(y)_t - mu.
arima_polys <- function(fit) {
  cf <- stats::coef(fit)
  get <- function(prefix, k) if (k > 0) unname(cf[paste0(prefix, 1:k)]) else numeric(0)
  p <- fit$arma[1]; q <- fit$arma[2]; P <- fit$arma[3]; Q <- fit$arma[4]
  ar_poly <- polymul(c(1, -get("ar", p)), seasonal_poly(-get("sar", P)))
  ma_poly <- polymul(c(1,  get("ma", q)), seasonal_poly( get("sma", Q)))
  list(a = -ar_poly[-1], m = ma_poly[-1], mu = unname(cf["drift"]))
}

# Steps 2 and 3 of the derivation: iterate x forward, then cumulate back to
# log levels. `shocks` is an h x N matrix of future eps (zeros = point
# forecast: future shocks have expectation zero). Past values of x and of
# the residuals stay as observed, which is why eps_T matters for h = 1 when
# the model has an MA term.
# Returns an h x N matrix of forecast log levels y_{T+1}, ..., y_{T+h}.
arima_paths <- function(fit, y, h, shocks = matrix(0, h, 1)) {
  pl <- arima_polys(fit)
  x_hist <- diff(y) - pl$mu
  e_hist <- as.numeric(stats::residuals(fit))[-1]   # aligned with diff(y)
  n_x <- length(x_hist); N <- ncol(shocks)
  P <- length(pl$a); Q <- length(pl$m)

  X <- matrix(c(x_hist, rep(0, h)), n_x + h, N)
  E <- matrix(c(e_hist, rep(0, h)), n_x + h, N)
  E[n_x + 1:h, ] <- shocks

  for (j in 1:h) {
    t  <- n_x + j
    ar <- if (P > 0) colSums(pl$a * X[t - seq_len(P), , drop = FALSE]) else 0
    ma <- if (Q > 0) colSums(pl$m * E[t - seq_len(Q), , drop = FALSE]) else 0
    X[t, ] <- ar + E[t, ] + ma
  }
  dy <- X[n_x + 1:h, , drop = FALSE] + pl$mu          # q-q inflation forecasts
  y[length(y)] + matrix(apply(dy, 2, cumsum), h, N)    # back to log levels
}


# 3. Oil ECM -----------------------------------------------------------------

# Engle-Granger two-step estimation.
# Long run:  oil_t = b0 + b1 brent_t + b2 usd_t + delta t + ect_t
# Short run: d_oil_t = c + alpha ect_{t-1} + b0 d_brent_t + g0 d_usd_t
#                      + a1 d_oil_{t-1} + b1 d_brent_{t-1} + g1 d_usd_{t-1} + eps_t
fit_ecm <- function(oil, brent, usd) {
  trend <- seq_along(oil)
  lr  <- stats::lm(oil ~ brent + usd + trend)
  ect <- unname(stats::resid(lr))
  d  <- function(x) c(NA, diff(x))
  l1 <- function(x) c(NA, utils::head(x, -1))
  sr_data <- data.frame(d_oil = d(oil), ect_l1 = l1(ect),
                        d_brent = d(brent), d_usd = d(usd),
                        d_oil_l1 = l1(d(oil)),
                        d_brent_l1 = l1(d(brent)), d_usd_l1 = l1(d(usd)))
  sr <- stats::lm(d_oil ~ ., data = sr_data)
  list(lr = lr, sr = sr, ect = ect, n = length(oil),
       oil = oil, brent = brent, usd = usd)
}

# The three forecast equations of the doc, iterated for h = 1, ..., H:
#   d_oil_{T+h} = c + alpha ect_{T+h-1} + b0 d_brent_{T+h} + ... (short run)
#   oil_{T+h}   = oil_{T+h-1} + d_oil_{T+h}
#   ect_{T+h}   = oil_{T+h} - b0 - b1 brent_{T+h} - b2 usd_{T+h} - delta (T+h)
# brent_fc, usd_fc: h x N matrices of driver log levels for T+1, ..., T+h.
# shocks: h x N matrix of short-run shocks (zeros = point forecast).
ecm_paths <- function(fit, h, brent_fc, usd_fc, shocks = matrix(0, h, 1)) {
  b <- stats::coef(fit$sr); lr <- stats::coef(fit$lr)
  n <- fit$n; N <- ncol(shocks)
  oil_prev   <- rep(fit$oil[n], N)
  ect_prev   <- rep(fit$ect[n], N)
  doil_prev  <- rep(fit$oil[n] - fit$oil[n - 1], N)
  brent_prev <- rep(fit$brent[n], N)
  usd_prev   <- rep(fit$usd[n], N)
  dbr_prev   <- rep(fit$brent[n] - fit$brent[n - 1], N)
  dus_prev   <- rep(fit$usd[n] - fit$usd[n - 1], N)

  out <- matrix(NA_real_, h, N)
  for (j in 1:h) {
    dbr <- brent_fc[j, ] - brent_prev
    dus <- usd_fc[j, ] - usd_prev
    doil <- b[["(Intercept)"]] + b[["ect_l1"]] * ect_prev +
      b[["d_brent"]] * dbr + b[["d_usd"]] * dus +
      b[["d_oil_l1"]] * doil_prev +
      b[["d_brent_l1"]] * dbr_prev + b[["d_usd_l1"]] * dus_prev + shocks[j, ]
    oil_now <- oil_prev + doil
    ect_now <- oil_now - lr[["(Intercept)"]] - lr[["brent"]] * brent_fc[j, ] -
      lr[["usd"]] * usd_fc[j, ] - lr[["trend"]] * (n + j)
    out[j, ] <- oil_now
    oil_prev <- oil_now; ect_prev <- ect_now; doil_prev <- doil
    dbr_prev <- dbr; dus_prev <- dus
    brent_prev <- brent_fc[j, ]; usd_prev <- usd_fc[j, ]
  }
  out
}

# Random walk for a driver: the last observed log level, held for h quarters
random_walk <- function(x, h, N = 1) matrix(x[length(x)], h, N)


# 4. Aggregation to headline CPI ---------------------------------------------

# The CPI is chained every December with that year's basket weights. In
# quarterly data we link at Q4 (the quarter that contains December):
#   P_cpi(t) = P_cpi(Q4 of previous year) *
#              sum_i w_i * P_i(t) / P_i(Q4 of previous year) / sum_i w_i
# Forecast quarters use one fixed weight vector (random walk on the weights:
# the last observed weights are held over the horizon).
#
# hist:     observed data up to the origin T (from load_quarterly)
# fc_dates: the h forecast dates
# fc_y:     named list (block_names) of h x N matrices of forecast log levels
# w:        named vector of block weights (same names as block_names)
# Returns an h x N matrix of forecast headline y-y inflation (%).
aggregate_headline <- function(hist, fc_dates, fc_y, w) {
  h <- length(fc_dates); N <- ncol(fc_y[[1]])
  lev <- lapply(fc_y, function(m) exp(m / 100))          # back to index levels
  head_lev <- matrix(NA_real_, h, N)

  hist_level <- function(block, date) {
    col <- if (block == "cpi") "cpi_total" else paste0("cpi_", block)
    hist[[col]][hist$date == date]
  }
  for (j in 1:h) {
    base_date <- as.Date(paste0(lubridate::year(fc_dates[j]) - 1, "-10-01"))
    k <- match(base_date, fc_dates)                      # base in the forecast?
    base_head <- if (is.na(k)) hist_level("cpi", base_date) else head_lev[k, ]
    link <- 0
    for (b in block_names) {
      base_b <- if (is.na(k)) hist_level(b, base_date) else lev[[b]][k, ]
      link <- link + w[[b]] * lev[[b]][j, ] / base_b
    }
    head_lev[j, ] <- base_head * link / sum(w)
  }

  # y-y inflation: the level four quarters earlier is observed or forecast
  y_head <- log100(head_lev)
  pi <- matrix(NA_real_, h, N)
  for (j in 1:h) {
    lag_date <- seq(fc_dates[j], by = "-1 year", length.out = 2)[2]
    k <- match(lag_date, fc_dates)
    y_lag <- if (is.na(k)) log100(hist_level("cpi", lag_date)) else y_head[k, ]
    pi[j, ] <- y_head[j, ] - y_lag
  }
  pi
}

# Block weights of the last observed year, as a named vector
latest_weights <- function(hist) {
  last <- hist[nrow(hist), ]
  c(oil = last$w_oil, rent = last$w_rent,
    imp_xoil = last$w_imp_xoil, dom_xrent = last$w_dom_xrent)
}


# 5. One full forecast from an origin ----------------------------------------

# Fits all four block models on `hist` (data up to the origin) and returns
# the point forecast of headline y-y inflation for h = 1, ..., H, with
# random-walk Brent and USD/CHF. Used for the recursive evaluation (06) and
# the final forecast (05).
forecast_point <- function(hist, H, specs) {
  fc_dates <- seq(max(hist$date), by = "quarter", length.out = H + 1)[-1]
  ecm <- fit_ecm(hist$y_oil, hist$y_brent, hist$y_usd)
  fc_y <- list(oil = ecm_paths(ecm, H, random_walk(hist$y_brent, H),
                               random_walk(hist$y_usd, H)))
  for (b in setdiff(block_names, "oil")) {
    y <- hist[[paste0("y_", b)]]
    fc_y[[b]] <- arima_paths(fit_arima(y, specs[[b]]), y, H)
  }
  list(dates = fc_dates, blocks = fc_y,
       headline = drop(aggregate_headline(hist, fc_dates, fc_y,
                                          latest_weights(hist))))
}


# 6. Evaluation helpers ------------------------------------------------------

# Wald test of R b = r with a HAC (Newey-West) covariance. Forecast errors
# at horizon h overlap, so they follow an MA(h-1): lag = h - 1.
wald_hac <- function(fit, R, r, h) {
  V <- sandwich::NeweyWest(fit, lag = h - 1, prewhite = FALSE, adjust = TRUE)
  diff <- R %*% stats::coef(fit) - r
  stat <- drop(t(diff) %*% solve(R %*% V %*% t(R)) %*% diff)
  stats::pchisq(stat, df = nrow(R), lower.tail = FALSE)
}

# Weak efficiency (course, section 3.1.1 II): h-step errors should be at most
# MA(h-1). Fit an MA(h-1) to the errors and test that its residuals are white
# noise with a Ljung-Box test on `lags` lags (degrees of freedom corrected
# for the h-1 MA coefficients). For h = 1: Ljung-Box on the errors directly.
# Returns the p-value (NA if the MA cannot be estimated).
weak_efficiency_p <- function(e, h, lags = 4) {
  if (h == 1) {
    return(stats::Box.test(e, lag = lags, type = "Ljung-Box")$p.value)
  }
  fit <- tryCatch(stats::arima(e, order = c(0, 0, h - 1)),
                  error = function(err) tryCatch(
                    stats::arima(e, order = c(0, 0, h - 1), method = "CSS"),
                    error = function(err) NULL))
  if (is.null(fit)) return(NA_real_)
  stats::Box.test(stats::residuals(fit), lag = h - 1 + lags,
                  type = "Ljung-Box", fitdf = h - 1)$p.value
}

# Diebold-Mariano test of equal accuracy, with the Harvey-Leybourne-Newbold
# small-sample correction. e1 = model errors, e2 = benchmark errors.
# A negative statistic means the model is more accurate. Two-sided p-value.
dm_test <- function(e1, e2, h, loss = c("squared", "absolute")) {
  loss <- match.arg(loss)
  d <- if (loss == "squared") e1^2 - e2^2 else abs(e1) - abs(e2)
  n <- length(d); dbar <- mean(d)
  gamma <- function(k) sum((d[(k + 1):n] - dbar) * (d[1:(n - k)] - dbar)) / n
  lrv <- gamma(0) + 2 * sum(vapply(seq_len(h - 1),
                                   function(k) (1 - k / h) * gamma(k), numeric(1)))
  stat <- dbar / sqrt(lrv / n) * sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  c(stat = stat, p = 2 * stats::pt(-abs(stat), df = n - 1))
}
