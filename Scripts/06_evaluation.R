# 06_evaluation.R ------------------------------------------------------------
# Pseudo out-of-sample evaluation of the bottom-up headline forecast.
#
# Recursive (expanding-window) scheme: for each origin t from first_origin to
# the second-to-last quarter, we re-estimate all four block models on
# 2000Q1..t only, forecast h = 1, ..., 12 with random-walk Brent and
# USD/CHF, and aggregate to headline y-y inflation. Nothing after t is used,
# so the forecasts are what we could have produced in real time (apart from
# data revisions, which are small for the CPI).
#
# Outputs
#   figures/spaghetti_headline.png   every forecast path against the outcome
#   output/eval_forecasts.csv        all forecasts and outcomes
#   output/eval_absolute.csv         absolute standards, by horizon
#   output/eval_relative.csv         relative standards, by horizon
#
# Needs (from main.R): start_estimation, first_origin, fcst_horizon, arima_specs

library(here)
library(tidyverse)
library(sandwich)
source(here("R", "models.R"))

dir.create(here("output"), showWarnings = FALSE)
dir.create(here("figures"), showWarnings = FALSE)

q <- load_quarterly(start_estimation)
H <- fcst_horizon
origins <- which(q$date >= first_origin & q$date < max(q$date))
cat("Origins:", format(q$date[min(origins)]), "to", format(q$date[max(origins)]),
    "(", length(origins), "origins )\n")


# 1. Recursive forecasts ------------------------------------------------------
# Two benchmarks, re-estimated at every origin as well:
#   arima: an ARIMA directly on headline CPI (spec in main.R), i.e. no
#          disaggregation. Tells us whether the bottom-up approach adds value.
#   rw:    y-y inflation stays at its last observed value. The naive
#          forecast any model should beat.
#   mean:  the course's naive white-noise benchmark (slides 3.1.2 II): the
#          sample mean of y-y inflation up to the origin. 1 - MSFE ratio
#          against it is the predictive R-squared.

recursive <- map_dfr(origins, function(t) {
  hist <- q[1:t, ]
  fc <- forecast_point(hist, H, arima_specs)

  y <- hist$y_cpi
  y_all <- c(y, drop(arima_paths(fit_arima(y, arima_specs$headline), y, H)))
  pi_arima <- y_all[length(y) + 1:H] - y_all[length(y) + 1:H - 4]

  tibble(origin = q$date[t], h = 1:H, target = fc$dates,
         model = fc$headline, arima = pi_arima, rw = hist$pi_cpi[t],
         mean = mean(hist$pi_cpi, na.rm = TRUE),
         # information known at the origin, for the strong-efficiency test
         pi_origin = hist$pi_cpi[t],
         brent_yy_origin = hist$y_brent[t] - hist$y_brent[t - 4])
})

recursive <- recursive %>%
  left_join(q %>% select(target = date, actual = pi_cpi), by = "target")
write_csv(recursive, here("output", "eval_forecasts.csv"))


# 2. Spaghetti chart ----------------------------------------------------------
# Each thin line is the 12-quarter forecast made at one origin, starting from
# the inflation rate observed at that origin. The thick line is the outcome.

paths <- bind_rows(
  recursive %>% select(origin, target, value = model),
  recursive %>% distinct(origin, value = pi_origin) %>% mutate(target = origin))

spaghetti <- ggplot() +
  geom_hline(yintercept = c(0, 2), linetype = "dotted", colour = "grey50") +
  geom_line(data = paths, aes(target, value, group = origin),
            colour = "#2b6cb0", alpha = 0.35, linewidth = 0.4) +
  geom_line(data = filter(q, date >= first_origin - years(1)),
            aes(date, pi_cpi), colour = "black", linewidth = 0.9) +
  labs(title = "Swiss CPI inflation: recursive bottom-up forecasts",
       subtitle = paste0("One line per origin, ", format(min(recursive$origin), "%Y"),
                         "-", format(max(recursive$origin), "%Y"),
                         ", horizons 1 to ", H, " quarters. Black: outcome."),
       x = NULL, y = "Year-on-year inflation, %",
       caption = "Source: BFS, FRED, SNB; own calculations. Dotted lines: SNB price-stability range (0-2%).") +
  theme_minimal(base_size = 11)
ggsave(here("figures", "spaghetti_headline.png"), spaghetti,
       width = 9, height = 5, dpi = 300, bg = "white")


# 3. Absolute standards, by horizon -------------------------------------------
# Course definitions (lecture slides, section 3.1.1). e = actual - forecast.
# Errors at horizon h overlap and are (at least) MA(h-1), so the regression
# tests use Newey-West (HAC) standard errors with h-1 lags.
#   I.   Mincer-Zarnowitz: actual = a + b * forecast;  H0: a = 0, b = 1.
#        Joint test of unbiasedness and (strong) efficiency.
#   I.   Unbiasedness: e = tau;  H0: tau = 0 (robust t-test).
#   I.   Strong efficiency: e = beta' Omega_t, Omega_t = constant + variables
#        known at the origin;  H0: beta = 0 (all of them, constant included).
#        Our Omega_t: constant, the forecast itself, the last observed
#        headline inflation and the y-y change in Brent at the origin.
#   II.  Weak efficiency: h-step errors are at most MA(h-1). We fit an
#        MA(h-1) to the errors and test that its residuals are white noise
#        (Ljung-Box, 4 lags). For h = 1 this is a Ljung-Box test on the
#        errors themselves.
#   II.3 The variance of the errors should not decrease with h (see the
#        error-variance chart).

evaluated <- recursive %>% filter(!is.na(actual)) %>% mutate(error = actual - model)

absolute <- evaluated %>%
  group_by(h) %>%
  group_modify(function(d, key) {
    h <- key$h
    mz     <- lm(actual ~ model, data = d)
    unbias <- lm(error ~ 1, data = d)
    strong <- lm(error ~ model + pi_origin + brent_yy_origin, data = d)
    tibble(n            = nrow(d),
           mean_error   = mean(d$error),
           var_error    = mean((d$error - mean(d$error))^2),
           rmse         = sqrt(mean(d$error^2)),
           mz_alpha     = coef(mz)[[1]],
           mz_beta      = coef(mz)[[2]],
           mz_p         = wald_hac(mz, diag(2), c(0, 1), h),
           unbiased_p   = wald_hac(unbias, matrix(1), 0, h),
           strong_eff_p = wald_hac(strong, diag(4), rep(0, 4), h),
           weak_eff_p   = weak_efficiency_p(d$error, h))
  }) %>%
  ungroup()

cat("\nAbsolute standards (p-values: small = reject)\n")
print(absolute %>% mutate(across(-c(h, n), ~ round(.x, 3))), n = Inf)
write_csv(absolute, here("output", "eval_absolute.csv"))

# II.3: variance of the h-step errors as a function of h
variance_chart <- ggplot(absolute, aes(h, var_error)) +
  geom_line(colour = "#2b6cb0", linewidth = 0.9) +
  geom_point(colour = "#2b6cb0", size = 2) +
  scale_x_continuous(breaks = 1:H) +
  labs(title = "Variance of the out-of-sample forecast errors, by horizon",
       subtitle = "Optimal forecasts have error variances that do not decrease with h",
       x = "Horizon h (quarters)", y = "Variance of errors, pp squared") +
  theme_minimal(base_size = 11)
ggsave(here("figures", "error_variance_by_h.png"), variance_chart,
       width = 7, height = 4, dpi = 300, bg = "white")


# 4. Relative standards, by horizon -------------------------------------------
# Ratio < 1: the bottom-up model beats the benchmark. DM p-value: is the
# difference in squared (MSFE) or absolute (MAFE) errors significant?
# DM test as in the slides (3.1.2 IV.2): Newey-West long-run variance of the
# loss differential, Harvey-Leybourne-Newbold correction, Student t.
# The bottom-up model is not nested in any of the benchmarks.

relative <- evaluated %>%
  group_by(h) %>%
  group_modify(function(d, key) {
    h <- key$h
    e <- d$actual - d$model
    map_dfr(c("arima", "rw", "mean"), function(b) {
      e_b <- d$actual - d[[b]]
      tibble(benchmark  = b,
             msfe_ratio = mean(e^2) / mean(e_b^2),
             dm_msfe_p  = dm_test(e, e_b, h, "squared")[["p"]],
             mafe_ratio = mean(abs(e)) / mean(abs(e_b)),
             dm_mafe_p  = dm_test(e, e_b, h, "absolute")[["p"]])
    })
  }) %>%
  ungroup() %>%
  arrange(benchmark, h)

cat("\nRelative standards (ratio < 1 = bottom-up model better)\n")
print(relative %>% mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
write_csv(relative, here("output", "eval_relative.csv"))
