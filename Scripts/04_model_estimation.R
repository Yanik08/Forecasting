# 04_model_estimation.R ------------------------------------------------------
# Estimates the four block models on the full sample and runs the checks we
# report: coefficients, residual diagnostics, cointegration tests for the
# oil ECM, the re-aggregation check, and a check that our hand-written
# forecast recursion gives the same numbers as R's own predict().
#
# Needs (from main.R): start_estimation, arima_specs, fcst_horizon
# Writes: output/estimates_arima.csv, output/estimates_ecm.csv,
#         output/reaggregation_check.csv

library(here)
library(tidyverse)
library(urca)
source(here("R", "models.R"))

dir.create(here("output"), showWarnings = FALSE)
q <- load_quarterly(start_estimation)
cat("Estimation sample:", format(min(q$date)), "to", format(max(q$date)),
    "(", nrow(q), "quarters )\n")


# 1. ARIMA blocks ------------------------------------------------------------
# The orders are set in main.R (arima_specs). The estimated equations match
# the table in section 2 of the model doc.

arima_fits <- lapply(setNames(nm = c("rent", "imp_xoil", "dom_xrent")),
                     function(b) fit_arima(q[[paste0("y_", b)]], arima_specs[[b]]))

arima_table <- imap_dfr(arima_fits, function(fit, b) {
  se <- sqrt(diag(fit$var.coef))
  tibble(block = b, term = names(coef(fit)), estimate = coef(fit),
         std_error = se, t_stat = coef(fit) / se)
})
print(arima_table, n = Inf)

# Residual diagnostics: Ljung-Box on 8 lags (two years), degrees of freedom
# corrected for the number of ARMA coefficients.
diagnostics <- imap_dfr(arima_fits, function(fit, b) {
  k  <- sum(fit$arma[1:4])
  lb <- Box.test(residuals(fit), lag = 8, type = "Ljung-Box", fitdf = k)
  tibble(block = b, aic = AIC(fit), bic = BIC(fit), ljung_box_p = lb$p.value)
})
print(diagnostics)

# The AR polynomial of imported excl. oil has an inverse root close to -1,
# which nearly cancels the MA root: a sign of over-fitting (see the doc).
inverse_roots <- function(fit) {
  pl <- arima_polys(fit)
  list(ar = 1 / polyroot(c(1, -pl$a)), ma = 1 / polyroot(c(1, pl$m)))
}
print(inverse_roots(arima_fits$imp_xoil))

write_csv(left_join(arima_table, diagnostics, by = "block"),
          here("output", "estimates_arima.csv"))


# 2. Oil ECM -----------------------------------------------------------------

ecm <- fit_ecm(q$y_oil, q$y_brent, q$y_usd)
print(summary(ecm$lr))   # long-run relation (standard errors not valid: I(1))
print(summary(ecm$sr))   # short-run dynamics

# Unit roots in levels (ADF with trend, lags by BIC). H0: unit root.
for (v in c("y_oil", "y_brent", "y_usd")) {
  adf <- ur.df(q[[v]], type = "trend", lags = 4, selectlags = "BIC")
  cat(v, ": ADF tau =", round(adf@teststat[1], 2),
      " 5% critical value =", adf@cval[1, 2], "\n")
}

# Engle-Granger: ADF on the long-run residuals. The critical value is NOT the
# usual ADF one: with 2 regressors plus a trend, the 5% value is about -4.1
# (MacKinnon 2010).
eg <- ur.df(ecm$ect, type = "none", lags = 4, selectlags = "BIC")
cat("Engle-Granger tau =", round(eg@teststat[1], 2), "(5% critical value about -4.1)\n")

# Johansen trace test with a trend in the cointegrating relation
jo <- ca.jo(data.frame(oil = q$y_oil, brent = q$y_brent, usd = q$y_usd),
            type = "trace", ecdet = "trend", K = 2, spec = "transitory")
print(summary(jo))

# H0: Brent and USD have the same long-run elasticity (then Brent in CHF
# would be enough). Rejected in the doc (p < 0.001). F-test of the restricted
# model (Brent in CHF = brent + usd) against the unrestricted one; with I(1)
# regressors read it as indicative only.
lr_restricted   <- lm(y_oil ~ I(y_brent + y_usd) + seq_along(y_oil), data = q)
lr_unrestricted <- lm(y_oil ~ y_brent + y_usd + seq_along(y_oil), data = q)
print(anova(lr_restricted, lr_unrestricted))

ecm_table <- bind_rows(
  tibble(equation = "long run", term = names(coef(ecm$lr)), estimate = coef(ecm$lr)),
  tibble(equation = "short run", term = names(coef(ecm$sr)), estimate = coef(ecm$sr)))
write_csv(ecm_table, here("output", "estimates_ecm.csv"))


# 3. Check: our forecast recursion = predict() --------------------------------
# arima_paths() implements the derivation in section 2 of the doc. With zero
# shocks it must give the same point forecast as R's Kalman-filter predict().

for (b in names(arima_fits)) {
  y <- q[[paste0("y_", b)]]
  ours  <- drop(arima_paths(arima_fits[[b]], y, fcst_horizon))
  theirs <- predict(arima_fits[[b]], n.ahead = fcst_horizon,
                    newxreg = length(y) + 1:fcst_horizon)$pred
  cat(b, ": max difference with predict() =",
      signif(max(abs(ours - theirs)), 3), "\n")
}


# 4. Re-aggregation check ----------------------------------------------------
# Rebuild headline CPI from the four blocks with each year's weights, linked
# at Q4, and compare y-y inflation with the published headline. The gap
# comes from linking at the Q4 average instead of December, and from
# rounding in the published indices.

all_q <- load_quarterly(as.Date("1987-01-01"))
rebuilt <- all_q %>%
  mutate(base_date = as.Date(paste0(year - 1, "-10-01"))) %>%
  left_join(all_q %>% select(base_date = date, b_cpi = cpi_total, b_oil = cpi_oil,
                             b_rent = cpi_rent, b_imp = cpi_imp_xoil,
                             b_dom = cpi_dom_xrent),
            by = "base_date") %>%
  mutate(link = (w_oil * cpi_oil / b_oil + w_rent * cpi_rent / b_rent +
                 w_imp_xoil * cpi_imp_xoil / b_imp +
                 w_dom_xrent * cpi_dom_xrent / b_dom) /
                (w_oil + w_rent + w_imp_xoil + w_dom_xrent))

# Chain the links year by year: each Q4 rebuilt level is the base of the
# following year.
rebuilt$cpi_rebuilt <- NA_real_
first <- which(!is.na(rebuilt$link))[1]
rebuilt$cpi_rebuilt[first - 1] <- rebuilt$cpi_total[first - 1]
for (i in first:nrow(rebuilt)) {
  base_i <- match(rebuilt$base_date[i], rebuilt$date)
  rebuilt$cpi_rebuilt[i] <- rebuilt$cpi_rebuilt[base_i] * rebuilt$link[i]
}

reagg <- rebuilt %>%
  mutate(pi_published = log100(cpi_total / lag(cpi_total, 4)),
         pi_rebuilt   = log100(cpi_rebuilt / lag(cpi_rebuilt, 4)),
         gap_pp       = pi_rebuilt - pi_published) %>%
  filter(date >= start_estimation) %>%
  select(date, pi_published, pi_rebuilt, gap_pp)

cat("Re-aggregation, y-y headline inflation since", format(start_estimation), ":\n",
    " max |gap| =", round(max(abs(reagg$gap_pp)), 3), "pp,",
    " mean |gap| =", round(mean(abs(reagg$gap_pp)), 3), "pp\n")
write_csv(reagg, here("output", "reaggregation_check.csv"))
