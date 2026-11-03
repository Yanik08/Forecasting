# 05_forecasting.R -----------------------------------------------------------
# Final forecast of headline y-y inflation, 2026Q4 to 2029Q3, from the last
# observed quarter (2026Q3):
#   1. point forecast: optimal h-step forecasts of the four blocks (random-walk
#      Brent and USD/CHF), aggregated with the latest weights held fixed;
#   2. density forecast: simulated paths;
#   3. oil-shock scenario: permanent rise in Brent (size and date in main.R).
#
# Density: for each simulated path and each future quarter we draw ONE past
# quarter at random and take, from that same quarter, the residuals of all
# four block models and the changes in Brent and USD/CHF. Drawing the same
# quarter for everything keeps the correlation between blocks and drivers
# (e.g. an oil shock hits Brent and the oil block at the same time).
# Parameter uncertainty is not included.
#
# Outputs
#   output/forecast_headline.csv   point, quantiles and scenario, by quarter
#   output/forecast_blocks.csv     point forecast of each block (y-y, %)
#   figures/fan_chart_headline.png
#
# Needs (from main.R): start_estimation, fcst_horizon, arima_specs,
#                      n_sims, seed, oil_shock_date, oil_shock_size

library(here)
library(tidyverse)
source(here("R", "models.R"))

dir.create(here("output"), showWarnings = FALSE)
dir.create(here("figures"), showWarnings = FALSE)
set.seed(seed)

q <- load_quarterly(start_estimation)
H <- fcst_horizon
n <- nrow(q)
w <- latest_weights(q)
cat("Forecast origin:", format(max(q$date)), " weights:",
    paste(names(w), round(w, 2), collapse = ", "), "\n")


# 1. Point forecast ----------------------------------------------------------

point <- forecast_point(q, H, arima_specs)
fc_dates <- point$dates

# y-y inflation of each block, for the table (forecast log levels minus the
# level four quarters earlier, observed or forecast)
block_yy <- map_dfc(setNames(nm = block_names), function(b) {
  y_all <- c(q[[paste0("y_", b)]], drop(point$blocks[[b]]))
  y_all[n + 1:H] - y_all[n + 1:H - 4]
})
forecast_blocks <- bind_cols(date = fc_dates, block_yy, headline = point$headline)
print(forecast_blocks %>% mutate(across(-date, ~ round(.x, 2))))
write_csv(forecast_blocks, here("output", "forecast_blocks.csv"))


# 2. Density forecast --------------------------------------------------------

N <- n_sims
ecm <- fit_ecm(q$y_oil, q$y_brent, q$y_usd)
arima_fits <- lapply(setNames(nm = setdiff(block_names, "oil")),
                     function(b) fit_arima(q[[paste0("y_", b)]], arima_specs[[b]]))

# Residuals and driver changes as full-length vectors indexed by quarter
res <- list(oil = c(NA, NA, unname(resid(ecm$sr))))      # ECM loses 2 quarters
for (b in names(arima_fits)) res[[b]] <- as.numeric(residuals(arima_fits[[b]]))
d_brent <- c(NA, diff(q$y_brent))
d_usd   <- c(NA, diff(q$y_usd))

# One random past quarter per future quarter and path (quarters 3..n have
# every residual)
draw <- matrix(sample(3:n, H * N, replace = TRUE), H, N)
pick <- function(x) matrix(x[draw], H, N)

brent_sim <- q$y_brent[n] + apply(pick(d_brent), 2, cumsum)
usd_sim   <- q$y_usd[n]   + apply(pick(d_usd),   2, cumsum)

sim_y <- list(oil = ecm_paths(ecm, H, brent_sim, usd_sim, shocks = pick(res$oil)))
for (b in names(arima_fits)) {
  sim_y[[b]] <- arima_paths(arima_fits[[b]], q[[paste0("y_", b)]], H,
                            shocks = pick(res[[b]]))
}
sim_pi <- aggregate_headline(q, fc_dates, sim_y, w)     # H x N

probs <- c(0.05, 0.16, 0.50, 0.84, 0.95)
quantiles <- t(apply(sim_pi, 1, quantile, probs = probs))
colnames(quantiles) <- paste0("q", probs * 100)


# 3. Oil-shock scenario ------------------------------------------------------
# Brent rises permanently by oil_shock_size % from oil_shock_date; USD/CHF
# and everything else as in the baseline. Point forecast only.

brent_shock <- random_walk(q$y_brent, H) +
  ifelse(fc_dates >= oil_shock_date, log100(1 + oil_shock_size / 100), 0)
shock_y <- point$blocks
shock_y$oil <- ecm_paths(ecm, H, brent_shock, random_walk(q$y_usd, H))
shock_pi <- drop(aggregate_headline(q, fc_dates, shock_y, w))

forecast_headline <- tibble(date = fc_dates, point = point$headline) %>%
  bind_cols(as_tibble(quantiles)) %>%
  mutate(oil_shock = shock_pi, shock_effect_pp = oil_shock - point)
cat("\nHeadline y-y inflation forecast (%)\n")
print(forecast_headline %>% mutate(across(-date, ~ round(.x, 2))))
write_csv(forecast_headline, here("output", "forecast_headline.csv"))


# 4. Fan chart ----------------------------------------------------------------

history <- q %>% filter(date >= as.Date("2018-01-01")) %>% select(date, pi_cpi)
last <- tail(history, 1)
fan <- forecast_headline %>%      # start the bands at the last observation
  bind_rows(tibble(date = last$date, point = last$pi_cpi, q5 = last$pi_cpi,
                   q16 = last$pi_cpi, q50 = last$pi_cpi, q84 = last$pi_cpi,
                   q95 = last$pi_cpi, oil_shock = last$pi_cpi)) %>%
  arrange(date)

fan_chart <- ggplot(fan, aes(date)) +
  annotate("rect", xmin = min(history$date), xmax = max(fan$date),
           ymin = 0, ymax = 2, fill = "grey92") +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "#2b6cb0", alpha = 0.18) +
  geom_ribbon(aes(ymin = q16, ymax = q84), fill = "#2b6cb0", alpha = 0.30) +
  geom_line(aes(y = point), colour = "#2b6cb0", linewidth = 1) +
  geom_line(aes(y = oil_shock), colour = "#c05621", linewidth = 0.8,
            linetype = "dashed") +
  geom_line(data = history, aes(date, pi_cpi), colour = "black", linewidth = 0.9) +
  labs(title = "Swiss CPI inflation forecast, 2026Q4-2029Q3",
       subtitle = paste0("Blue: point forecast with 68% and 90% bands. Orange dashed: Brent +",
                         oil_shock_size, "% from ", format(oil_shock_date, "%Y"), "Q",
                         lubridate::quarter(oil_shock_date), ". Grey area: SNB range 0-2%."),
       x = NULL, y = "Year-on-year inflation, %",
       caption = "Source: BFS, FRED, SNB; own calculations.") +
  theme_minimal(base_size = 11)
ggsave(here("figures", "fan_chart_headline.png"), fan_chart,
       width = 9, height = 5, dpi = 300, bg = "white")
