## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)
library(daoh)

## ----example1-----------------------------------------------------------------
ex <- load_example("daystay")
ex$events

# Nights: 0 nights in hospital -> DAOH = 30/30 = 100%
calc_daoh(ex$events, ex$index_dates, period = 30, method = "nights")

# Days: 1 day in hospital -> DAOH = 29/30 = 96.7%
calc_daoh(ex$events, ex$index_dates, period = 30, method = "days")

## ----example2-----------------------------------------------------------------
ex2 <- load_example("death")
ex2$events

# All seven variants
results <- expand.grid(
  method       = c("nights", "days", "exact"),
  death_method = c("midday", "midnight", "zero"),
  stringsAsFactors = FALSE
)

results$daoh <- mapply(function(m, dm) {
  calc_daoh(ex2$events, ex2$index_dates,
            period = 30, method = m, death_method = dm)$daoh
}, results$method, results$death_method)

results$daohPC <- round(100 * results$daoh / 30, 1)
print(results)

## ----example3_load------------------------------------------------------------
pop <- load_example("population")

## ----example3_compare---------------------------------------------------------
res_n <- calc_daoh(pop$events, pop$index_dates, period = 90, method = "nights")
res_d <- calc_daoh(pop$events, pop$index_dates, period = 90, method = "days")

# Summary statistics
cat("Nights: median DAOH% =", round(median(res_n$daohPC), 1), "\n")
cat("Days:   median DAOH% =", round(median(res_d$daohPC), 1), "\n")

# Difference ≈ number of episodes per patient
cat("Mean episodes (nights):", round(mean(res_n$n_episodes), 2), "\n")
cat("Mean difference in DAOH (nights - days):",
    round(mean(res_n$daoh - res_d$daoh), 3), "days\n")

## ----ba_plot------------------------------------------------------------------
ba <- bland_altman_daoh(res_n, res_d)
cat(sprintf("Mean difference: %.3f%%\n95%% LoA: %.3f%% to %.3f%%\n",
            ba$mean_diff, ba$loa_lower, ba$loa_upper))

plot_daoh_ba(ba, method_a = "Nights", method_b = "Days")

## ----reclassify---------------------------------------------------------------
rc <- daoh_reclassify(res_n, res_d, n_groups = 4)
cat(sprintf("%.1f%% of patients change quartile when switching nights → days\n",
            rc$pct_reclassified))
print(rc$confusion_matrix)
plot_daoh_reclassify(rc, method_a = "Nights", method_b = "Days")

## ----dist_plot----------------------------------------------------------------
plot_daoh_dist(res_n, title = "DAOH – Nights algorithm, 90-day period")

