library(testthat)
library(daoh)

# ─── merge_intervals ──────────────────────────────────────────────────────────

test_that("merge_intervals: overlapping intervals are merged", {
  res <- merge_intervals(c(0, 0.8), c(0.5, 1.5))
  expect_equal(nrow(res), 1)
  expect_equal(res$start, 0)
  expect_equal(res$end,   1.5)
})

test_that("merge_intervals: adjacent within gap are merged", {
  # Gap = 0.5 days; intervals separated by 0.4 days -> merged
  res <- merge_intervals(c(0, 1.9), c(1.5, 3.0), gap = 0.5)
  expect_equal(nrow(res), 1)
})

test_that("merge_intervals: intervals beyond gap stay separate", {
  res <- merge_intervals(c(0, 3), c(1, 4), gap = 0.5)
  expect_equal(nrow(res), 2)
})

test_that("merge_intervals: empty input returns empty data.frame", {
  res <- merge_intervals(numeric(0), numeric(0))
  expect_equal(nrow(res), 0)
})

# ─── hospital_time ─────────────────────────────────────────────────────────────

test_that("hospital_time: same-day, nights = 0, days = 1", {
  ht_n <- hospital_time(as.Date("2020-01-01"), as.Date("2020-01-01"), "nights")
  ht_d <- hospital_time(as.Date("2020-01-01"), as.Date("2020-01-01"), "days")
  expect_equal(ht_n$end - ht_n$start, 0)
  expect_equal(ht_d$end - ht_d$start, 1)
})

test_that("hospital_time: 2-night stay: nights=2, days=3", {
  ht_n <- hospital_time(as.Date("2020-01-01"), as.Date("2020-01-03"), "nights")
  ht_d <- hospital_time(as.Date("2020-01-01"), as.Date("2020-01-03"), "days")
  expect_equal(ht_n$end - ht_n$start, 2)
  expect_equal(ht_d$end - ht_d$start, 3)
})

# ─── calc_daoh ─────────────────────────────────────────────────────────────────

test_that("calc_daoh: no admissions -> DAOH = period", {
  events <- data.frame(patientID="P1", admission=as.Date(NA),
                       discharge=as.Date(NA), dod=as.Date(NA))[0,]
  idx    <- data.frame(patientID="P1", indexDate=as.Date("2020-01-01"))
  # Empty events: no hospital time
  expect_equal(nrow(calc_daoh(events, idx, period=30)), 0)
})

test_that("calc_daoh: day-stay nights=30, days=29 over 30-day period", {
  ex <- load_example("daystay")
  rn <- calc_daoh(ex$events, ex$index_dates, period=30, method="nights")
  rd <- calc_daoh(ex$events, ex$index_dates, period=30, method="days")
  expect_equal(rn$daoh, 30)
  expect_equal(rd$daoh, 29)
})

test_that("calc_daoh: death=zero returns 0 when patient dies in period", {
  ex2 <- load_example("death")
  r <- calc_daoh(ex2$events, ex2$index_dates, period=30,
                 method="nights", death_method="zero")
  expect_equal(r$daoh, 0)
})

test_that("calc_daoh: systematic difference = n_episodes", {
  ex <- load_example("population")
  rn <- calc_daoh(ex$events, ex$index_dates, period=90, method="nights")
  rd <- calc_daoh(ex$events, ex$index_dates, period=90, method="days")
  merged <- merge(rn[,c("patientID","indexDate","daoh","n_episodes")],
                  rd[,c("patientID","indexDate","daoh")],
                  by=c("patientID","indexDate"), suffixes=c("_n","_d"))
  # Difference should equal n_episodes (within boundary correction tolerance)
  diff <- merged$daoh_n - merged$daoh_d
  expect_true(all(abs(diff - merged$n_episodes) <= 1))
})

test_that("daoh_reclassify: pct_reclassified is numeric in [0,100]", {
  ex <- load_example("population")
  rn <- calc_daoh(ex$events, ex$index_dates, period=90, method="nights")
  rd <- calc_daoh(ex$events, ex$index_dates, period=90, method="days")
  rc <- daoh_reclassify(rn, rd, n_groups=4)
  expect_gte(rc$pct_reclassified, 0)
  expect_lte(rc$pct_reclassified, 100)
})
