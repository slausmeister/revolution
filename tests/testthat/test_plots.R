context("plots testen")

test_that("death plots result is ggplot-object", {
  expect_s3_class(plot_abs_deaths(), c("ggplot"))
  expect_s3_class(plot_abs_deaths(c(2001,2003,2006,2016),smoothing=2), c("ggplot"))
  expect_s3_class(plot_abs_deaths(c(2006,2016)), c("ggplot"))

  expect_s3_class(plot_weekly_deaths(c(2015,2016)), c("ggplot"))
  expect_s3_class(plot_weekly_deaths(c(2014,2015,2016), age="A35-A59",rate=T), c("ggplot"))
  expect_s3_class(plot_weekly_deaths(c(2014,2015,2016), age="A80+"), c("ggplot"))

  expect_s3_class(plot_total_mortality(), c("ggplot"))
  expect_s3_class(plot_total_mortality(age="A35-A59"), c("ggplot"))

  expect_s3_class(plot_excess_mortality(smoothing=1), c("ggplot"))
  expect_s3_class(plot_excess_mortality(excess_year=2018, average_years=c(2016,2019), smoothing=2), c("ggplot"))
  expect_s3_class(plot_excess_mortality(excess_year=2018, average_years=c(2018,2019), smoothing=15), c("ggplot"))
})
