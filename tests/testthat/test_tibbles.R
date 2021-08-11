test_that("death result is tbl-object", {
  expect_s3_class(get_abs_deaths(), c("tbl"))
  expect_s3_class(get_abs_deaths(c(2001,2003,2006,2016)), c("tbl"))
  expect_s3_class(get_abs_deaths(c(2006,2016)), c("tbl"))

  expect_s3_class(get_weekly_deaths(c(2015,2016)), c("tbl"))
  expect_s3_class(get_weekly_deaths(c(2014,2015,2016), age="A35-A59",rate=T), c("tbl"))
  expect_s3_class(get_weekly_deaths(c(2014,2015,2016), age="A80+"), c("tbl"))

  expect_s3_class(get_total_mortality(), c("tbl"))
  expect_s3_class(get_total_mortality(age="A35-A59"), c("tbl"))
})

test_that("healthcare result is tbl-object", {
  expect_s3_class(calc_traced_cases(), c("tbl"))
  expect_s3_class(calc_traced_cases(ages=c(1,20,100), regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(calc_traced_cases(ages=c(-1,20,31231), regions=c("Bayern")), c("tbl"))
  expect_s3_class(calc_traced_cases(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("tbl"))

  expect_s3_class(calc_distribution_report_diff(), c("tbl"))
  expect_s3_class(calc_distribution_report_diff(ages=c(1,20,100), regions=c(8221, 1001, "Bremen"), cut=49), c("tbl"))
  expect_s3_class(calc_distribution_report_diff(ages=c(-1,20,31231), regions=c("Bayern")), c("tbl"))
  expect_s3_class(calc_distribution_report_diff(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("tbl"))
})

test_that("covid mortality result is tbl-object", {
  expect_s3_class(calc_covid_mortality(), c("tbl"))
  expect_s3_class(calc_covid_mortality(ages=c(1,20,100), regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(calc_covid_mortality(ages=c(-1,20,31231), regions=c("Bayern")), c("tbl"))
  expect_s3_class(calc_covid_mortality(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("tbl"))
})

test_that("pop_density are tbl", {
  expect_s3_class(get_pop_density_with_sti(regions=c(8221, 1001)), c("tbl"))
  expect_s3_class(get_pop_density_with_sti(regions=c("Heidelberg", 1001)), c("tbl"))
})

test_that("sti results are tbl", {
  expect_s3_class(get_data_for(regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(get_data_for(ages=c(1,20,100), regions=c(8221, 1001, "Bremen"), type="sti"), c("tbl"))
  expect_s3_class(get_data_for(ages=c(-1,20,31231), regions=c("Bayern"), type="cases"), c("tbl"))
  expect_s3_class(get_data_for(ages=c(-1,20,31231), regions=c("Bayern", 8221), from="2020-09-09", to="2021-04-05", type="deaths"), c("tbl"))

  expect_s3_class(get_time_series_for(regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(get_time_series_for(ages=c(-1,20,31231), regions=c("Bayern", "Hessen"), from="2020-09-09", to="2021-04-05"), c("tbl"))

  expect_s3_class(get_sti_series_for(regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(get_sti_series_for(ages=c(-1,20,31231), regions=c("Bayern", "Hessen"), from="2020-09-09", to="2021-04-05", return_deaths=T), c("tbl"))
})

test_that("traffic results are tbl", {
  expect_s3_class(get_accidents_data(), c("tbl"))
  expect_s3_class(get_public_transportation_data(), c("tbl"))
})

test_that("vaccination results are tbl", {
  expect_s3_class(get_vaccination_data(), c("tbl"))
  expect_s3_class(get_vaccination_data(regions=c(8221, 1001, "Bremen")), c("tbl"))
  expect_s3_class(get_vaccination_data(ages="12-17", regions=c(8221, 1001, "Bremen"), cumulate=T), c("tbl"))
  expect_s3_class(get_vaccination_data(ages="18-59", regions=c("Bayern"), vac_num=1), c("tbl"))
  expect_s3_class(get_vaccination_data(ages="60+", regions=c("Bayern"), from="2020-09-09", to="2021-04-05", vac_num=2), c("tbl"))
})

test_that("variant results are tbl", {
  expect_s3_class(variant_case_time_series(), c("tbl"))
  expect_s3_class(variant_case_time_series("none", T), c("tbl"))
  expect_s3_class(variant_case_time_series("none"), c("tbl"))

  expect_s3_class(variant_case_r_value(), c("tbl"))
  expect_s3_class(variant_case_r_value(), c("tbl"))

  expect_s3_class(variant_sti_time_series(), c("tbl"))
  expect_s3_class(variant_sti_time_series("none", T), c("tbl"))
  expect_s3_class(variant_sti_time_series("none"), c("tbl"))
})
