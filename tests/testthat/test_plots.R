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

test_that("healthcare plots result is ggplot-object", {
  expect_s3_class(plot_traced_cases_percentage(), c("ggplot"))
  expect_s3_class(plot_traced_cases_percentage(ages=c(1,20,100), regions=c(8221, 1001, "Bremen")), c("ggplot"))
  expect_s3_class(plot_traced_cases_percentage(ages=c(-1,20,31231), regions=c("Bayern")), c("ggplot"))
  expect_s3_class(plot_traced_cases_percentage(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("ggplot"))

  expect_s3_class(plot_traced_cases_total(), c("ggplot"))
  expect_s3_class(plot_traced_cases_total(ages=c(1,20,100), regions=c(8221, 1001, "Bremen")), c("ggplot"))
  expect_s3_class(plot_traced_cases_total(ages=c(-1,20,31231), regions=c("Bayern")), c("ggplot"))
  expect_s3_class(plot_traced_cases_total(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("ggplot"))

  expect_s3_class(plot_distribution_report_diff(), c("ggplot"))
  expect_s3_class(plot_distribution_report_diff(ages=c(1,20,100), regions=c(8221, 1001, "Bremen"), cut=49), c("ggplot"))
  expect_s3_class(plot_distribution_report_diff(ages=c(-1,20,31231), regions=c("Bayern")), c("ggplot"))
  expect_s3_class(plot_distribution_report_diff(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("ggplot"))
})

test_that("covid mortality plots result is ggplot-object", {
  expect_s3_class(plot_covid_mortality(), c("ggplot"))
  expect_s3_class(plot_covid_mortality(ages=c(1,20,100), regions=c(8221, 1001, "Bremen"), smoothing=49), c("ggplot"))
  expect_s3_class(plot_covid_mortality(ages=c(-1,20,31231), regions=c("Bayern")), c("ggplot"))
  expect_s3_class(plot_covid_mortality(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05"), c("ggplot"))
})

test_that("pop_density plots are ggplot", {
  expect_s3_class(plot_pop_density_with_linear_model(regions=c(8221, 1001)), c("ggplot"))
  expect_s3_class(plot_pop_density_with_linear_model(regions=c("Heidelberg", 1001)), c("ggplot"))
})

test_that("sti plots are ggplot", {
  expect_s3_class(plot_data_for(regions=c(8221, 1001, "Bremen")), c("ggplot"))
  expect_s3_class(plot_data_for(ages=c(1,20,100), regions=c(8221, 1001, "Bremen"), smoothing=49, type="sti"), c("ggplot"))
  expect_s3_class(plot_data_for(ages=c(-1,20,31231), regions=c("Bayern"), type="cases"), c("ggplot"))
  expect_s3_class(plot_data_for(ages=c(-1,20,31231), regions=c("Bayern"), from="2020-09-09", to="2021-04-05", type="deaths"), c("ggplot"))

  expect_s3_class(plot_for_agegroups(), c("ggplot"))
  expect_s3_class(plot_for_agegroups(type="deaths"), c("ggplot"))
  expect_s3_class(plot_for_agegroups(type="sti"), c("ggplot"))
})

test_that("traffic plots are ggplot", {
  expect_s3_class(plot_accidents_with_sti(), c("ggplot"))
  expect_s3_class(plot_public_transportation_with_sti(), c("ggplot"))
})

test_that("vaccination plots are ggplot", {
  expect_s3_class(plot_vaccination_data(), c("ggplot"))
  expect_s3_class(plot_vaccination_data(regions=c(8221, 1001, "Bremen")), c("ggplot"))
  expect_s3_class(plot_vaccination_data(ages="12-17", regions=c(8221, 1001, "Bremen"), smoothing=49, cumulate=T), c("ggplot"))
  expect_s3_class(plot_vaccination_data(ages="18-59", regions=c("Bayern"), vac_num=1), c("ggplot"))
  expect_s3_class(plot_vaccination_data(ages="60+", regions=c("Bayern"), from="2020-09-09", to="2021-04-05", vac_num=2), c("ggplot"))
})

test_that("weekly distr plots are ggplot", {
  expect_s3_class(office_case_distribution(), c("ggplot"))
  expect_s3_class(office_case_distribution("Hessen", "Sachsen"), c("ggplot"))
  expect_s3_class(office_case_distribution("Bremen", "Berlin"), c("ggplot"))
})

test_that("variant plots are ggplot", {
  expect_s3_class(plot_variants(type="r"), c("ggplot"))
  expect_s3_class(plot_variants(), c("ggplot"))
  expect_s3_class(plot_variants(type="share"), c("ggplot"))
  expect_s3_class(plot_variants(type="percentage"), c("ggplot"))
  expect_s3_class(plot_variants(type="sti"), c("ggplot"))
})
