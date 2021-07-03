# source("incidence_lk.R")

# cases <- cases_time_series_germany[["cases"]]
# deaths <- cases_time_series_germany[["deaths"]]
# recoveries <- cases_time_series_germany[["recoveries"]]

# acf(cases, lag.max=length(cases))
# acf(deaths, lag.max=length(deaths))
# acf(recoveries, lag.max=length(recoveries))

# ccf(deaths, cases, lag.max=length(cases))


normalize_sti <- function(sti){
  norm <- sti / calc_sti_germany()
  norm[is.nan(norm)] <- 0
  return(norm)
}
# plot(normalize_sti(calc_sti_lk("Rheingau")), type="l")
ccf(normalize_sti(calc_sti_lk("Potsdam")), normalize_sti(calc_sti_lk("Berlin")), lag.max=500)
