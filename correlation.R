source("incidence_lk.R")

# cases <- cases_time_series_germany[["cases"]]
# deaths <- cases_time_series_germany[["deaths"]]
# recoveries <- cases_time_series_germany[["recoveries"]]

# acf(cases, lag.max=length(cases))
# acf(deaths, lag.max=length(deaths))
# acf(recoveries, lag.max=length(recoveries))

# ccf(deaths, cases, lag.max=length(cases))


avg_correlation <- function(ts1, ts2, window=20){
  return(ccf(ts1, ts2, lag.max=window, plot=F)$acf %>% abs() %>% mean())
}

calc_cor_pair <- function(lk1_name, lk2_name){
  return(avg_correlation(calc_sti_lk(lk1_name, print_name=F), calc_sti_lk(lk2_name, print_name=F)))
}

lk_name1 <- population_lk_data[["Landkreis"]]
lk_name2 <- lk_name1

crossing(lk_name1, lk_name2) %>% filter(lk_name1 < lk_name2) -> lk_pairs
avg_corrs <- rep(0, length(lk_name1))

no_of_pairs <- lk_pairs %>% count() %>% `[[`(1)

for(i in 1:no_of_pairs){
  lk_pairs %>% slice(i) %>% unlist(use.names=F) -> lks
  avg_corrs[i] <- calc_cor_pair(lks[1], lks[2])
  if(i %% 200 == 0){
    print(paste("Progress: ", 100*i/no_of_pairs, "%"))
  }
}

lk_pairs %>% mutate(correlation=avg_corrs) %>% arrange(desc(correlation)) -> lk_cor

# plot(normalize_sti(calc_sti_lk("Rheingau")), type="l")
# ccf(normalize_sti(calc_sti_lk("Potsdam")), normalize_sti(calc_sti_lk("Berlin")), lag.max=500)
