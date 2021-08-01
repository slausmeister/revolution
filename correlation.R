source("sti.R")

# cases <- cases_time_series_germany[["cases"]]
# deaths <- cases_time_series_germany[["deaths"]]
# recoveries <- cases_time_series_germany[["recoveries"]]

# acf(cases, lag.max=length(cases))
# acf(deaths, lag.max=length(deaths))
# acf(recoveries, lag.max=length(recoveries))

# ccf(deaths, cases, lag.max=length(cases))


avg_correlation <- function(ts1, ts2, window=0){
  return(ccf(ts1, ts2, lag.max=window, plot=F)$acf %>% abs() %>% mean())
}

calc_cor_pair <- function(lk_id1, lk_id2){
  return(avg_correlation(get_sti_series_by_id(lk_id1)[["sti"]],
    get_sti_series_by_id(lk_id2)[["sti"]]))
}

lk_ids1 <- population_lk_data[["IdLandkreis"]]
lk_ids2 <- lk_ids1

crossing(lk_ids1, lk_ids2) %>% filter(lk_ids1 < lk_ids2) -> lk_pairs
avg_corrs <- rep(0, length(lk_ids1))

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
