source("utilities.R")

calc_traced_cases <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){

    data <- filter_data_by(ages=ages, regions, from, to)

    # infections time series
    tibble(date=days_since_2020) %>%
      left_join(data, by=c("date"="Meldedatum"))  %>%
      group_by(date) %>%
      summarise(cases=sum(AnzahlFall),
        traced_percentage=1-sum(IstErkrankungsbeginn*AnzahlFall)/sum(AnzahlFall),
        traced_total=sum(IstErkrankungsbeginn*AnzahlFall)) %>%
      # the days for which we have no infection data for are days with 0 infections
      mutate(cases=replace_na(cases, 0), traced_percentage=replace_na(traced_percentage, 0),
        traced_total=replace_na(traced_total, 0)) %>%
      # smoothen the data a bit
      mutate(traced_percentage=stats::filter(traced_percentage, rep(1/10, 10), sides=1)) %>%
      return()
  }

calc_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
    
    filter_data_by(ages=ages, regions, from, to) %>%
    filter(IstErkrankungsbeginn==1) %>%
    mutate(diff=as.numeric(Meldedatum - Refdatum)) %>%
    group_by(diff) %>%
    count() %>%
    filter(abs(diff)<30) %>%
    ungroup() %>% return()
  }
