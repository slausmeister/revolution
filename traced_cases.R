source("sti.R")
library(ggplot2)
library(ggstream)

# USER FUNKTION
# returnt einen tibble in dem die fälle an jedem tag stehen und bei wie vielen davon
# das erkrankungsdatum bekannt ist (absolut und prozentual)
calc_traced_cases <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){

    data <- filter_data_by(ages=ages, regions, from, to)

    # infections time series
    tibble(date=days_since_2020) %>%
      left_join(data, by=c("date"="Meldedatum"))  %>%
      group_by(date) %>%
      summarise(cases=sum(AnzahlFall),
        traced_percentage=sum(IstErkrankungsbeginn*AnzahlFall)/sum(AnzahlFall),
        traced_total=sum(IstErkrankungsbeginn*AnzahlFall)) %>%
      # the days for which we have no infection data for are days with 0 infections
      mutate(cases=replace_na(cases, 0), traced_percentage=replace_na(traced_percentage, 1),
        traced_total=replace_na(traced_total, 0)) %>%
      # smoothen the data a bit
      mutate(traced_percentage=stats::filter(traced_percentage, rep(1/10, 10), sides=1)) %>%
      mutate(traced_percentage=replace_na(traced_percentage, 1)) %>%
      return()
  }

# USER FUNKTION
# plottet die sti über die plandemie und darunter zu welchem zeitpunkt wie viele
# dieser fälle ein bekanntes erkrankungsdatum haben (prozentual)
plot_traced_percentage <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
    data <- calc_traced_cases(ages, regions, from, to)
    plt <- ggplot(data, aes(x=date, y=traced_percentage)) + geom_line()

    ggplot(data=get_sti_series_for(), aes(x=date, y=sti)) +
      geom_line() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      return()
}

# USER FUNKTION
# plots the proportion of traced/untraced cases in total over the pandemic
# next to the sti
plot_traced_total <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
    data <- calc_traced_cases(ages, regions, from, to)
    data %>% mutate(untraced_total=cases-traced_total) %>% select(-cases, -traced_percentage) %>%
      pivot_longer(!date, names_to="traced", values_to="count") -> data

    ggplot(data, aes(x=date)) + geom_stream(aes(y=count, fill=traced), type="ridge") -> plt

    ggplot(data=get_sti_series_for(), aes(x=date, y=sti)) +
      geom_line() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      return()
  }

# USER FUNKTION
# berechnet die verteilung zwischen meldedatum und erkrankungsdatum (falls bekannt)
calc_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=Inf){
    # cut cuts the distribution at this value

    filter_data_by(ages=ages, regions, from, to) %>%
    filter(IstErkrankungsbeginn==1) %>%
    mutate(diff=as.numeric(Meldedatum - Refdatum)) %>%
    group_by(diff) %>%
    count() %>%
    # we cut every data which has a diff greater than cut because that is nonsensical and
    # distorts the distribution
    filter(abs(diff)<cut) %>%
    ungroup() %>% return()
  }

# USER FUNKTION
# plottet die obige verteilung
plot_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=30){
    calc_distribution_report_diff(cut=cut) %>%
      ggplot(aes(x=diff, y=n)) + geom_bar(stat="identity") %>% return()
  }
