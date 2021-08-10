# returnt einen tibble in dem die fälle an jedem tag stehen und bei wie vielen davon
# das erkrankungsdatum bekannt ist (absolut und prozentual)
#' @export
calc_traced_cases <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){

    data <- filter_data_by(ages, regions, from, to)

    # infections time series
    tibble::tibble(date=rev.env$days_since_2020) %>%
      dplyr::left_join(data, by=c("date"="Meldedatum"))  %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(cases=sum(AnzahlFall),
        traced_percentage=sum(IstErkrankungsbeginn*AnzahlFall)/sum(AnzahlFall),
        traced_total=sum(IstErkrankungsbeginn*AnzahlFall)) %>%
      # the days for which we have no infection data for are days with 0 infections
      dplyr::mutate(cases=tidyr::replace_na(cases, 0), traced_percentage=tidyr::replace_na(traced_percentage, 1),
        traced_total=tidyr::replace_na(traced_total, 0)) %>%
      # smoothen the data a bit
      dplyr::mutate(traced_percentage=stats::filter(traced_percentage, rep(1/10, 10), sides=1)) %>%
      dplyr::mutate(traced_percentage=tidyr::replace_na(traced_percentage, 1)) %>%
      return()
  }

# plottet die sti über die plandemie und darunter zu welchem zeitpunkt wie viele
# dieser fälle ein bekanntes erkrankungsdatum haben (prozentual)
#' @export
plot_traced_cases_percentage <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
    #TODO: remove "Don't know how to automatically pick scale for object of type ts" warning
    data <- calc_traced_cases(ages, regions, from, to)
    plt <- ggplot2::ggplot(data, ggplot2::aes(x=date, y=traced_percentage)) +
      ggplot2::geom_path()

    ggplot2::ggplot(data=get_sti_series_for(), ggplot2::aes(x=date, y=sti)) +
      ggplot2::geom_path() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      return()
}

# plots the proportion of traced/untraced cases in total over the pandemic
# next to the sti
#' @export
plot_traced_cases_total <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
    #TODO: remove "Don't know how to automatically pick scale for object of type ts" warning
    data <- calc_traced_cases(ages, regions, from, to)
    data %>% dplyr::mutate(untraced_total=cases-traced_total) %>%
      dplyr::select(-cases, -traced_percentage) %>%
      tidyr::pivot_longer(!date, names_to="traced", values_to="count") -> data

    ggplot2::ggplot(data, ggplot2::aes(x=date)) +
      ggstream::geom_stream(ggplot2::aes(y=count, fill=traced), type="ridge") -> plt

    ggplot2::ggplot(data=get_sti_series_for(), ggplot2::aes(x=date, y=sti)) +
      ggplot2::geom_line() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      return()
  }

# berechnet die verteilung zwischen meldedatum und erkrankungsdatum (falls bekannt)
#' @export
calc_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=Inf){
    # cut cuts the distribution at this value
    filter_data_by(ages, regions, from, to) %>%
      dplyr::filter(IstErkrankungsbeginn==1) %>%
      dplyr::mutate(diff=as.numeric(Meldedatum - Refdatum)) %>%
      dplyr::group_by(diff) %>%
      dplyr::count() %>%
      # we cut every data which has a diff greater than cut because that is nonsensical and
      # distorts the distribution
      dplyr::filter(abs(diff)<cut) %>%
      dplyr::ungroup() %>%
      return()
  }

# plottet die obige verteilung
#' @export
plot_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=30){
    calc_distribution_report_diff(ages, regions, from, to, cut=cut) %>%
      ggplot2::ggplot(ggplot2::aes(x=diff, y=n)) %>%
      `+`(ggplot2::geom_bar(stat="identity")) %>%
      return()
  }
