#' Time Series of Traced Cases over Pandemic
#'
#' \code{calc_traced_cases()} is used to create a time series for the pandemic
#' which shows the total and relative amount of cases traced by the German healthcare system.
#' Traced cases here means that the infection date of an infection is known,
#' as opposed to cases which had a positive test, but could not be traced back to the infection.
#' The parameters work exactly like the ones in \code{get_sti_series_for}
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the data of these regions together.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the data will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A tibble that contains total and relative amount of traced cases for each day since 2020
#'
#' @family traced cases
#' @export
calc_traced_cases <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){

    data <- filter_data_by(ages, regions, from, to)
    days_series <- seq(as.Date(from), as.Date(to), by="days")
    # infections time series
    tibble::tibble(date=days_series) %>%
      dplyr::left_join(data, by=c("date"="Meldedatum"))  %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(cases=sum(AnzahlFall),
        traced_total=sum(IstErkrankungsbeginn*AnzahlFall)) %>%
      # the days for which we have no infection data for are days with 0 infections
      dplyr::mutate(cases=tidyr::replace_na(cases, 0),
        traced_total=tidyr::replace_na(traced_total, 0)) %>%
      dplyr::mutate(traced_percentage=traced_total/cases) %>%
      dplyr::mutate(traced_percentage=tidyr::replace_na(traced_percentage, 1)) %>%
      return()
  }


#' Plot Time Series of Relative Traced Cases over Pandemic
#'
#'Plots the data created by \code{calc_traced_cases}, next to the STI of the regions for comparison.
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the data of these regions together.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the data will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A plot that displays the desired data next to the STI for comparison
#'
#' @family traced cases
#' @export
plot_traced_cases_percentage <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), smoothing=0){
    #TODO: remove "Don't know how to automatically pick scale for object of type ts" warning
    calc_traced_cases(ages, regions, from, to) %>%
      dplyr::mutate(traced_percentage=slider::slide_dbl(traced_percentage,mean,.before=smoothing,.after=smoothing)) ->
      data

    plt <- ggplot2::ggplot(data, ggplot2::aes(x=date, y=traced_percentage)) +
      ggplot2::geom_path()

    ggplot2::ggplot(data=get_sti_series_for(ages, regions, from, to), ggplot2::aes(x=date, y=sti)) +
      ggplot2::geom_path() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      suppressMessages() %>%
      return()
}

#' Plot Time Series of Total Traced Cases over Pandemic
#'
#'Plots the data created by \code{calc_traced_cases}, next to the STI of the regions for comparison.
#'The plot is a stream plot so that the proportions are still visible.
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the data of these regions together.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the data will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A plot that displays the desired data next to the STI for comparison
#'
#' @family traced cases
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

    ggplot2::ggplot(data=get_sti_series_for(ages, regions, from, to), ggplot2::aes(x=date, y=sti)) +
      ggplot2::geom_line() -> plt_germany

    cowplot::plot_grid(plotlist = list(plt_germany, plt), nrow=2) %>%
      suppressMessages() %>%
      return()
  }


#' Time Difference Between Infection and Report of Cases
#'
#' This function returns a tibble, which summarises the distribution of the
#' difference between a case being reported and a person being infected over the
#' whole pandemic. It only uses the cases for which the actual time of infection is know.
#'
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the data of these regions together.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'@param cut A integer that determines at which number the distribution is cut
#' off, because some of the results are nonsensical
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the data will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A plot that displays the distribution of differences between infection and report date
#'
#' @family time difference infection vs. report
#' @export
calc_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=Inf){
    # cut cuts the distribution at this value
    filter_data_by(ages, regions, from, to) %>%
      dplyr::filter(IstErkrankungsbeginn==1) %>%
      dplyr::mutate(diff=as.numeric(Meldedatum - Refdatum)) %>%
      dplyr::group_by(diff) %>%
      dplyr::summarise(n=sum(AnzahlFall)) %>%
      # we cut every data which has a diff greater than cut because that is nonsensical and
      # distorts the distribution
      dplyr::filter(abs(diff)<cut) %>%
      dplyr::ungroup() %>%
      return()
  }

#' Plot Time Difference Between Infection and Report of Cases
#'
#' Plots the data of \code{calc_distribution_report_diff}. See \code{\link{calc_distribution_report_diff}} for an explanation of the data
#' 
#' @family time difference infection vs. report
#' @export
plot_distribution_report_diff <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date(), cut=30){
    calc_distribution_report_diff(ages, regions, from, to, cut=cut) %>%
      ggplot2::ggplot(ggplot2::aes(x=diff, y=n)) %>%
      `+`(ggplot2::geom_bar(stat="identity")) %>%
      return()
  }
