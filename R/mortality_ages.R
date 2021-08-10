# gibt eine Zeitreihe zurück, an welchem Datum wie viele der infizierten gestorben
# sind, aufgeschlüsselt nach Altersgruppe


#'Time series of COVID-19 associated mortality
#'
#'\code{calc_covid_mortality()} is used to create a tibble of the COVID-19 mortality per age group and user-definded 
#'time period.
#'
#'@param ages A vector of integer numbers that specify the desired age groups.
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's.
#'If this vector contains more than entry, the function calculates the combined mortality of these districts or states.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'
#'@return A tibble that contains the COVID-19 associated mortality per age group and time period.
#'
#'@section Warning:
#'These numbers must be handled carefully because of the unknown dark figure.
#'
#'@examples calc_covid_mortality(ages=c(10,76,42),regions=c("Bayern","Sachsen"))
#'
#'calc_covid_mortality(ages="all",regions=8221,from="2020-02-03",to="2021-02-03")
#'
#' @export
calc_covid_mortality <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))
  data <- filter_data_by(ages, regions, from, to)

  # get all the age labels
  data %>% dplyr::select(Altersgruppe) %>% unique()%>%
    dplyr::filter(Altersgruppe != "unbekannt") -> age_labels

  # create time series
  days_series <- seq(as.Date(from), as.Date(to), by="days")

  # create the combination of date and age groups
  tidyr::crossing(days_series, age_labels) -> date_and_ages

  # calculate mortality per age group and date
  data %>% dplyr::group_by(Meldedatum, Altersgruppe) %>%
    dplyr::summarise(AnzahlTodesfall=sum(AnzahlTodesfall), AnzahlFall=sum(AnzahlFall)) %>%
    dplyr::mutate(mortality=AnzahlTodesfall/AnzahlFall) %>%
    dplyr::select(-AnzahlTodesfall, -AnzahlFall) -> data

  # create a time series for all days in the time span and return it
  date_and_ages %>% dplyr::left_join(data, by=c("days_series"="Meldedatum", "Altersgruppe")) %>%
    dplyr::mutate(date=days_series, mortality=tidyr::replace_na(mortality, 0)) %>%
    dplyr::select(-days_series) %>% return()
}
#'Plotting the COVID-19 associated mortality
#'
#'\code{plot_covid_mortality()} is used to create a plot of the COVID-19 mortality per age group and user-definded 
#'time period.
#'
#'@param ages A vector of integer numbers that specify the desired age groups.
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's.
#'If this vector contains more than entry, the function plots the combined mortality of these districts or states.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'@param smoothing A positive integer that defines the window size of the moving average. Thus, the plot will be smoother
#'the higher 'smoothing' is chosen. The default setting is 'no smoothing'.
#'
#'@return A plot of the COVID-19 associated mortality per age group and time period.
#'
#'@section Warning:
#'These numbers must be handled carefully because of the unknown dark figure.
#'
#'@examples plot_covid_mortality(ages=c(10,76,42),regions=c("Bayern","Sachsen"))
#'
#'plot_covid_mortality(ages="all",regions=8221,from="2020-02-03",to="2021-02-03")
#' @export
plot_covid_mortality <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date(),smoothing=0){
  calc_covid_mortality(ages, regions, from, to) %>%
    dplyr::group_by(Altersgruppe) %>% 
    dplyr::mutate(mortality=slider::slide_dbl(mortality,mean,.before=smoothing,.after=smoothing)) %>% 
    ggplot2::ggplot(ggplot2::aes(x=date, y=mortality, color=Altersgruppe)) %>%
    `+`(ggplot2::geom_line()) %>% return()
}
