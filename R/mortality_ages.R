# gibt eine Zeitreihe zurück, an welchem Datum wie viele der infizierten gestorben
# sind, aufgeschlüsselt nach Altersgruppe
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

# plottet die obige Zeitreihe
#' @export
plot_covid_mortality <- function(){
  calc_mortality_ages() %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=mortality, color=Altersgruppe)) %>%
    `+`(ggplot2::geom_line()) %>% return()
}
