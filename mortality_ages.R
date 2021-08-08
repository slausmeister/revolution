source("utilities.R",encoding="UTF-8")

# USER FUNKTION
# gibt eine Zeitreihe zurück, an welchem Datum wie viele der infizierten gestorben
# sind, aufgeschlüsselt nach Altersgruppe
calc_mortality_ages <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))
  data <- filter_data_by(ages, regions, from, to)

  # get all the age labels
  data %>% select(Altersgruppe) %>% unique()%>%
    filter(Altersgruppe != "unbekannt") -> age_labels

  # create time series
  days_series <- seq(as.Date(from), as.Date(to), by="days")

  # create the combination of date and age groups
  crossing(days_series, age_labels) -> date_and_ages

  # calculate mortality per age group and date
  data %>% group_by(Meldedatum, Altersgruppe) %>%
    summarise(AnzahlTodesfall=sum(AnzahlTodesfall), AnzahlFall=sum(AnzahlFall)) %>%
    mutate(mortality=AnzahlTodesfall/AnzahlFall) %>%
    select(-AnzahlTodesfall, -AnzahlFall) -> data

  # create a time series for all days in the time span and return it
  date_and_ages %>% left_join(data, by=c("days_series"="Meldedatum", "Altersgruppe")) %>%
    mutate(date=days_series, mortality=replace_na(mortality, 0)) %>%
    select(-days_series) %>% return()
}

# USER funktion
# plottet die obige Zeitreihe
plot_mortality_ages <- function(){
  calc_mortality_ages() %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=mortality, color=Altersgruppe)) %>%
    `+`(ggplot2::geom_line()) %>% return()
}
