sti <- function(cases, pop){
  # cases is a vector of daily cases, pop the population of the group
  sti <- stats::filter(cases, rep(1, 7), method="convolution", sides=1)
  sti[1:6] <- cases[1:6]
  return(sti / pop * 1e5)
}

#'Time series of COVID-19 cases and deaths
#'
#'\code{get_time_series_for()} is used to create a tibble of
#'COVID-19 cases and COVID-19 related deaths for a region and a certain
#'age group. The time period can be defined by the user.
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the summed cases/deaths of these regions.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'
#'@return A tibble that contains the number of cases and deaths for the desired regions during the defined period of time.
#'The default version without any arguments returns a tibble that contains all the cases/deaths of Germany between 2020-01-01 and today.
#'@examples get_time_series_for(ages=c(3,55),regions="Sachsen",from="2021-01-05",to="2021-02-06")
#'
#'get_time_series_for(ages=32,regions=c(8221,8222))
#'
#'\dontrun{get_time_series_for(ages=c(12,42),from="2020-05-02",to="2020-05-01"}
#'##"from" must always be an earlier date than "to"
#'@export
get_time_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))
  data <- filter_data_by(ages, regions, from, to)

  # create time series
  days_series <- seq(as.Date(from), as.Date(to), by="days")
  dplyr::tibble(date=days_series) %>%
    dplyr::left_join(data, by=c("date"="Meldedatum"))  %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    # the days for which we have no infection data for are days with 0 infections
    dplyr::mutate(cases=tidyr::replace_na(cases, 0), deaths=tidyr::replace_na(deaths, 0)) -> time_series

  return(time_series)
}

#'Time series of 7-day-incidence
#'
#'\code{get_sti_series_for()} is used to create a tibble of
#'the 7-day-incidence of a region and a certain
#'age group. The time period can be defined by the user.
#'
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param regions A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
#'If this vector has more than one entry, the tibble contains the 7-day-incidence of these regions together.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'@param return_deaths A boolean. If one sets \code{return_deaths=T}, another column is added to the tibble that contains the 7-day-death-average weighted by
#'the population. (Basically, this is the something like a 7-day-death-incidence.)
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the incidence will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A tibble that contains the 7-day-incidence (or additionally, the 7-day-death-average) for the desired regions during the defined period of time.
#'The default version without any arguments returns a tibble that contains the 7-day-incidence of Germany between 2020-01-01 and today.
#'@examples get_sti_series_for(ages=c(3,55),regions="Sachsen",from="2021-01-05",to="2021-02-06")
#'
#'get_sti_series_for(ages=32,regions=c(8221,8222))
#'
#'\dontrun{get_time_series_for(ages=c(12,42),from="2020-05-02",to="2020-05-01"}
#'#"from" must always be an earlier date than "to"
#' @export
get_sti_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){
  # careful! when specifying region *and* agegroup, the incidence will not be accurate because
  # there is no population data for the age groups in each Landkreis and it will be estimated
  # by the age distribution in all of Germany
  # therefore, it is recommended to specify only one or the other
  ids <- is.numeric(regions)

  stopifnot("invalid age"=(ages=="all" || suppressWarnings(!is.na(as.numeric(ages)))))
  stopifnot("from must be before to"=as.Date(from)<as.Date(to))

  # calculate the population of the specified group
  rev.env$population_lk_data %>% `[[`("Bevölkerung") %>% sum() -> total_pop
  spec_pop_percentage <- 1

  if(!all(ages=="all")){
    age_labels <- rep("", length(ages))
    for(i in 1:length(ages)){
      age_labels[i] <- get_age_label_from_number(ages[i])
    }
    rev.env$population_age_data %>%
      dplyr::filter(Altersgruppe %in% age_labels) %>% `[[`("Bevölkerung") %>% sum() -> spec_pop
    spec_pop_percentage <- spec_pop / total_pop
  }

  time_series <- get_time_series_for(ages, regions, from, to)
  days_series <- seq(as.Date(from), as.Date(to), by="days")

  # filter the regions (not robust at the moment)
  rki_data %>% dplyr::select(Bundesland) %>% unique() %>%
    dplyr::filter(!Bundesland %in% c("Berlin", "Bremen", "Hamburg")) %>%
    `[[`("Bundesland") %>% tolower() -> bundesländer

  if(all(tolower(regions)=="germany")){
    final_pop <- spec_pop_percentage * total_pop
  }
  else if(all(tolower(regions) %in% bundesländer)){
    rev.env$population_lk_data %>%
      dplyr::filter(tolower(Bundesland) %in% tolower(regions)) %>%
      `[[`("Bevölkerung") %>% sum() ->
      region_pop
    final_pop <- region_pop * spec_pop_percentage
  }
  else{
    if(!ids){
      for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions[i])
    }
    rev.env$population_lk_data %>% dplyr::filter(IdLandkreis %in% regions) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
    final_pop <- region_pop * spec_pop_percentage
  }

  if(return_deaths){
    sti_series <- sti(time_series[["deaths"]], final_pop)
  }
  else{
    sti_series <- sti(time_series[["cases"]], final_pop)
  }

  return(dplyr::tibble(date=days_series, sti=sti_series))
}

# A faster sti function, it is however less adaptable
sti_id <- function(id){
    # Getting population
    pop <- as.integer(dplyr::filter(rev.env$population_lk_data,IdLandkreis==id)[5])

    #Getting each case
    rki_id <- rki_data[which(rki_data$IdLandkreis == id),]

    dplyr::left_join(tibble::tibble(date=rev.env$days_since_2020),rki_id, by=c("date"="Meldedatum")) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
      # the days we have no infection data for are days with 0 infections
        dplyr::mutate(cases=tidyr::replace_na(cases, 0), deaths=tidyr::replace_na(deaths, 0),
        recoveries=tidyr::replace_na(recoveries, 0)) ->
            id_time_series


    # Calculating the rolling average
    n <- length(id_time_series$date)
    sti <- rep(0, n)
    for(i in 1:n){
        for(j in max(1, i-6):i) sti[i] <- sti[i] + id_time_series$cases[j]
    }
    sti <- sti / pop * 1e5

    return(tibble::tibble(id_time_series$date,sti, .name_repair = ~ c("date", "sti")))
}

#Eine weitere "schnelle sti"
get_sti_series_simple <- function(lk_id){
  rev.env$population_lk_data %>% dplyr::filter(IdLandkreis %in% lk_id) %>%
    unique() -> data
  data %>% `[[`("Landkreis") -> lk_name
  data %>% `[[`("Bevölkerung") -> population

  ts <- get_time_series_for(regions=lk_name)
  cases_ts <- ts[["cases"]]

  sti(cases_ts, population) %>% return()
}

#'Creating Time Series of Cases, Deaths or Incidence
#'
#'\code{get_data_for} is used to create a table of the absolute number of cases, of deaths or of
#'the 7-day-incidence of a region and a certain
#'age group. The time period can be defined by the user.
#'
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's, or just "Germany" as a whole
#'If this vector has more than one entry, the tibble contains the values of these regions separately.
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'@param type A string specifying which time series is returned. Can be "sti" for the incidence or just "cases" / "deaths"
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the incidence will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A tibble that contains the wanted value for the desired regions during the defined period of time.
#'@examples get_data_for(ages=c(3,55),regions="Sachsen",from="2021-01-05",to="2021-02-06")
#'
#'get_data_for(ages=32,regions=c(8221, "Hessen", "Germany"), type="sti")
#'
#'\dontrun{get_time_series_for(ages=c(12,42),from="2020-05-02",to="2020-05-01"}
#'#"from" must always be an earlier date than "to"
#' @export
get_data_for <- function(regions, ages="all", from="2020-01-01", to=Sys.Date(), type="cases"){
  # type can be "cases", "sti", "deaths"
  stopifnot("invalid type!"=type %in% c("cases", "sti", "deaths"))

  regions_user <- regions

  rki_data %>% dplyr::select(Bundesland) %>% unique() %>%
    dplyr::filter(!Bundesland %in% c("Bremen", "Hamburg")) %>%
    `[[`("Bundesland") %>% tolower() -> bundesländer

  for(i in 1:length(regions)){
    if(tolower(regions[i]) %in% bundesländer){
      rev.env$population_lk_data %>%
        dplyr::filter(tolower(Bundesland) == tolower(regions[i])) %>%
        dplyr::select(Bundesland) %>%
        unique() %>%
        `[[`(1) ->
        regions[i]
    }
    else if(tolower(regions[i])=="germany"){
      regions[i] <- "Germany"
    }
    else{
      regions[i] <- get_lk_id_from_string(regions[i])
    }
  }

  regions_labels <- tibble::tibble(region_name=regions_user, region=regions)

  data <- tibble::tibble(date=character(), value=numeric(), region=character())

  for(reg in regions){
    if(type=="sti"){
      get_sti_series_for(regions=reg, ages=ages, from=from, to=to) %>%
        dplyr::mutate(date=as.character(date), region=reg, value=sti) %>%
        dplyr::select(-sti) %>%
        tibble::add_row(data, .) ->
        data
    }
    else if(type=="cases"){
      get_time_series_for(regions=reg, ages=ages, from=from, to=to) %>%
        dplyr::mutate(date=as.character(date), region=reg, value=cases) %>%
        dplyr::select(-cases, -deaths) %>%
        tibble::add_row(data, .) ->
        data
    }
    else if(type=="deaths"){
      get_time_series_for(regions=reg, ages=ages, from=from, to=to) %>%
        dplyr::mutate(date=as.character(date), region=reg, value=deaths) %>%
        dplyr::select(-cases, -deaths) %>%
        tibble::add_row(data, .) ->
        data
    }
  }

  data %>%
    dplyr::left_join(regions_labels, by="region") %>%
    dplyr::select(-region) %>%
    dplyr::mutate(date=as.Date(date)) %>%
    return()
}

#'Plotting a Time Series of Cases, Deaths or Incidence
#'
#'\code{plot_data_for} is used to plot the absolute number of cases, of deaths or of
#'the 7-day-incidence of a region and a certain
#'age group. The time period can be defined by the user.
#'The function will plot the returns of \code{get_data_for}
#'
#'@param smoothing A positive integer that defines the window size of the moving average. Thus, the plot will be smoother
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's, or just "Germany" as a whole
#'If this vector has more than one entry, the tibble contains the values of these regions separately.
#'@param ages A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
#'The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.
#'@param from A date that specifies the beginning of the time series
#'@param to A date that specifies the end of the time series
#'@param type A string specifying which time series is returned. Can be "sti" for the incidence or just "cases" / "deaths"
#'
#'@section Warning:
#'When specifying region \strong{and} agegroup, the incidence will not be accurate because
#'there is no population data for the age groups in each district and it will be estimated
#'by the age distribution of Germany.
#'Therefore, it is recommended to specify only one or the other
#'
#'@return A plot that displays the wanted value for the desired regions during the defined period of time.
#'@examples plot_data_for(ages=c(3,55),regions="Sachsen",from="2021-01-05",to="2021-02-06")
#'
#'plot_data_for(ages=32,regions=c(8221, "Hessen", "Germany"), type="sti")
#'
#'\dontrun{plot_time_series_for(ages=c(12,42),from="2020-05-02",to="2020-05-01"}
#'#"from" must always be an earlier date than "to"
#' @export
plot_data_for <- function(regions, ages="all", from="2020-01-01", to=Sys.Date(), type="cases", smoothing=0){
  smoothing <- as.integer(smoothing)
  stopifnot("invalid smoothing"=smoothing>=0)
  data <- get_data_for(regions, ages, from, to, type)

  data %>%
    dplyr::group_by(region_name) %>%
    dplyr::mutate(value=slider::slide_dbl(value,mean,.before=smoothing,.after=smoothing)) %>%
    dplyr::ungroup()->data

  data %>% dplyr::mutate(date=as.Date(date)) %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=value, color=region_name, group=region_name)) %>%
    `+`(ggplot2::geom_line()) %>% return()
}


#' Plotting a Comparison of Age Groups over the Pandemic
#'
#'\code{plot_for_agegroups} is used to generate a plot that can compare the case/death numbers
#'and incidence age groups. When comparing the cases or deaths, it returns a plot
#'which shows the share of cases/deaths for each age group. When plotting cases or deaths,
#' the plot will get smoothened automatically by the ggstream library
#' @param type A string which can be "cases" or "deaths".
#'@examples plot_for_agegroups(type="deaths")
#' @export
plot_for_agegroups <- function(type="cases"){
  # type can be cases or deaths

  stopifnot(type %in% c("cases", "deaths"))

  days_since_2020 <- rev.env$days_since_2020
  rki_data %>% dplyr::select(Altersgruppe) %>% unique() %>% `[[`(1) -> Altersgruppe
  tidyr::crossing(Altersgruppe, days_since_2020) -> series1
  print(series1)
  options(dplyr.summarise.inform = FALSE)
  series1 %>%
    dplyr::mutate(date=days_since_2020) %>%
    dplyr::select(-days_since_2020) %>%
    dplyr::left_join(filter_data_by(), by=c("date"="Meldedatum", "Altersgruppe"))  %>%
    dplyr::group_by(date, Altersgruppe) %>%
    dplyr::summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    # the days for which we have no infection data for are days with 0 infections
    dplyr::mutate(cases=tidyr::replace_na(cases, 0), deaths=tidyr::replace_na(deaths, 0)) -> data

  if(type=="cases"){
    data %>% ggplot2::ggplot(ggplot2::aes(x=date, y=cases, fill=Altersgruppe)) %>%
    `+`(ggstream::geom_stream(type="ridge")) %>% print()
  }
  if(type=="deaths"){
    data %>% ggplot(ggplot2::aes(x=date, y=deaths, fill=Altersgruppe)) %>%
    `+`(ggstream::geom_stream(type="ridge")) %>% print()
  }
}
