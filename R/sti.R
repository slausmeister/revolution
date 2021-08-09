# calc sti for a certain time series with a given population
# calc sti for a certain time series with a given population
sti <- function(cases, pop){
  # cases is a vector of daily cases, pop the population of the group
  sti <- stats::filter(cases, rep(1, 7), method="convolution", sides=1)
  sti[1:6] <- cases[1:6]
  return(sti / pop * 1e5)
}

# returns a total time series of cases and deaths for a region/age group etc.
#' @export
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

#' @export
get_sti_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){
  # careful! when specifying region *and* agegroup, the incidence will not be accurate because
  # there is no population data for the age groups in each Landkreis and it will be estimated
  # by the age distribution in all of Germany
  # therefore, it is recommended to specify only one or the other

  ids <- is.numeric(regions)

  # calculate the population of the specified group
  rev.env$population_age_data %>% `[[`("Bevölkerung") %>% sum() -> total_pop
  spec_pop_percentage <- 1

  if(!all(ages=="all")){
    age_labels <- rep("", length(ages))
    for(i in 1:length(ages)){
      age_labels[i] <- get_age_label_from_number(ages[i])
    }
    population_age_data %>%
      dplyr::filter(Altersgruppe %in% age_labels) %>% `[[`("Bevölkerung") %>% sum() -> spec_pop
    spec_pop_percentage <- spec_pop / total_pop
  }

  time_series <- get_time_series_for(ages, regions, from, to)
  days_series <- days_series <- seq(as.Date(from), as.Date(to), by="days")

  # filter the regions (not robust at the moment)
  rki_data %>% dplyr::select(Bundesland) %>% unique() %>%
    dplyr::filter(!Bundesland %in% c("Berlin", "Bremen", "Hamburg")) %>%
    `[[`("Bundesland") %>% tolower() -> bundesländer

  if(all(tolower(regions)=="germany")) final_pop <- spec_pop_percentage * total_pop
  else if(all(tolower(regions) %in% bundesländer)){
    rev.env$population_age_data %>% dplyr::filter(tolower(Bundesland) %in% tolower(regions)) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
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

  if(return_deaths) sti_series <- sti(time_series[["deaths"]], final_pop)
  else sti_series <- sti(time_series[["cases"]], final_pop)

  return(dplyr::tibble(date=days_series, sti=sti_series))
}

# get a sti time series for a lk id
#' @export
get_sti_series_by_id <- function(lk_ids, ages="all", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){

    # get the lk names
    rev.env$population_lk_data %>% dplyr::filter(IdLandkreis %in% lk_ids) %>% dplyr::select(Landkreis) %>%
      unique() %>% `[[`("Landkreis") -> lk_names
    return(get_sti_series_for(ages=ages, regions=lk_names, from=from, to=to,
      return_deaths=return_deaths))
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

# plots sti for lks, takes several lks for comparison
#' @export
plot_for_lks <- function(lks, type="cases"){
  # type can be "cases", "sti", "deaths"
  stopifnot("invalid type!"=type %in% c("cases", "sti", "deaths"))
  ids <- is.numeric(lks)
  if(ids){
    rev.env$population_lk_data %>% dplyr::select(IdLandkreis) %>% unique() %>%
      `[[`(1) -> valid_ids

    stopifnot("invalid id!"=!all(ids %in% valid_ids))
  }

  lk_ids <- lks
  if(!ids){
    for(i in 1:length(lks)){
      lk_ids[i] <- get_lk_id_from_string(lks[i])
    }
  }

  data <- tibble::tibble(date=character(), value=numeric(), lk=character())

  if(type=="sti"){
    for(i in 1:length(lks)){
      get_sti_series_for(regions=lk_ids[i]) -> temp
      temp %>% dplyr::mutate(date=as.character(date), value=sti, lk=as.character(lks[i])) %>%
        dplyr::select(-sti) %>% tibble::add_row(data, .) -> data
    }
  }

  if(type=="cases"){
    for(i in 1:length(lks)){
      get_time_series_for(regions=lk_ids[i]) -> temp
      temp %>% dplyr::mutate(date=as.character(date), value=cases, lk=as.character(lks[i])) %>%
        dplyr::select(-cases, -deaths) %>% tibble::add_row(data, .) -> data
    }
  }

  if(type=="deaths"){
    for(i in 1:length(lks)){
      get_time_series_for(regions=lk_ids[i]) -> temp
      temp %>% dplyr::mutate(date=as.character(date), value=deaths, lk=as.character(lks[i])) %>%
        dplyr::select(-cases, -deaths) %>% tibble::add_row(data, .) -> data
    }
  }

  data %>% dplyr::mutate(date=as.Date(date)) %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=value, color=lk, group=lk)) %>%
    `+`(ggplot2::geom_line()) %>% return()
}

# schöner stream plot zur aufteilung der Altersgruppen
#' @export
plot_for_agegroups <- function(type="cases"){
  # type can be cases or deaths
  rki_data %>% dplyr::select(Altersgruppe) %>% unique() %>% `[[`(1) -> Altersgruppe
  tidyr::crossing(Altersgruppe, rev.env$days_since_2020) -> series1
  options(dplyr.summarise.inform = FALSE)
  series1 %>% dplyr::rename("date"="days_since_2020") %>%
    dplyr::left_join(filter_data_by(), by=c("date"="Meldedatum", "Altersgruppe"))  %>%
    dplyr::group_by(date, Altersgruppe) %>%
    dplyr::summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    # the days for which we have no infection data for are days with 0 infections
    dplyr::mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0)) -> data

  if(type=="cases"){
    data %>% ggplot2::ggplot(ggplot2::aes(x=date, y=cases, fill=Altersgruppe)) %>%
    `+`(ggstream::geom_stream(type="ridge")) %>% print()
  }
  if(type=="deaths"){
    data %>% ggplot(ggplot2::aes(x=date, y=deaths, fill=Altersgruppe)) %>%
    `+`(ggstream::geom_stream(type="ridge")) %>% print()
  }
}
