source("utilities.R")

# HIDDEN FUNKTION
# calc sti for a certain time series with a given population
sti <- function(cases, pop){
  # cases is a vector of daily cases, pop the population of the group

  sti <- stats::filter(cases, rep(1, 7), method="convolution", sides=1)
  sti[1:6] <- cases[1:6]

  return(sti / pop * 1e5)
}

# USER FUNKTION
# returns a total time series of cases and deaths for a region/age group etc.
get_time_series_for <- function(ages="all", regions="Germany",
  from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))

  data <- filter_data_by(ages, regions, from, to)

  # create time series
  days_series <- seq(as.Date(from), as.Date(to), by="days")
  tibble(date=days_series) %>%
    left_join(data, by=c("date"="Meldedatum"))  %>%
    group_by(date) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    # the days for which we have no infection data for are days with 0 infections
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0)) -> time_series

  return(time_series)
}

# HIDDEN funktion
# funktion die schneller laufen sollte, klappt so halb
get_sti_series_simple <- function(lk_id){
  population_lk_data %>% filter(IdLandkreis %in% lk_id) %>%
    unique() -> data
  data %>% `[[`("Landkreis") -> lk_name
  data %>% `[[`("Bevölkerung") -> population

  ts <- get_time_series_for(regions=lk_name)
  cases_ts <- ts[["cases"]]

  sti(cases_ts, population) %>% return()
}

# USER FUNKTION
# returnt eine zeitreihe der sti für eine bestimmte gruppe (lk, alter, etc)
get_sti_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){
  # careful! when specifying region *and* agegroup, the incidence will not be accurate because
  # there is no population data for the age groups in each Landkreis and it will be estimated
  # by the age distribution in all of Germany
  # therefore, it is recommended to specify only one or the other

  ids <- is.numeric(regions)

  # calculate the population of the specified group
  population_age_data %>% filter(Jahr=="2020") -> population_age_2020_data

  population_age_2020_data %>% `[[`("Bevölkerung") %>% sum() -> total_pop
  spec_pop_percentage <- 1

  if(!all(ages=="all")){
    age_labels <- rep("", length(ages))
    for(i in 1:length(ages)){
      age_labels[i] <- get_age_label_from_number(ages[i])
    }
    population_age_2020_data %>%
      filter(Altersgruppe %in% age_labels) %>% `[[`("Bevölkerung") %>% sum() -> spec_pop
    spec_pop_percentage <- spec_pop / total_pop
  }

  time_series <- get_time_series_for(ages, regions, from, to)
  days_series <- days_series <- seq(as.Date(from), as.Date(to), by="days")

  # filter the regions (not robust at the moment)
  rki_data %>% select(Bundesland) %>% unique() %>%
    filter(!Bundesland %in% c("Berlin", "Bremen", "Hamburg")) %>%
    `[[`("Bundesland") %>% tolower() -> bundesländer

  if(all(tolower(regions)=="germany")) final_pop <- spec_pop_percentage * total_pop
  else if(all(tolower(regions) %in% bundesländer)){
    population_age_2020_data %>% filter(tolower(Bundesland) %in% tolower(regions)) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
    final_pop <- region_pop * spec_pop_percentage
  }
  else{
    if(!ids){
      for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions[i])
    }
    population_lk_data %>% filter(IdLandkreis %in% regions) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
    final_pop <- region_pop * spec_pop_percentage
  }

  if(return_deaths) sti_series <- sti(time_series[["deaths"]], final_pop)
  else sti_series <- sti(time_series[["cases"]], final_pop)

  return(tibble(date=days_series, sti=sti_series))
}

library(ggplot2)
plot_for_lks <- function(lks, type="cases"){
  # type can be "cases", "sti", "deaths"
  stopifnot("invalid type!"=type %in% c("cases", "sti", "deaths"))

  ids <- is.numeric(lks)

  lk_ids <- lks
  if(!ids){
    for(i in 1:length(lks)){
      lk_ids[i] <- get_lk_id_from_string(lks[i])
    }
  }

  data <- tibble(date=character(), value=numeric(), lk=character())

  if(type=="sti"){
    for(i in 1:length(lks)){
      get_sti_series_for(regions=lk_ids[i]) -> temp
      temp %>% mutate(date=as.character(date), value=sti, lk=as.character(lks[i])) %>%
        select(-sti) %>% add_row(data, .) -> data
    }
  }

  if(type=="cases"){
    for(i in 1:length(lks)){
      get_time_series_for(regions=lk_ids[i]) -> temp
      temp %>% mutate(date=as.character(date), value=cases, lk=as.character(lks[i])) %>%
        select(-cases, -deaths) %>% add_row(data, .) -> data
    }
  }

  if(type=="deaths"){
    for(i in 1:length(lks)){
      get_time_series_for(regions=lk_ids[i]) -> temp
      temp %>% mutate(date=as.character(date), value=deaths, lk=as.character(lks[i])) %>%
        select(-cases, -deaths) %>% add_row(data, .) -> data
    }
  }
  data %>% mutate(date=as.Date(date)) -> data
  
  if(length(lks==1)){
    data %>% ggplot(aes(x=date, y=value, color=lk, group=1)) %>%
      `+`(geom_line()) %>% return()
  }
  else{
    data %>% ggplot(aes(x=date, y=value, color=lk)) %>%
      `+`(geom_line()) %>% return()
  }
}
