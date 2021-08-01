# ein Skript, das aus allen Kriterien (zB Altersgruppe, Landkreisname, Bundeslandname etc)
# eine Zeitreihe aus Infektionen erstellen kann
# außerdem soll das Skript Benutzereingaben in IDs umwandeln können (zB Schleswig Holstein zu 01)

source("rki_data_preparation.R")
source("pop_data_preparation.R")

get_bundesland_id_from_lk_id <- function(lk_id){
  return((lk_id - lk_id %% 1000) / 1000)
}

# calc sti for a certain time series with a given population
sti <- function(cases, pop){
  # cases is a vector of daily cases, pop the population of the group
  n <- length(cases)
  sti <- rep(0, n)
  for(i in 1:n){
      for(j in max(1, i-6):i) sti[i] <- sti[i] + cases[j]
  }
  return(sti / pop * 1e5)
}

# get the LandkreisID from a input string
get_lk_id_from_string <- function(lk_name, print_process=F){
  population_lk_data %>% filter(str_detect(Landkreis, regex(lk_name, ignore_case=T))) %>%
     `[[`("Landkreis") -> lks

  population_lk_data %>% filter(str_detect(Landkreis, regex(lk_name, ignore_case=T))) %>%
    `[[`("IdLandkreis") -> lk_ids

  if(length(lks) == 1) return(lk_ids)
  if(length(lks) == 0) {
    cat("No match found for:\n")
    print(lk_name)
    cat("Defaulting to Heidelberg\n")
    return(8221)
  }

  if(print_process){
    cat("For ", lk_name, " the following 'Landkreise' were found\n")
    print(lks)
  }

  for(i in 1:length(lks)) if(tolower(lks[i]) == tolower(lk_name)){
    if(print_process){
      cat("Because of an exact match, the following 'Landkreis' was returned:\n")
      print(lk_name)
      cat("If this is wrong, please type the exact 'Landkreis'\n")
    }
    return(lk_ids[i])
  }

  if(print_process){
    cat("The following 'Landkreis' was returned:\n")
    print(lk_name)
    cat("If this is wrong, please type the exact 'Landkreis'\n")
  }

  return(lk_ids[[1]])
}

# get the right age label from number
get_age_label_from_number <- function(age_number){
  if(as.integer(age_number) < 0) return("A00-A04")
  if(as.integer(age_number) < 5) return("A00-A04")
  if(as.integer(age_number) < 15) return("A05-A14")
  if(as.integer(age_number) < 34) return("A15-A34")
  if(as.integer(age_number) < 59) return("A35-A59")
  if(as.integer(age_number) < 79) return("A60-A79")
  return("A80+")
}

#
get_time_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))
  data <- rki_data

  # filter the age groups
  if(!all(ages=="all")){
    for(i in 1:length(ages)){
      ages[i] <- get_age_label_from_number(ages[i])
    }
    data %>% filter(Altersgruppe %in% ages) -> data
  }

  # filter the time span
  data %>% filter(Meldedatum >= from, Meldedatum <= to) -> data

  # filter the regions (not robust at the moment)
  rki_data %>% select(Bundesland) %>% unique() %>% `[[`("Bundesland") %>% tolower() -> bundesländer

  if(all(tolower(regions) %in% bundesländer)){
    data %>% filter(tolower(Bundesland) %in% tolower(regions)) -> data
  }
  else if(!all(tolower(regions)=="germany")){
    for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions[i], print_process=T)
    data %>% filter(IdLandkreis %in% regions) -> data
  }

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

get_sti_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){
  # careful! when specifying region *and* agegroup, the incidence will not be accurate because
  # there is no population data for the age groups in each Landkreis and it will be estimated
  # by the age distribution in all of Germany
  # therefore, it is recommended to specify only one or the other

  # calculate the population of the specified group
  population_age_2020_data %>% `[[`("Bevölkerung") %>% sum() -> total_pop
  spec_pop_percentage <- 1

  if(!all(ages=="all")){
    age_labels <- rep("", length(ages))
    for(i in 1:length(ages)){
      age_labels[i] <- get_age_label_from_number(ages[i])
    }
    print(age_labels)
    population_age_2020_data %>%
      filter(Altersgruppe %in% age_labels) %>% `[[`("Bevölkerung") %>% sum() -> spec_pop
    spec_pop_percentage <- spec_pop / total_pop
  }

  time_series <- get_time_series_for(ages, regions, from, to)

  # filter the regions (not robust at the moment)
  rki_data %>% select(Bundesland) %>% unique() %>% `[[`("Bundesland") %>% tolower() -> bundesländer

  if(all(tolower(regions)=="germany")) final_pop <- spec_pop_percentage * total_pop
  else if(all(tolower(regions) %in% bundesländer)){
    population_age_2020_data %>% filter(tolower(Bundesland) %in% tolower(regions)) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
    final_pop <- region_pop * spec_pop_percentage
  }
  else{
    for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions[i], print_process=T)
    population_lk_data %>% filter(IdLandkreis %in% regions) %>%
      `[[`("Bevölkerung") %>% sum() -> region_pop
    final_pop <- region_pop * spec_pop_percentage
  }

  if(return_deaths) sti_series <- sti(time_series[["deaths"]], final_pop)
  else sti_series <- sti(time_series[["cases"]], final_pop)

  return(tibble(date=days_since_2020, sti=sti_series))
}

# get a sti time series for a lk id
get_sti_series_by_id <- function(lk_ids, ages="all", from="2020-01-01", to=Sys.Date(),
  return_deaths=F){
    # get the lk names
    population_lk_data %>% filter(IdLandkreis %in% lk_ids) %>% select(Landkreis) %>%
      unique() %>% `[[`("Landkreis") -> lk_names
    return(get_sti_series_for(ages=ages, regions=lk_names, from=from, to=to,
      return_deaths=return_deaths))
  }
