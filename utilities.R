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

  for(i in length(lks)) if(tolower(lks[i]) == tolower(lk_name)){
    if(print_process){
      cat("Because of an exact match, the following was 'Landkreis' was returned:\n")
      print(lk_name)
      cat("If this is wrong, please type the exact 'Landkreis'\n")
    }
    return(lk_ids[i])
  }

  if(print_process){
    cat("The following was 'Landkreis' was returned:\n")
    print(lk_name)
    cat("If this is wrong, please type the exact 'Landkreis'\n")
  }

  return(lk_ids[[1]])
}

# get the right age label from number
get_age_label_from_name <- function(age_name){
  if(as.integer(age_name) < 0) return("A00-A04")
  if(as.integer(age_name) < 5) return("A00-A04")
  if(as.integer(age_name) < 15) return("A05-A14")
  if(as.integer(age_name) < 34) return("A15-A34")
  if(as.integer(age_name) < 59) return("A35-A59")
  if(as.integer(age_name) < 79) return("A60-A79")
  return("A80+")
}

#
get_time_series_for <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # regions can be either Landkreise, Bundesländer or just Germany
  # ages should be a number or a numeric vector (eg c(10, 76, 42))
  data <- rki_data

  # filter the age groups
  if(all(ages != "all")){
    for(i in 1:length(ages)){
      ages[i] <- get_age_label_from_name(ages[i])
    }
    print(ages)
    data %>% filter(Altersgruppe %in% ages) -> data
  }

  # filter the time span
  data %>% filter(Meldedatum >= from, Meldedatum <= to) -> data

  # filter the regions (not robust at the moment)
  rki_data %>% select(Bundesland) %>% unique() %>% `[[`("Bundesland") %>% tolower() -> bundesländer
  if(all(tolower(regions) %in% bundesländer)){
    data %>% filter(tolower(Bundesland) %in% tolower(regions)) -> data
  }
  else if(regions!="Germany"){
    for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions, print_process=T)
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
