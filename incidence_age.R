source("data_preparation.R")

# libraries for plotting
library(ggplot2)

### sti means 'Sieben Tage Inzidenz'

# calc sti for a certain time series with a given population
calc_sti <- function(cases, pop){
  # cases is a vector of daily cases, pop the population of the group
  n <- length(cases)
  sti <- rep(0, n)
  for(i in 1:n){
    for(j in max(1, i-6):i) sti[i] <- sti[i] + cases[j]
  }
  return(sti / pop * 1e5)
}

# get the right age label
get_age <- function(age_name){
  if(age_name=="total") return("total")
  if(as.integer(age_name) < 0) return("A00-A04")
  if(as.integer(age_name) < 5) return("A00-A04")
  if(as.integer(age_name) < 15) return("A05-A14")
  if(as.integer(age_name) < 34) return("A15-A34")
  if(as.integer(age_name) < 59) return("A35-A59")
  if(as.integer(age_name) < 79) return("A60-A79")
  return("A80+")
}


# calculate sti for a 'Landkreis' or Germany
calc_sti_age <- function(age_name, deaths=F){
  if(age_name=="total") return(calc_sti_germany(deaths))
  age_name <- get_age(age_name)

  # pop of the landkreis
  age_pop <- population_age_2020_data %>%
    filter(Altersgruppe==age_name) %>%
    `[[`("Bevölkerung")

  data <- rki_data  %>%
    filter(Altersgruppe==age_name)

  # infections time series
  tibble(date=days_since_2020) %>%
    left_join(data, by=c("date"="Meldedatum"))  %>%
    group_by(date) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
    # the days for which we have no infection data for are days with 0 infections
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
      recoveries=replace_na(recoveries, 0)) -> df
  if(deaths) df %>% `[[`("deaths") -> cases
  else df %>% `[[`("cases") -> cases
  return(calc_sti(cases, age_pop))
}

# calc sti for all of Germany
calc_sti_germany <- function(deaths=FALSE){
  # infections time series
  tibble(date=days_since_2020) %>%
    left_join(rki_data, by=c("date"="Meldedatum"))  %>%
    group_by(date) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
    # the days for which we have no infection data for are days with 0 infections
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
      recoveries=replace_na(recoveries, 0)) -> df
  if(deaths) df %>% `[[`("deaths") -> cases
  else df %>% `[[`("cases") -> cases
  return(calc_sti(cases, total_population_germany))
}

# plot sti for multiple 'Altersgruppen'
plot_sti_for <- function(age_names, deaths=FALSE){
  tibble(date=days_since_2020, sti=calc_sti_age(age_names[[1]], deaths),
    Altersgruppe=get_age(age_names[[1]])) -> df

  for(age_name in age_names[-1]){
    df %>% add_row(date=days_since_2020, sti=calc_sti_age(age_name, deaths),
      Altersgruppe=get_age(age_name)) -> df
  }

  ggplot(data=df, aes(x=date, y=sti, color=Altersgruppe)) + geom_line()
}

library(ggstream)
# plot infections/deaths by age
plot_total_by_age <- function(deaths=FALSE){
  if(deaths){
    tibble(date=days_since_2020) %>%
      left_join(rki_data, by=c("date"="Meldedatum")) %>% group_by(date, Altersgruppe) %>%
      summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
      mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
       Altersgruppe=replace_na(Altersgruppe, "unbekannt")) %>%
      filter(Altersgruppe!="unbekannt") %>%
      select(date, deaths, Altersgruppe) ->
      deaths_by_age

      return(ggplot(data=deaths_by_age, aes(x=date, y=deaths, fill=Altersgruppe)) +
        geom_stream(type="ridge"))
  }

  tibble(date=days_since_2020) %>%
    left_join(rki_data, by=c("date"="Meldedatum")) %>% group_by(date, Altersgruppe) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
     Altersgruppe=replace_na(Altersgruppe, "unbekannt")) %>%
    filter(Altersgruppe!="unbekannt") %>%
    select(date, cases, Altersgruppe) ->
    cases_by_age

  ggplot(data=cases_by_age, aes(x=date, y=cases, fill=Altersgruppe)) + geom_stream(type="ridge")
}

# plot for the age group with people of age x in it (or the total)
print(plot_sti_for(c(51, 91, "total"), deaths=TRUE))

# ridge plots to show ratios
# print(plot_total_by_age(deaths=T))
