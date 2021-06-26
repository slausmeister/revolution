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

# get a 'Landkreis' name
get_lk <- function(lk_name){
  if(str_detect("Germany", regex(lk_name, ignore_case=T))) return("Germany")

  population_lk_data %>% filter(str_detect(Landkreis, regex(lk_name, ignore_case=T))) %>%
     `[[`("Landkreis") -> lks
  if(length(lks) == 1) return(lks)
  if(length(lks) == 0) {
    cat("No match found for:\n")
    print(lk_name)
    cat("Defaulting to Heidelberg <3\n")
    return("Heidelberg")
  }

  cat("For ", lk_name, " the following 'Landkreise' were found\n")
  print(lks)

  for(lk in lks) if(tolower(lk) == tolower(lk_name)){
    cat("We considered, because of exact match:\n")
    print(lk_name)
    cat("If this is wrong, please type the exact 'Landkreis'\n")
    return(lk_name)
  }

  cat("We considered:\n")
  lk_name <- lks[[1]]
  print(lk_name)
  cat("If this is wrong, please type the exact 'Landkreis'\n")
  return(lk_name)
}

# calculate sti for a 'Landkreis' or Germany
calc_sti_lk <- function(lk_name){
  lk_name <- get_lk(lk_name)

  if(lk_name=="Germany") return(calc_sti_germany())

  # pop of the landkreis
  lk_pop <- population_lk_data %>%
    filter(Landkreis==lk_name) %>%
    `[[`("Bevölkerung")


  data <- rki_data  %>%
    filter(Landkreis==lk_name)

  # infections time series
  tibble(date=days_since_2020) %>%
    left_join(data, by=c("date"="Meldedatum"))  %>%
    group_by(date) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
    # the days for which we have no infection data for are days with 0 infections
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
      recoveries=replace_na(recoveries, 0)) %>% `[[`("cases") -> cases
  return(calc_sti(cases, lk_pop))
}

# calc sti for all of Germany
calc_sti_germany <- function(){
  # infections time series
  tibble(date=days_since_2020) %>%
    left_join(rki_data, by=c("date"="Meldedatum"))  %>%
    group_by(date) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
    # the days for which we have no infection data for are days with 0 infections
    mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
      recoveries=replace_na(recoveries, 0)) %>% `[[`("cases") -> cases
  return(calc_sti(cases, total_population_germany))
}

# plot sti for multiple 'Landkreise'
plot_sti_for <- function(lk_names){

  tibble(date=days_since_2020, sti=calc_sti_lk(lk_names[[1]]), Landkreis=lk_names[[1]]) -> df

  for(lk_name in lk_names[-1]){
    df %>% add_row(date=days_since_2020, sti=calc_sti_lk(lk_name), Landkreis=lk_name) -> df
  }
  ggplot(data=df, aes(x=date, y=sti, color=Landkreis)) + geom_line()
}


print(plot_sti_for(c("Passau", "Joghurt City", "Germa")))
